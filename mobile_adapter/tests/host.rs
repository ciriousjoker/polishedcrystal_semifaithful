use mobile_lobby_core::{
    group::{Envelope, Owner, MAX_MESSAGE},
    Lobbies,
};
use mobile_protocol::{
    lobby::{self as wire, Error, Hello, Snapshot},
    Frame, MAX_PAYLOAD,
};

#[test]
fn acceptance_workspaces_and_storage_reserve_preserve_user_data() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let temporary = std::env::var_os("MOBILE_TEST_CACHE")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|| "/Volumes/Storage/Cache/mobileadapter".into())
        .join("acceptance-unit-temp");
    std::fs::create_dir_all(&temporary).unwrap();
    let result = std::process::Command::new("python3")
        .args([
            "-m",
            "unittest",
            "discover",
            "-s",
            "mobile_adapter/tests/support",
            "-p",
            "test_runtime.py",
        ])
        .env("TMPDIR", temporary)
        .current_dir(root)
        .output()
        .expect("Run host-only workspace and cleanup checks");
    assert!(
        result.status.success(),
        "Workspace/storage regression: {}{}",
        String::from_utf8_lossy(&result.stdout),
        String::from_utf8_lossy(&result.stderr)
    );
}

fn hello() -> Hello {
    Hello {
        mechanics: 0,
        lobby: 1,
        resume: 0,
        token: [0; 8],
        counterpart: [0x42; 32],
    }
}

#[test]
fn timing_oracle_accepts_existing_native_gaps_and_rejects_new_delay() {
    use mobile_acceptance::contracts::cadence_regression;
    assert!(cadence_regression(&[1, 0, 1, 0, 1], &[1, 0, 1, 0, 1]).is_none());
    assert!(cadence_regression(&[1, 0, 1, 0, 1], &[1, 0, 0, 1, 1]).is_some());
    assert!(cadence_regression(&[1, 0, 1, 0, 1], &[1, 1, 1, 1, 1]).is_none());
    assert!(cadence_regression(&[1, 0, 1], &[1, 2, 1]).is_some());
}

#[test]
fn four_players_and_fifth_capacity_rejection() {
    let mut lobbies = Lobbies::default();
    let ids: Vec<_> = (1..=4)
        .map(|s| lobbies.join(s, hello(), [s as u8; 8], 0).unwrap().player)
        .collect();
    assert_eq!(
        ids.iter().collect::<std::collections::BTreeSet<_>>().len(),
        4
    );
    assert_eq!(lobbies.join(5, hello(), [5; 8], 0), Err(Error::Full));
}

#[test]
fn mixed_speeds_coalesce_presence_without_stale_replay() {
    let mut lobbies = Lobbies::default();
    let peers: Vec<_> = (1..=4)
        .map(|s| lobbies.join(s, hello(), [s as u8; 8], 0).unwrap())
        .collect();
    for tick in 1..=1000u16 {
        for (i, identity) in peers.iter().enumerate() {
            // Network time is common; clients publish at independent rates.
            if tick % (4 - i as u16) != 0 {
                continue;
            }
            lobbies
                .presence(
                    i as u64 + 1,
                    identity.player,
                    identity.generation,
                    tick,
                    Snapshot {
                        epoch: 1,
                        x: tick,
                        status: 1,
                        ..Default::default()
                    },
                    u64::from(tick),
                )
                .unwrap();
        }
    }
    for (i, identity) in peers.iter().enumerate() {
        let interval = 4 - i as u16;
        assert_eq!(
            lobbies.snapshot(i as u64 + 1).unwrap().x,
            1000 / interval * interval
        );
        assert_eq!(
            lobbies.presence(
                i as u64 + 1,
                identity.player,
                identity.generation,
                999,
                Snapshot {
                    epoch: 1,
                    x: 1,
                    ..Default::default()
                },
                1001
            ),
            Err(Error::Stale)
        );
    }
}

#[test]
fn reconnect_requires_token_and_rejects_old_generation() {
    let mut lobbies = Lobbies::default();
    let first = lobbies.join(1, hello(), [7; 8], 0).unwrap();
    lobbies.disconnect(1, 1);
    let mut resume = hello();
    resume.resume = first.player;
    assert_eq!(
        lobbies.join(2, resume.clone(), [9; 8], 2),
        Err(Error::Resume)
    );
    resume.token = first.token;
    let second = lobbies.join(2, resume, [9; 8], 2).unwrap();
    assert_eq!(first.player, second.player);
    assert!(second.generation > first.generation);
    assert_eq!(
        lobbies.presence(2, first.player, first.generation, 1, Snapshot::default(), 3),
        Err(Error::Stale)
    );
}

#[test]
fn all_header_byte_mutations_and_packet_lengths_are_bounded() {
    let encoded = Frame {
        kind: wire::HELLO,
        sequence: 1,
        payload: hello().encode(),
    }
    .encode()
    .unwrap();
    for position in 0..encoded.len() {
        for byte in 0..=255 {
            let mut data = encoded.clone();
            data[position] = byte;
            if let Ok(frame) = Frame::decode(&data) {
                assert!(frame.payload.len() <= MAX_PAYLOAD);
                assert_eq!(frame.encode().unwrap(), data);
            }
        }
    }
    for length in 0..=512 {
        let data: Vec<_> = encoded.iter().copied().cycle().take(length).collect();
        if let Ok(frame) = Frame::decode(&data) {
            assert!(frame.payload.len() <= MAX_PAYLOAD);
        }
    }
}

#[test]
fn every_snapshot_byte_mutation_validates_before_admission() {
    let encoded = Snapshot::default().encode();
    for position in 0..encoded.len() {
        for byte in 0..=255 {
            let mut bytes = encoded.clone();
            bytes[position] = byte;
            if let Ok(snapshot) = Snapshot::decode(&bytes) {
                assert!(snapshot.is_valid());
                assert_eq!(snapshot.encode(), bytes);
            }
        }
    }
}

#[test]
fn deterministic_hostile_event_sequences_preserve_room_invariants() {
    let mut seed = 0xa7613c29u32;
    let mut owner = Owner::new(1).unwrap();
    for tick in 0..20_000u64 {
        let mut next = || {
            seed ^= seed << 13;
            seed ^= seed >> 17;
            seed ^= seed << 5;
            seed
        };
        let stream = (next() % 16 + 1) as u16;
        let group = u64::from(next() % 8 + 1);
        let kind =
            [wire::HELLO, wire::PRESENCE, wire::LEAVE, wire::POLL, 255][(next() % 5) as usize];
        let length = (next() % 122) as usize;
        let payload = if kind == wire::HELLO {
            hello().encode()
        } else {
            (0..length).map(|_| next() as u8).collect()
        };
        let effects = owner.receive(
            group,
            Envelope {
                stream,
                frame: Some(Frame {
                    kind,
                    sequence: tick as u16,
                    payload,
                }),
            },
            [group as u8; 8],
            tick,
        );
        assert!(owner.member_count() <= 4, "seed=a7613c29, tick={tick}");
        assert!(
            owner.checkpoint().validate_checkpoint(),
            "seed=a7613c29, tick={tick}"
        );
        for effect in effects {
            assert!(effect.envelope.encode().unwrap().len() <= MAX_MESSAGE);
        }
        if tick % 17 == 0 {
            owner.disconnect_group(group, tick);
        }
        owner.tick(tick);
    }
}
#[test]
fn work_before_measurement_cannot_mask_a_later_dropped_frame() {
    use mobile_acceptance::contracts::{cadence_regression, cumulative_deltas};
    let native = cumulative_deltas(100, &[101, 102, 103]);
    let mobile = cumulative_deltas(500, &[501, 501, 502]);
    assert_eq!(native, vec![1, 1, 1]);
    assert_eq!(mobile, vec![1, 0, 1]);
    assert!(cadence_regression(&native, &mobile).is_some());
}

#[test]
fn owner_counterpart_checkpoint_restore_and_recreation() {
    fn input(stream: u16, hello: Hello) -> Envelope {
        Envelope {
            stream,
            frame: Some(Frame {
                kind: wire::HELLO,
                sequence: 1,
                payload: hello.encode(),
            }),
        }
    }
    fn result(effects: &[mobile_lobby_core::group::Effect], group: u64, stream: u16) -> &Frame {
        effects
            .iter()
            .find(|e| {
                e.group == group
                    && e.envelope.stream == stream
                    && e.envelope
                        .frame
                        .as_ref()
                        .is_some_and(|f| matches!(f.kind, wire::WELCOME | wire::ERROR))
            })
            .unwrap()
            .envelope
            .frame
            .as_ref()
            .unwrap()
    }
    let mut owner = Owner::new(1).unwrap();
    let first = owner.receive(1, input(1, hello()), [1; 8], 0);
    assert_eq!(result(&first, 1, 1).kind, wire::WELCOME);
    let mut changed = hello();
    changed.counterpart[0] ^= 1;
    let rejected = owner.receive(2, input(1, changed.clone()), [2; 8], 1);
    assert_eq!(
        result(&rejected, 2, 1).payload,
        vec![Error::Incompatible as u8]
    );
    assert_eq!(owner.member_count(), 1);
    let mut restored = owner.checkpoint();
    assert!(restored.validate_checkpoint());
    let rejected = restored.receive(2, input(1, changed.clone()), [2; 8], 2);
    assert_eq!(
        result(&rejected, 2, 1).payload,
        vec![Error::Incompatible as u8]
    );
    for group in 2..=4 {
        let accepted = restored.receive(group, input(1, hello()), [group as u8; 8], 3);
        assert_eq!(result(&accepted, group, 1).kind, wire::WELCOME);
    }
    assert_eq!(restored.member_count(), 4);
    assert!(restored.checkpoint().validate_checkpoint());
    // Room serialization is independent of the Owner's transport tuple-key map.
    let mut room = mobile_lobby_core::Room::new(1).unwrap();
    room.reply(1, input(1, hello()).frame.as_ref().unwrap(), [1; 8], 0)
        .unwrap();
    room.reply(2, input(2, hello()).frame.as_ref().unwrap(), [2; 8], 0)
        .unwrap();
    let bytes = serde_json::to_vec(&room.checkpoint()).unwrap();
    let mut decoded: mobile_lobby_core::Room = serde_json::from_slice(&bytes).unwrap();
    assert!(decoded.validate_checkpoint());
    assert_eq!(
        decoded.reply(
            3,
            input(3, changed.clone()).frame.as_ref().unwrap(),
            [3; 8],
            1
        ),
        Err(Error::Incompatible)
    );
    // Checkpoint corruption cannot erase one retained player's identity constraint.
    let mut mixed = serde_json::to_value(room.checkpoint()).unwrap();
    mixed["model"]["players"]["1"]["counterpart"][0] = serde_json::json!(0);
    let invalid: mobile_lobby_core::Room = serde_json::from_value(mixed).unwrap();
    assert!(!invalid.validate_checkpoint());
    let mut missing = serde_json::to_value(room.checkpoint()).unwrap();
    missing["model"]["players"]["1"]
        .as_object_mut()
        .unwrap()
        .remove("counterpart");
    assert!(serde_json::from_value::<mobile_lobby_core::Room>(missing).is_err());
    let mut recreated = Owner::new(1).unwrap();
    let accepted = recreated.receive(1, input(1, changed), [1; 8], 4);
    assert_eq!(result(&accepted, 1, 1).kind, wire::WELCOME);
}
