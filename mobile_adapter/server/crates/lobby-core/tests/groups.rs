use mobile_lobby_core::group::{Envelope, Owner, MAX_MESSAGE};
use mobile_protocol::{lobby as wire, Frame};

fn hello(stream: u16) -> Envelope {
    Envelope {
        stream,
        frame: Some(Frame {
            kind: wire::HELLO,
            sequence: 1,
            payload: wire::Hello {
                mechanics: 0,
                lobby: 1,
                resume: 0,
                token: [0; 8],
            }
            .encode(),
        }),
    }
}

#[test]
fn household_streams_have_distinct_players_and_faults_are_scoped() {
    let mut room = Owner::new(1).unwrap();
    let mut ids = Vec::new();
    for i in 0..wire::CAPACITY {
        let group = u64::from(i / 2 + 1);
        let stream = u16::from(i % 2 + 1);
        let effects = room.receive(group, hello(stream), [i + 1; 8], 0);
        let welcome = effects
            .iter()
            .find_map(|e| {
                e.envelope
                    .frame
                    .as_ref()
                    .filter(|f| f.kind == wire::WELCOME)
            })
            .unwrap();
        ids.push(wire::Welcome::decode(&welcome.payload).unwrap().player);
    }
    ids.sort();
    ids.dedup();
    assert_eq!(ids.len(), usize::from(wire::CAPACITY));
    assert_eq!(room.member_count(), usize::from(wire::CAPACITY));
    let full = room.receive(9, hello(1), [9; 8], 0);
    assert_eq!(
        full[0].envelope.frame.as_ref().unwrap().payload,
        [wire::Error::Full as u8]
    );
    let bad = Envelope {
        stream: 1,
        frame: Some(Frame {
            kind: wire::PRESENCE,
            sequence: 2,
            payload: vec![],
        }),
    };
    let effects = room.receive(1, bad, [0; 8], 1);
    assert_eq!(effects[0].group, 1);
    assert_eq!(room.member_count(), usize::from(wire::CAPACITY) - 1);
    assert!(room.checkpoint().validate_checkpoint());
    room.disconnect_group(1, 2);
    assert_eq!(room.member_count(), usize::from(wire::CAPACITY) - 2);
}

#[test]
fn arbitrary_envelopes_are_bounded_and_roundtrip_at_every_byte_boundary() {
    let encoded = hello(16).encode().unwrap();
    assert_eq!(Envelope::decode(&encoded).unwrap(), hello(16));
    for length in 0..encoded.len() {
        assert!(Envelope::decode(&encoded[..length]).is_err());
    }
    for index in [0, 1, 2, 3, 4, 5] {
        let mut bytes = encoded.clone();
        bytes[index] = 255;
        assert!(Envelope::decode(&bytes).is_err());
    }
    let mut seed = 0x41a178abu32;
    for length in 0..=MAX_MESSAGE + 1 {
        for _ in 0..128 {
            let bytes: Vec<_> = (0..length)
                .map(|_| {
                    seed ^= seed << 13;
                    seed ^= seed >> 17;
                    seed ^= seed << 5;
                    seed as u8
                })
                .collect();
            if let Ok(envelope) = Envelope::decode(&bytes) {
                assert_eq!(envelope.encode().unwrap(), bytes);
            }
        }
    }
}

fn presence(stream: u16, identity: &wire::Welcome, sequence: u16, x: u16) -> Envelope {
    let mut payload = identity.player.to_be_bytes().to_vec();
    payload.extend(identity.generation.to_be_bytes());
    payload.extend(
        wire::Snapshot {
            x,
            epoch: 1,
            ..Default::default()
        }
        .encode(),
    );
    Envelope {
        stream,
        frame: Some(Frame {
            kind: wire::PRESENCE,
            sequence,
            payload,
        }),
    }
}

#[test]
fn bounded_pushes_keep_latest_state_through_churn() {
    let mut room = Owner::new(1).unwrap();
    let first = room.receive(1, hello(1), [1; 8], 0);
    let source = wire::Welcome::decode(&first[0].envelope.frame.as_ref().unwrap().payload).unwrap();
    let second = room.receive(1, hello(2), [2; 8], 0);
    let receiver = wire::Welcome::decode(
        &second
            .iter()
            .find_map(|e| {
                e.envelope
                    .frame
                    .as_ref()
                    .filter(|f| f.kind == wire::WELCOME)
            })
            .unwrap()
            .payload,
    )
    .unwrap();
    let mut latest = None;
    for sequence in 1..=2000 {
        let now = u64::from(sequence) * 50;
        let mut effects = room.receive(1, presence(1, &source, sequence, sequence), [0; 8], now);
        effects.extend(room.receive(1, presence(2, &receiver, sequence, 0), [0; 8], now));
        assert!(effects.len() <= 2 * usize::from(wire::CAPACITY) + 2);
        for effect in effects {
            if effect.envelope.stream == 2 {
                if let Some(frame) = effect.envelope.frame.filter(|f| f.kind == wire::ROSTER) {
                    latest = Some(
                        wire::Peer::decode(&frame.payload[5..5 + wire::PEER_LEN])
                            .unwrap()
                            .snapshot
                            .x,
                    );
                }
            }
        }
    }
    assert_eq!(latest, Some(2000));
    assert!(room
        .tick(104_001)
        .iter()
        .all(|e| e.envelope.frame.is_none()));
    assert_eq!(room.member_count(), 0);
}

#[test]
fn a_fast_source_cannot_exceed_push_rate_and_the_pending_latest_state_flushes() {
    let mut owner = Owner::new(1).unwrap();
    let effects = owner.receive(1, hello(1), [1; 8], 0);
    let identity =
        wire::Welcome::decode(&effects[0].envelope.frame.as_ref().unwrap().payload).unwrap();
    owner.receive(2, hello(1), [2; 8], 0);
    // Establish the receiver's first snapshot and then send one update per ms.
    owner.receive(1, presence(1, &identity, 1, 1), [0; 8], 50);
    let mut delivered = Vec::new();
    for now in 51..=199u64 {
        for effect in owner.receive(
            1,
            presence(1, &identity, now as u16, now as u16),
            [0; 8],
            now,
        ) {
            if effect.group == 2 {
                if let Some(frame) = effect.envelope.frame.filter(|f| f.kind == wire::ROSTER) {
                    delivered.push((
                        now,
                        wire::Peer::decode(&frame.payload[5..5 + wire::PEER_LEN])
                            .unwrap()
                            .snapshot
                            .x,
                    ));
                }
            }
        }
    }
    assert_eq!(delivered.len(), 2);
    assert_eq!(delivered[1].0 - delivered[0].0, 50);
    assert_eq!(owner.flush_after(199), Some(1));
    assert!(owner.tick(199).is_empty());
    let effects = owner.tick(200);
    let frame = effects
        .iter()
        .find(|e| e.group == 2)
        .unwrap()
        .envelope
        .frame
        .as_ref()
        .unwrap();
    assert_eq!(
        wire::Peer::decode(&frame.payload[5..5 + wire::PEER_LEN])
            .unwrap()
            .snapshot
            .x,
        199
    );
    assert_eq!(owner.flush_after(200), None);
    assert!(owner.tick(201).is_empty());
}

#[test]
fn arbitrary_state_sequences_keep_valid_bounded_effects_and_checkpoints() {
    let mut owner = Owner::new(1).unwrap();
    let mut seed = 0x7829b6d3u32;
    for step in 0..20_000u64 {
        seed ^= seed << 13;
        seed ^= seed >> 17;
        seed ^= seed << 5;
        let group = u64::from(seed % 8 + 1);
        let stream = ((seed >> 8) % 16 + 1) as u16;
        let envelope = match step % 7 {
            0 => hello(stream),
            1 => Envelope {
                stream,
                frame: None,
            },
            2 => Envelope {
                stream,
                frame: Some(Frame {
                    kind: wire::POLL,
                    sequence: 0,
                    payload: vec![],
                }),
            },
            _ => Envelope {
                stream,
                frame: Some(Frame {
                    kind: (seed >> 16) as u8,
                    sequence: seed as u16,
                    payload: (0..seed as usize % 129)
                        .map(|i| seed.wrapping_add(i as u32) as u8)
                        .collect(),
                }),
            },
        };
        let effects = owner.receive(
            group,
            envelope,
            seed.to_be_bytes().repeat(2).try_into().unwrap(),
            step,
        );
        assert!(effects.len() <= 2 * usize::from(wire::CAPACITY) + 2);
        for effect in effects {
            let bytes = effect.envelope.encode().unwrap();
            assert_eq!(Envelope::decode(&bytes).unwrap(), effect.envelope);
            assert!(bytes.len() <= MAX_MESSAGE);
        }
        assert!(owner.member_count() <= usize::from(wire::CAPACITY));
        assert!(
            owner.checkpoint().validate_checkpoint(),
            "State corruption after step {step}"
        );
    }
    owner.tick(50_000);
    assert_eq!(owner.member_count(), 0);
    assert!(owner.checkpoint().validate_checkpoint());
}

#[test]
fn moving_receivers_get_bounded_state_pushes_without_polls() {
    let mut owner = Owner::new(1).unwrap();
    let mut identities = Vec::new();
    for stream in 1..=2 {
        let effects = owner.receive(1, hello(stream), [stream as u8; 8], 0);
        let welcome = effects
            .iter()
            .filter_map(|e| e.envelope.frame.as_ref())
            .find(|f| f.kind == wire::WELCOME)
            .unwrap();
        identities.push(wire::Welcome::decode(&welcome.payload).unwrap());
    }
    for sequence in 1..=20 {
        let mut delivered = None;
        for stream in 1..=2 {
            let identity = &identities[stream - 1];
            let mut payload = identity.player.to_be_bytes().to_vec();
            payload.extend(identity.generation.to_be_bytes());
            payload.extend(
                wire::Snapshot {
                    x: sequence,
                    epoch: 1,
                    ..Default::default()
                }
                .encode(),
            );
            let effects = owner.receive(
                1,
                Envelope {
                    stream: stream as u16,
                    frame: Some(Frame {
                        kind: wire::PRESENCE,
                        sequence,
                        payload,
                    }),
                },
                [0; 8],
                u64::from(sequence) * 50,
            );
            for effect in effects {
                if effect.envelope.stream == 2 {
                    if let Some(frame) = effect.envelope.frame {
                        if frame.kind == wire::ROSTER {
                            delivered = Some(frame);
                        }
                    }
                }
            }
        }
        let frame =
            delivered.expect("A moving receiver must receive fresh bounded pushes without polls");
        let first = wire::Peer::decode(&frame.payload[5..5 + wire::PEER_LEN]).unwrap();
        assert_eq!(first.player, identities[0].player);
        assert_eq!(first.snapshot.x, sequence);
    }
}

#[test]
fn embedded_flush_uses_identical_rules_without_cloud_cadence_delay() {
    let mut owner = Owner::new(1).unwrap();
    let effects = owner.receive(1, hello(1), [1; 8], 0);
    let identity =
        wire::Welcome::decode(&effects[0].envelope.frame.as_ref().unwrap().payload).unwrap();
    owner.receive(2, hello(1), [2; 8], 0);
    owner.receive(1, presence(1, &identity, 1, 1), [0; 8], 50);
    let effects = owner.receive(1, presence(1, &identity, 2, 9), [0; 8], 51);
    assert!(!effects.iter().any(|e| e.group == 2));
    let effects = owner.tick_local(51);
    assert!(effects.len() <= usize::from(wire::CAPACITY));
    let received = effects.iter().find(|e| e.group == 2).unwrap();
    let frame = received.envelope.frame.as_ref().unwrap();
    assert_eq!(frame.kind, wire::ROSTER);
    let peer = wire::Peer::decode(&frame.payload[5..5 + wire::PEER_LEN]).unwrap();
    assert_eq!(peer.player, identity.player);
    assert_eq!(peer.snapshot.x, 9);
    assert!(owner.tick_local(51).is_empty());
    assert_eq!(owner.member_count(), 2);
    assert!(owner.checkpoint().validate_checkpoint());
}
