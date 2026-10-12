; Persistent lobby protocol. All application multibyte fields are big endian.
; Called with WBK=2, except the explicitly bank-preserving capture hook.
MobileSetFrame:
; A=kind, B=expected response, C=payload length. Return payload pointer in DE.
	ld [wMobileOutgoing + 3], a
	ld a, b
	ld [wMobileExpectedKind], a
	ld a, c
	ld [wMobileOutgoing + 7], a
	add 8
	ld [wMobileOutgoingLength], a
	ld hl, wMobileOutgoing
	ld a, $50
	ld [hli], a
	ld a, $43
	ld [hli], a
	ld a, 1
	ld [hli], a
	inc hl
	ld a, [wMobileSequence + 1]
	ld [hli], a
	ld a, [wMobileSequence]
	ld [hli], a
	xor a
	ld [hl], a
	ld de, wMobileOutgoing + 8
	ret

MobileBuildHello:
	ld a, MOBILE_HELLO
	lb bc, MOBILE_WELCOME, 15
	call MobileSetFrame
	ld hl, MobileHelloPrefix
	ld bc, 5
	rst CopyBytes
	ld hl, wMobilePlayerId
	ld bc, 2
	rst CopyBytes
	ld hl, wMobileResumeToken
	ld bc, 8
	rst CopyBytes
	ret
MobileHelloPrefix:
	db MOBILE_SCHEMA, MOBILE_MECHANICS, MOBILE_CAPACITY, 0, 1 ; public test lobby 1
MobileHelloVector::
	db $50, $43, 1, MOBILE_HELLO, 0, 1, 0, 15
	db MOBILE_SCHEMA, MOBILE_MECHANICS, MOBILE_CAPACITY, 0, 1, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0
MobileHelloVectorEnd::
ASSERT MobileHelloVectorEnd - MobileHelloVector == 23
MobilePollVector:
	db $50, $43, 1, MOBILE_POLL, 0, 0, 0, 0

MobileBuildUpdate:
	ld a, [wMobileWanted]
	and a
	jr z, .leave
	ld a, MOBILE_PRESENCE
	lb bc, MOBILE_ROSTER, 4 + MOBILE_SNAPSHOT_WIRE_SIZE
	call MobileSetFrame
	ld hl, wMobilePlayerId
	ld bc, 4
	rst CopyBytes
	push de
	ld hl, wMobileLocalSnapshot
	ld de, wMobileUploadedSnapshot
	ld bc, MOBILE_SNAPSHOT_SIZE
	rst CopyBytes
	ld a, [wMobileAvailability]
	ld [wMobileUploadedSnapshot + 11], a
	pop de
	ld hl, wMobileUploadedSnapshot
	ld bc, 6 ; map, instance and epoch retain their exact words
	rst CopyBytes
	jmp MobilePackSnapshot

.leave
	ld a, MOBILE_PHASE_LEAVE
	ld [wMobilePhase], a
	ld a, MOBILE_LEAVE
	lb bc, MOBILE_LEFT, 4
	call MobileSetFrame
	ld hl, wMobilePlayerId
	ld bc, 4
	rst CopyBytes
	ret

MobilePresenceChanged:
; Compare against the last upload, including pause/resume availability.
	ld a, [wMobileOutgoing + 3]
	cp MOBILE_PRESENCE
	ret nz
	ld hl, wMobileLocalSnapshot
	ld de, wMobileUploadedSnapshot
	ld b, MOBILE_SNAPSHOT_SIZE
.byte
	ld a, b
	cp 3 ; status byte at snapshot offset 11
	ld a, [hli]
	jr nz, .compare
	ld a, [wMobileAvailability]
.compare
	ld c, a
	ld a, [de]
	inc de
	cp c
	ret nz
	dec b
	jr nz, .byte
	ret

MobilePackSnapshot:
; HL=expanded coordinate fields, DE=wire output. Both coordinates are 12-bit.
	ld a, [hli]
	swap a
	and $f0
	ld b, a
	ld a, [hli]
	ld c, a
	swap a
	and $f
	or b
	ld [de], a
	inc de
	ld a, c
	swap a
	and $f0
	ld b, a
	ld a, [hli]
	and $f
	or b
	ld [de], a
	inc de
	ld a, [hli]
	ld [de], a
	inc de
	ld a, [hli]
	swap a
	and $f0
	ld b, a
	ld a, [hli]
	ld c, a
	inc hl ; avatar follows the packed state byte
	ld a, [hl]
	add a
	or b
	or c
	ld [de], a
	inc de
	dec hl
	ld a, [hl]
	ld [de], a
	ret

MobileExpandRoster:
; Expand backwards through one fixed scratch record. Never overlap unread data.
	ld a, [wMobileApp + 12]
	and a
	ret z
	ld b, a
.loop
	dec b
	push bc
	ld hl, wMobileApp + 13
	ld de, MOBILE_PEER_WIRE_SIZE
	ld a, b
.source
	and a
	jr z, .source_ready
	add hl, de
	dec a
	jr .source
.source_ready
	ld de, wMobileApp + MOBILE_FRAME_SIZE - MOBILE_PEER_WIRE_SIZE
	ld bc, MOBILE_PEER_WIRE_SIZE
	rst CopyBytes
	pop bc
	push bc
	ld hl, wMobileApp + 13
	ld de, MOBILE_PEER_RECORD_SIZE
	ld a, b
.destination
	and a
	jr z, .destination_ready
	add hl, de
	dec a
	jr .destination
.destination_ready
	ld d, h
	ld e, l
	ld hl, wMobileApp + MOBILE_FRAME_SIZE - MOBILE_PEER_WIRE_SIZE
	call MobileExpandPeer
	pop bc
	ld a, b
	and a
	jr nz, .loop
	ret
MobileExpandPeer:
	ld bc, 12 ; identity, revision, map, instance and epoch
	rst CopyBytes
	ld a, [hli]
	ld b, a
	swap a
	and $f
	ld [de], a
	inc de
	ld a, [hli]
	ld c, a
	ld a, b
	swap a
	and $f0
	ld b, a
	ld a, c
	swap a
	and $f
	or b
	ld [de], a
	inc de
	ld a, c
	and $f
	ld [de], a
	inc de
	ld a, [hli]
	ld [de], a
	inc de
	ld a, [hl]
	ld b, a
	swap a
	and $f
	ld [de], a
	inc de
	ld a, b
	and 1
	ld [de], a
	inc de
	inc hl
	ld a, [hl]
	ld [de], a
	inc de
	ld a, b
	and $e
	rrca
	ld [de], a
	ret
MobileNextSequence:
	ld hl, wMobileSequence
	inc [hl]
	ret nz
	inc hl
	inc [hl]
	ret

MobileConsumeFrame:
	ld a, [wMobileAppCount]
	cp 8
	jp c, .incomplete
	ld hl, wMobileApp
	ld de, MobileReplyHeader
	ld b, 3
.header
	ld a, [de]
	inc de
	cp [hl]
	jp nz, .bad
	inc hl
	dec b
	jr nz, .header
	ld a, [hli]
	cp MOBILE_RECEIPT
	jr z, .push_kind
	cp MOBILE_DELTA
	jr z, .push_kind
	cp MOBILE_ROSTER
	jr nz, .reply_kind
.push_kind
	ld a, [wMobileOnline]
	and a
	jp z, .bad
	inc hl
	inc hl
	jr .length
.reply_kind
	cp MOBILE_SERVER_ERROR
	jp z, .error_kind
	ld b, a
	ld a, [wMobileExpectedKind]
	cp b
	jp nz, .bad
.kind
	ld a, [wMobileSequence + 1]
	cp [hl]
	jp nz, .bad
	inc hl
	ld a, [wMobileSequence]
	cp [hl]
	jp nz, .bad
	inc hl
.length
	ld a, [hli]
	and a
	jp nz, .bad
	ld a, [hl]
	cp MOBILE_FRAME_SIZE - 8 + 1
	jp nc, .bad
	add 8
	ld b, a
	ld a, [wMobileAppCount]
	cp b
	jp c, .incomplete
	jp nz, .bad
	ld a, [wMobileApp + 3]
	cp MOBILE_SERVER_ERROR
	jp z, .server_error
	cp MOBILE_WELCOME
	jr z, .welcome
	cp MOBILE_LEFT
	jp z, .left
	cp MOBILE_RECEIPT
	jp z, .receipt
	jp .roster
.welcome
	ld a, [wMobileApp + 7]
	cp 15
	jp nz, .bad
	ld a, [wMobileApp + 22]
	cp MOBILE_CAPACITY
	jp nz, .bad
	ld a, [wMobileApp + 8]
	ld b, a
	ld a, [wMobileApp + 9]
	or b
	jp z, .bad
	ld a, [wMobileApp + 10]
	ld b, a
	ld a, [wMobileApp + 11]
	or b
	jp z, .bad
	ld a, [wMobileApp + 12]
	and a
	jp nz, .bad
	ld a, [wMobileApp + 13]
	cp 1
	jp nz, .bad
	ld hl, wMobileApp + 8
	ld de, wMobilePlayerId
	ld bc, 4
	rst CopyBytes
	ld hl, wMobileApp + 14
	ld de, wMobileResumeToken
	ld bc, 8
	rst CopyBytes
	ld a, 1
	ld [wMobileOnline], a
	ld [wMobilePhase], a
	ld hl, wMobileSuccessCount
	inc [hl]
	xor a
	ld [wMobilePingSent], a
	call MobileNextSequence
	jp .accepted
.receipt
; Local transfer receipts only acknowledge a known identity. They cannot alter
; online state, membership, local game data or the latest remote positions.
	ld a, [wMobileApp + 7]
	cp 4
	jp nz, .bad
	call .check_identity
	jp c, .bad
	jp .accepted
.roster
	call .check_identity
	jp c, .bad
	ld a, [wMobileApp + 12]
	cp MOBILE_PEERS + 1
	jp nc, .bad
	ld b, a
	ld c, 5
	and a
	jr z, .size
.sum
	ld a, c
	add MOBILE_PEER_WIRE_SIZE
	ld c, a
	dec b
	jr nz, .sum
.size
	ld a, [wMobileApp + 7]
	cp c
	jp nz, .bad
	ld a, [wMobileDecodePhase]
	and a
	jr nz, .validate_roster
	call MobileExpandRoster
	ld a, 1
	ld [wMobileDecodePhase], a
; A single changed peer has a small fixed validation/copy bound. Keep the
; three-peer path staged, but avoid two extra frame waits for this common case.
	ld a, [wMobileApp + 12]
	cp 2
	jp nc, .incomplete
	call MobileValidateRoster
	jp c, .bad
	jr .publish_roster
.validate_roster
	cp 2
	jr z, .publish_roster
	call MobileValidateRoster
	jp c, .bad
	ld a, 2
	ld [wMobileDecodePhase], a
	jp .incomplete
.publish_roster
	ld a, [wMobileApp + 3]
	cp MOBILE_DELTA
	jp z, MobileMergePeers
	ld a, [wMobileApp + 12]
	ld [wMobilePeerCount], a
	and a
	jr z, .roster_done
	ld b, a
	ld hl, wMobileApp + 13
	ld de, wMobilePeer1
.copy_peer
	push bc
	call MobileInvalidatePeer
	ld bc, MOBILE_PEER_RECORD_SIZE
	rst CopyBytes
	push hl
	ld hl, MOBILE_PEER_SIZE - MOBILE_PEER_RECORD_SIZE
	add hl, de
	ld d, h
	ld e, l
	pop hl
	pop bc
	dec b
	jr nz, .copy_peer
.roster_done
	ld hl, wMobilePresenceCount
	inc [hl]
	jr nz, .accepted
	inc hl
	inc [hl]
.accepted
	xor a
	ld [wMobileDecodePhase], a
	ld [wMobileAppCount], a
.incomplete
	and a
	ret
.left
	ld a, [wMobileApp + 7]
	cp 4
	jr nz, .bad
	call .check_identity
	jr c, .bad
	xor a
	ld [wMobilePlayerId], a
	ld [wMobilePlayerId + 1], a
	ld [wMobileOnline], a
	ld [wMobilePeerCount], a
	ld a, 5
	ld [wMobileStep], a
	and a
	ret
.check_identity
	ld hl, wMobileApp + 8
	ld de, wMobilePlayerId
	ld b, 4
.identity
	ld a, [de]
	inc de
	cp [hl]
	jr nz, .bad
	inc hl
	dec b
	jr nz, .identity
	and a
	ret
.server_error
	ld a, [wMobileApp + 7]
	cp 1
	jr nz, .bad
	ld a, [wMobileApp + 8]
	ld [wMobileServerError], a
	cp 5 ; a restarted server may have forgotten the resume identity
	jr nz, .stop
	xor a
	ld [wMobilePlayerId], a
	ld [wMobilePlayerId + 1], a
	jr .bad
.stop
	xor a
	ld [wMobileWanted], a
.bad
	scf
	ret
.error_kind
	inc hl
	inc hl
	jp .length

MobileValidateRoster:
; Validate every entry and all identity bindings before publishing any bytes.
	ld a, [wMobileApp + 12]
	ld b, a
	ld c, 0
	ld hl, wMobileApp + 13
.loop
	ld a, b
	and a
	ret z
	push bc
	push hl
	call MobileValidatePeer
	jr c, .bad
	ld a, [wMobileApp + 3]
	cp MOBILE_DELTA
	jr nz, .duplicates
	call MobileFindPeer
	jr c, .bad
.duplicates
	pop hl
	push hl
	ld d, h
	ld e, l
	ld hl, wMobileApp + 13
	pop bc ; current record pointer, retain the loop counters below it
	push bc
.compare
	ld a, h
	cp b
	jr nz, .identity
	ld a, l
	cp c
	jr z, .next
.identity
	ld a, [de]
	cp [hl]
	jr nz, .advance
	inc de
	inc hl
	ld a, [de]
	cp [hl]
	dec de
	dec hl
	jr z, .bad
.advance
	push bc
	ld bc, MOBILE_PEER_RECORD_SIZE
	add hl, bc
	pop bc
	jr .compare
.next
	pop hl
	ld bc, MOBILE_PEER_RECORD_SIZE
	add hl, bc
	pop bc
	dec b
	inc c
	jr .loop
.bad
	pop hl
	pop bc
	scf
	ret

MobileFindPeer:
; HL validated record -> DE known private peer, carry if no binding. HL preserved.
	push hl
	push bc
	ld a, [wMobilePeerCount]
	ld b, a
	ld de, wMobilePeer1
.loop
	ld a, b
	and a
	jr z, .missing
	ld a, [de]
	cp [hl]
	jr nz, .next
	inc de
	inc hl
	ld a, [de]
	cp [hl]
	dec de
	dec hl
	jr z, .found
.next
	push hl
	ld hl, MOBILE_PEER_SIZE
	add hl, de
	ld d, h
	ld e, l
	pop hl
	dec b
	jr .loop
.found
	pop bc
	pop hl
	and a
	ret
.missing
	pop bc
	pop hl
	scf
	ret

MobileMergePeers:
; All identity bindings were checked before publication. No partial delta commits.
	ld a, [wMobileApp + 12]
	and a
	jr z, .bad
	ld b, a
	ld hl, wMobileApp + 13
.loop
	push bc
	call MobileFindPeer
; Prioritize one already validated changed record without increasing frame work.
; Private records have a 64-byte stride; subtraction remains valid across pages.
	ld a, e
	sub LOW(wMobilePeer1)
rept 6
	rrca
endr
	inc a
	ld [wMobileBudgetPriority], a
	call MobileInvalidatePeer
	ld bc, MOBILE_PEER_RECORD_SIZE
	rst CopyBytes
	pop bc
	dec b
	jr nz, .loop
	jmp MobileConsumeFrame.roster_done
.bad
	scf
	ret

MobileAppendLobbyData:
; Adapter TCP reads may contain partial or several application frames. Parse
; each complete bounded frame before appending the next, without a stream queue.
	ld a, [wMobileRx + 5]
	and a
	jp z, MobileAppendPending.bad
	dec a
	ld [wMobileAppPending], a
	xor a
	ld [wMobileRxConsumed], a
	ld a, [wMobileRx + 6]
	ld hl, wMobileConnection
	cp [hl]
	jp nz, MobileAppendPending.bad
	ld a, MOBILE_WIRE_APP
	ld [wMobileWire], a
	; The adapter checksum was verified in this pump. Start application parsing
	; in the next pump so a full TCP read cannot combine both worst-case paths.
	and a
	ret
MobileAppendPending:
; Copy only to the next validated frame boundary. Header/body chunks replace
; repeated per-byte pointer construction, without enlarging either buffer.
	ld c, 16 ; bounded copy slice leaves idle time for presentation between phases
.target
	ld a, [wMobileAppCount]
	and a
	jr nz, .partial
	ld [wMobileDecodePhase], a
.partial
	cp 8
	ld b, 8
	jr c, .size
	ld a, [wMobileApp + 6]
	and a
	jp nz, .bad
	ld a, [wMobileApp + 7]
	cp MOBILE_FRAME_SIZE - 8 + 1
	jp nc, .bad
	add 8
	ld b, a
.size
	ld a, [wMobileAppCount]
	cp b
	jr z, .complete
	jp nc, .bad
	ld e, a
	ld a, b
	sub e
	ld b, a ; bytes remaining to the header or complete frame boundary
	ld a, [wMobileAppPending]
	and a
	jr z, .finished
	cp b
	jr nc, .budget
	ld b, a
.budget
	ld a, c
	and a
	ret z
	cp b
	jr nc, .copy
	ld b, a
.copy
	push bc ; B=chunk, C=remaining pump budget
	ld a, [wMobileRxConsumed]
	ld e, a
	ld d, 0
	ld hl, wMobileRx + 7
	add hl, de
	push hl
	ld a, [wMobileAppCount]
	ld e, a
	ld hl, wMobileApp
	add hl, de
	ld d, h
	ld e, l
	pop hl
	ld c, b
	ld b, 0
	rst CopyBytes
	pop bc
	ld a, [wMobileAppCount]
	add b
	ld [wMobileAppCount], a
	ld a, [wMobileRxConsumed]
	add b
	ld [wMobileRxConsumed], a
	ld a, [wMobileAppPending]
	sub b
	ld [wMobileAppPending], a
	ld a, c
	sub b
	ld c, a
	jr .target
.complete
	call MobileConsumeFrame
	ret c
	ld a, [wMobileAppCount]
	and a
	ret nz ; unfinished validation keeps the bounded application frame
	ld a, [wMobileAppPending]
	and a
	ret nz
.finished
	ld a, MOBILE_WIRE_DONE
	ld [wMobileWire], a
	and a
	ret
.bad
	scf
	ret

MobileValidatePeer:
; Validate one complete wire record before publishing it. HL is preserved.
	push hl
	ld a, [hli]
	ld b, a
	ld a, [hl]
	or b
	jr z, .bad
	ld a, [wMobilePlayerId]
	cp b
	jr nz, .generation
	ld a, [wMobilePlayerId + 1]
	cp [hl]
	jr z, .bad
.generation
	inc hl
	ld a, [hli]
	ld b, a
	ld a, [hl]
	or b
	jr z, .bad
	ld de, 9 ; record offset 12: world X high byte
	add hl, de
	ld a, [hli]
	cp 16
	jr nc, .bad
	inc hl
	ld a, [hli]
	cp 16
	jr nc, .bad
	inc hl
	ld a, [hli]
	cp 16
	jr nc, .bad
	ld a, [hli]
	cp 2
	jr nc, .bad
	ld a, [hli]
	cp NUM_PLAYER_GENDERS
	jr nc, .bad
	ld a, [hl]
	cp 6
	jr nc, .bad
	pop hl
	and a
	ret
.bad
	pop hl
	scf
	ret

MobileMapEntered::
	call MobileNativeInvalidateHash
	call MobileNativeResetLeases
	xor a
	ld [wMobileBudgetCursor], a
	ld [wMobileBudgetPriority], a
	ld [wMobileVisibleCount], a
	ld [wMobileRenderedCount], a
	ldh a, [rWBK]
	push af
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld hl, wMobilePeer1 + 33
	ld b, MOBILE_PEERS
.loop
	xor a
	ld [hli], a
	ld [hli], a
	ld [hl], a
	push bc
	ld bc, MOBILE_PEER_SIZE - 2
	add hl, bc
	pop bc
	dec b
	jr nz, .loop
	pop af
	ldh [rWBK], a
	xor a
	ld [wMobileLocalValid], a
	ld [wMobileGraphicsReady], a
	ret

MobileCapture::
; One coherent snapshot after the player's object step. Preserve WBK.
	ld a, [wMobileOwner]
	and a
	ret z
	ld a, [wMobileMode]
	and a
	ret z
	ldh a, [rWBK]
	push af
	ld a, 1
	ldh [rWBK], a
	ldh a, [hInMenu]
	and a
	jr nz, .paused
	call MobileSceneIsSafe
	jr nz, .paused
	ld a, [wBattleMode]
	and a
	jr z, .safe
.paused
	call MobileFrozenOverworld
	jr z, .frozen ; ordinary Start menu retains its stationary world graphics
	call MobileNativeInvalidateHash
	xor a
	ld [wMobileLocalValid], a
; Native menu/map graphics may overwrite private pose windows. Force a complete
; validated reload when returning, without touching any ordinary game record.
	ld [wMobileVirtualWindowOwners], a
	ld [wMobileVirtualWindowOwners + 1], a
	ld [wMobileVirtualWindowOwners + 2], a
.frozen
	pop af
	ldh [rWBK], a
	ret
.safe
	ld a, [wMobileLocalValid]
	and a
	jr nz, .epoch
	call MobileNativeInstanceSlice
	jr c, .fingerprint_ready
	pop af
	ldh [rWBK], a
	ret
.fingerprint_ready
	ld hl, wMobileMapEpoch
	inc [hl]
	jr nz, .valid
	inc hl
	inc [hl]
.valid
	ld a, 1
	ld [wMobileLocalValid], a
.epoch
	ld de, wMobileLocalStaging
	ld a, [wMapGroup]
	ld [de], a
	inc de
	ld a, [wMapNumber]
	ld [de], a
	inc de
	ld a, [wMobileInstanceKey]
	ld [de], a
	inc de
	ld a, [wMobileInstanceKey + 1]
	ld [de], a
	inc de
	ld a, [wMobileMapEpoch + 1]
	ld [de], a
	inc de
	ld a, [wMobileMapEpoch]
	ld [de], a
	inc de
	push de
	call MobileStepProgress
	ld b, 64
	ld c, d
	ld a, [wXCoord]
	call MobileWorldPixel
	call MobileClampSnapshotPixel
	pop de
	ld a, h
	ld [de], a
	inc de
	ld a, l
	ld [de], a
	inc de
	push de
	call MobileStepProgress
	ld a, [wPlayerSpriteYOffset]
	add e
	ld c, a
	ld b, 64
	ld a, [wYCoord]
	call MobileWorldPixel
	call MobileClampSnapshotPixel
	pop de
	ld a, h
	ld [de], a
	inc de
	ld a, l
	ld [de], a
	inc de
	ld a, [wPlayerFacing]
	cp 16
	jr c, .facing
	ld a, [wPlayerDirection]
	and $c
.facing
	ld [de], a
	inc de
	ld a, 1
	ld [de], a
	inc de
	ld a, [wPlayerGender]
	ld [de], a
	inc de
	ld a, [wPlayerState]
	cp 6
	jr c, .movement
	ld a, 5
.movement
	ld [de], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld hl, wMobileLocalStaging
	ld de, wMobileLocalSnapshot
	ld bc, MOBILE_SNAPSHOT_SIZE
	rst CopyBytes
	pop af
	ldh [rWBK], a
	ret

MobileInvalidatePeer:
; HL=new wire record, DE=old peer. Invalidate interpolation on identity/warp.
	push hl
	push de
	push bc
	ld b, 4 ; player and generation
.identity
	ld a, [de]
	cp [hl]
	jr nz, .changed
	inc de
	inc hl
	dec b
	jr nz, .identity
	ld bc, 6 ; entry epoch at record offset 10
	add hl, bc
	ld a, e
	add 6
	ld e, a
	jr nc, .epoch
	inc d
.epoch
	ld a, [de]
	cp [hl]
	jr nz, .changed
	inc de
	inc hl
	ld a, [de]
	cp [hl]
	jr z, .same
.changed
	pop bc
	pop de
	push de
	ld hl, MOBILE_PEER_DRAW_VALID
	add hl, de
	ld [hl], 0
	jr .done
.same
	pop bc
.done
	pop de
	pop hl
	ret

MobileComputeInstance:
; Hash current interior block layout, excluding connection padding. WRAM0 is
; live here; never hash its menu/battle UNION while a script or menu is active.
	ld a, [wMapWidth]
	ld e, a
	add 6
	ld c, a
	ld b, 0
	ld hl, wOverworldMapBlocks + 3
	add hl, bc
	add hl, bc
	add hl, bc
	ld a, [wMapHeight]
	ld d, a
	ld a, [wMapTileset]
	ld c, a
	ld b, a
.row
	push de
.column
	ld a, [hli]
	add c
	ld c, a
	add b
	ld b, a
	dec e
	jr nz, .column
	pop de
	push bc
	ld bc, 6
	add hl, bc
	pop bc
	dec d
	jr nz, .row
	ld a, b
	ld [wMobileInstanceKey], a
	ld a, c
	ld [wMobileInstanceKey + 1], a
	ret

MobileWorldPixel:
; A=camera tile coordinate, B=object screen pixel, C=signed pending scroll.
; Return absolute world pixel in HL, independent of step duration and speed.
	ld l, a
	ld h, 0
	ld de, -4
	add hl, de
rept 4
	add hl, hl
endr
	ld e, b
	ld d, 0
	add hl, de
	ld e, c
	ld d, 0
	bit 7, c
	jr z, .offset
	dec d
.offset
	add hl, de
	ret

MobileClampSnapshotPixel:
; During an outward border step the old map's fractional coordinate can be
; negative before its connection loads. Keep that last snapshot in range.
	bit 7, h
	ret z
	ld hl, 0
	ret

MobileSceneIsSafe:
; Ordinary player-controlled turns use a short vanilla script. Keep their
; presence live without invalidating map state or reloading social graphics.
	ld a, [wPanningAroundTinyMap]
	and a
	ret nz
	ld a, [wScriptRunning]
	and a
	ret z
	cp PLAYEREVENT_JOYCHANGEFACING
	ret

MobileFrozenOverworld:
; Z only for the ordinary paused overworld, never a bag, battle, camera or map
; transition. Projection/upload remain frozen; accepted network state is private.
	ldh a, [hInMenu]
	and a
	ret nz
	ld a, [wMobileMenuState]
	and a
	ret nz
	ld a, [wPanningAroundTinyMap]
	and a
	ret nz
	ld a, [wBattleMode]
	and a
	ret nz
	ld a, [wMapStatus]
	cp MAPSTATUS_HANDLE
	ret nz
	ld a, [wScriptRunning]
	cp $ff
	ret

MobileStepProgress:
; Camera movement cancels OBJECT_SPRITE_X/Y before logical tile coordinates
; advance. Recover the actual fractional tile from the player's step timer.
; Return signed pixel displacement D=x, E=y, including normal/run/bike/slow.
	ld de, 0
	ld a, [wPlayerWalking]
	cp STANDING
	ret z
	ld b, a
	and $c
	jr z, .slow
	cp STEP_WALK << 2
	jr z, .walk
	cp STEP_BIKE << 2
	jr z, .bike
	ld a, 8 ; run, two pixels per update
	ld hl, wPlayerStepDuration
	sub [hl]
	add a
	jr .direction
.bike
	ld a, 4
	ld hl, wPlayerStepDuration
	sub [hl]
	add a
	add a
	jr .direction
.walk
	ld a, 16
	ld hl, wPlayerStepDuration
	sub [hl]
	jr .direction
.slow
	ld a, 32
	ld hl, wPlayerStepDuration
	sub [hl]
	srl a
.direction
	ld c, a
	ld a, b
	and 3
	jr z, .down
	cp 1
	jr z, .up
	cp 2
	jr z, .left
	ld d, c
	ret
.left
	xor a
	sub c
	ld d, a
	ret
.up
	xor a
	sub c
	ld e, a
	ret
.down
	ld e, c
	ret
