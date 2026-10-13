; Exact resource plans for the saturated aligned canonical scene only. Plans
; contain actor IDs and palette ownership, never RGB values, poses or OAM bytes.
SECTION "Mobile Admission Plans", ROMX, BANK[MOBILE_ROM_BANK + 5]
MobilePlanStart::

MobilePlanPrepareAndAudit::
	ld a, [wMobileVirtualNeedColorAudit]
	bit 7, a
	jr z, .audit_ready
	call MobileVirtualCurrentColors
	ldh a, [hUsedOAMIndex]
	cp OAM_SIZE
	jr nz, .audit_ready
; The completed color audit survives an aborted frame. Do not compound its
; cold cost with proof construction when the remaining idle window is short.
	ldh a, [rLY]
	cp 124
	jr c, .audit_ready
	scf
	ret
.audit_ready
MobilePlanPrepare::
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jp nz, .ordinary
	ldh a, [hUsedOAMIndex]
	cp OAM_SIZE
	jp nz, .ordinary
	ld a, [wMobileVirtualForceFull]
	and a
	jr z, .first_proof
	ld a, [wMobileVirtualPlanSignatureValid]
	cp 1
	jr nz, .first_proof
; A cache hit validates the complete current native view while emitting fresh
; pixels. A mismatch cannot publish and invalidates this tentative proof.
	jr .proof_current
.first_proof
	ldh a, [rLY]
	cp 124
	jr c, .proof_admitted
	scf
	ret
.proof_admitted
	farcall MobileVirtualAlignedScene
	ld a, [wMobileVirtualForceFull]
	and a
	jp z, .ordinary
	call MobilePlanSignature
.proof_current
	ldh a, [rLY]
	cp 131
	jr c, .lookup_admitted
	scf
	ret
.lookup_admitted
	ld a, [wPalFadeDelayFrames]
	and a
	ret nz
	ld a, [wMobileVirtualPhase]
	cp MOBILE_PEERS
	ret nc
	call MobilePlanGatherPeer
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileVirtualPlanGatherValid]
	and a
	ret z ; ordinary fallback still handles an unavailable starting peer
	ld a, [wMobileAvatarPalette]
	cp 8
	jr c, .color_valid
	ld a, [wMobileVirtualCacheHit]
	cp 1
	jr z, .color_valid
	and a
	ret
.color_valid
	ld a, [wMobileVirtualNativeRotation]
	ld [wMobileVirtualPlanPending], a
	ld a, [wMobileVirtualGroundRotation]
	ld [wMobileVirtualPlanPending + 1], a
	ld a, [wMobileVirtualPhase]
	ld [wMobileVirtualPlanPending + 2], a
	ld a, [wMobileVirtualPaletteRotation]
	ld [wMobileVirtualPlanPending + 3], a
	ld a, [wMobileAvatarTile]
	ld [wMobileVirtualPlanPending + 4], a
	ld a, [wMobileAvatarPalette]
	ld [wMobileVirtualPlanPending + 5], a
	ld a, [wMobileOAMDemand]
	ld [wMobileVirtualPlanPending + 6], a
	call MobilePlanLookup
	jr nc, .miss
	call MobilePlanBeginEmission
	call MobilePlanEmit
	scf
	ret
.miss
	xor a
	ld [wMobileVirtualPlanExpandedValid], a
; Selection may tentatively use the retained resource view, just as a hit does.
; Fresh emission validates every field before publication. Repeating the full
; signature walk here buys no additional safety and consumes the seeding window.
; Select resources directly, without materializing and discarding a local or
; guest figure, and without three general dispatcher/bank round trips.
	ldh a, [rLY]
	cp 129
	jr c, .seed
	scf
	ret
.seed
	call MobilePlanBuildOrder
	ldh a, [rLY]
	cp 132
	jr c, .order_ready
	scf
	ret ; retain the completed membership/order prerequisite
.order_ready
	xor a
	ld [wMobileVirtualPlanCount], a
	ld a, 1
	ld [wMobileVirtualPlanMode], a
	call MobilePlanSeedResources
	ret c
	ld a, 4
	ld [wMobileVirtualStage], a
	call MobilePlanSelectNatives
	ld a, [wMobileVirtualStage]
	cp 9
	call z, MobilePlanEmit
	scf
	ret
.ordinary
	xor a
	ld [wMobileVirtualForceFull], a
	ret

MobilePlanHashSlot::
; Eight two-way buckets. An odd ground multiplier separates the common
; rotated-class families without coupling admission policy to any map.
	ld a, [wMobileVirtualPlanPending + 1]
	ld b, a
	add a
	add a
	add b
	ld b, a
	ld a, [wMobileVirtualPlanPending + 5]
	cp 8
	ld a, 0
	jr c, .rotation_ready
	ld a, [wMobileVirtualPlanPending + 3]
.rotation_ready
	add b
	ld b, a
	ld a, [wMobileVirtualPlanPending]
	rlca
	xor b
	ld b, a
	ld a, [wMobileVirtualPlanPending + 5]
	swap a
	xor b
	ld b, a
	ld a, [wMobileVirtualPlanPending + 6]
	rrca
	rrca
	xor b
	and MOBILE_PLAN_COUNT / 2 - 1
	add a
	ret

MobilePlanLookup::
; A failed publication retains its expanded plan. Live keys and fresh native
; validation still protect every reuse; a miss never trusts an old pixel page.
	call MobilePlanExpandedMatch
	ret c
	ld a, [wMobileVirtualPlanPending]
	ld [wMobileVirtualPlanLiveRotation], a
	ld a, 255
	ld [wMobileVirtualPlanPending], a
	call MobilePlanLookupExact
	ld a, [wMobileVirtualPlanLiveRotation]
	ld [wMobileVirtualPlanPending], a
	jr c, .expanded
	call MobilePlanLookupExact
	ret nc
.expanded
	ld a, [wMobileVirtualPlanPending + 9]
	cp 254
	call z, MobilePlanCanonicalSuccessor
	ld a, [wMobileVirtualPlanPending + 5]
	cp 8
	jr nc, .palette_ready
	ld a, [wMobileVirtualPlanPending + 3]
	ld [wMobileVirtualPlanPending + 11], a
.palette_ready
	call MobilePlanStampExpanded
	scf
	ret

MobilePlanExpandedMatch:
	ld a, [wMobileVirtualPlanExpandedValid]
	cp 1
	jr nz, .miss
	ld hl, wMobileVirtualPlanExpandedKey
for field, 7
if field != 2 && field != 4
	ld a, [wMobileVirtualPlanPending + field]
	cp [hl]
	jr nz, .miss
	inc l
endc
endr
	scf
	ret
.miss
	and a
	ret

MobilePlanStampExpanded:
	ld hl, wMobileVirtualPlanExpandedKey
for field, 7
if field != 2 && field != 4
	ld a, [wMobileVirtualPlanPending + field]
	ld [hli], a
endc
endr
	ld a, 1
	ld [wMobileVirtualPlanExpandedValid], a
	ret

MobilePlanCanonicalSuccessor::
	ld a, [wMobileVirtualNPCCount]
	ld b, a
	ld a, [wMobileVirtualNativeRotation]
	ld c, a
	ld d, 0
	call MobilePlanFirstSelected
	ld [wMobileVirtualPlanPending + 9], a
	ret

MobilePlanFirstSelected:
; B=class length, C=live start, D=base ordinal. No invalid state may turn this
; into an unbounded walk or escape the private twelve-member order table.
	ld a, b
	cp NUM_OBJECT_STRUCTS
	jr nc, .none
	and a
	jr z, .none
	add d
	cp NUM_OBJECT_STRUCTS
	jr nc, .none
	ld a, b
	cp c
	jr c, .none
	ld e, b
.loop
	ld a, b
	cp c
	jr nz, .ordinal_ready
	ld c, 0
.ordinal_ready
	ld a, c
	add d
	add LOW(wMobileVirtualOrderBase)
	ld l, a
	ld h, HIGH(wMobileVirtualOrderBase)
	ld a, [hl]
	cp NUM_OBJECT_STRUCTS
	jr nc, .none
	add LOW(wMobileVirtualPlanPending + 13)
	ld l, a
	ld h, HIGH(wMobileVirtualPlanPending)
	ld a, [hl]
	cp 255
	jr nz, .selected
	inc c
	dec e
	jr nz, .loop
.none
	ld a, 255
	ret
.selected
	ld a, c
	inc a
	ret

MobilePlanLookupExact::
	call MobilePlanHashSlot
	ld [wMobileVirtualPlanSlot], a
	call MobilePlanLookupWay
	ret c
	ld hl, wMobileVirtualPlanSlot
	inc [hl]
MobilePlanLookupWay:
	ld a, [wMobileVirtualPlanSlot]
	call MobilePlanEntryPointer
	push hl
	ld de, 26
	add hl, de
	ld a, [hl]
	cp 1
	pop hl
	jr nz, .not_found
for field, 7
if field != 2 && field != 4
if field == 3
	ld a, [wMobileVirtualPlanPending + 5]
	cp 8
	jr c, .native_rotation
endc
	ld a, [wMobileVirtualPlanPending + field]
	cp [hl]
	jr nz, .not_found
if field == 3
.native_rotation
endc
endc
	inc hl
endr
	ld de, wMobileVirtualPlanPending + 7
rept 19
	ld a, [hli]
	ld [de], a
	inc de
endr
	scf
	ret
.not_found
	and a
	ret

MobilePlanInsertionSlot:
; Prefer an invalid way. Otherwise alternate victims independently per bucket.
	call MobilePlanHashSlot
	ld [wMobileVirtualPlanSlot], a
	call MobilePlanEntryPointer
	ld de, 26
	add hl, de
	ld a, [hl]
	cp 1
	ret nz
	ld a, [wMobileVirtualPlanSlot]
	inc a
	call MobilePlanEntryPointer
	ld de, 26
	add hl, de
	ld a, [hl]
	cp 1
	jr z, .replace
	ld hl, wMobileVirtualPlanSlot
	inc [hl]
	ret
.replace
	ld a, [wMobileVirtualPlanSlot]
	rrca
	add LOW(wMobileVirtualPlanVictims)
	ld l, a
	ld h, HIGH(wMobileVirtualPlanVictims)
	ld a, [hl]
	and 1
	ld b, a
	xor 1
	ld [hl], a
	ld a, [wMobileVirtualPlanSlot]
	add b
	ld [wMobileVirtualPlanSlot], a
	ret

MACRO mobile_plan_signature_field
if \1
	ld b, a
	ld a, [wMobileVirtualPlanPreviousSignature + \2]
	cp b
	ret nz
else
	ld [wMobileVirtualPlanPreviousSignature + \2], a
endc
ENDM

MACRO mobile_plan_signature_view
for actor, NUM_OBJECT_STRUCTS
	ld hl, wMobileVirtualDescriptors + actor * 8 + 6
	ld a, [hli]
	ld d, a
	and $3f ; raw/template storage does not change canonical resource demand
	mobile_plan_signature_field \1, actor * 3
	bit 7, d
	jr z, .template{d:actor}
	ld a, [hl]
	add 8
	ld l, a
	ld h, HIGH(wShadowOAM)
	ld a, [hli]
	mobile_plan_signature_field \1, actor * 3 + 1
	inc l
	inc l
	ld a, [hl]
	jr .color{d:actor}
.template{d:actor}
	ld a, [wMobileVirtualDescriptors + actor * 8 + 1]
	mobile_plan_signature_field \1, actor * 3 + 1
	ld a, [wMobileVirtualDescriptors + actor * 8 + 3]
.color{d:actor}
	and OAM_PALETTE
	mobile_plan_signature_field \1, actor * 3 + 2
endr
	ld a, BANK(wUsedObjectPals)
	ldh [rWBK], a
	ld a, [wUsedObjectPals]
	ld b, a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, b
	mobile_plan_signature_field \1, NUM_OBJECT_STRUCTS * 3
	ld a, [wMobileVirtualNativeAvatarColors]
	mobile_plan_signature_field \1, NUM_OBJECT_STRUCTS * 3 + 1
ENDM

MobilePlanSignature:
; Compare live resource fields directly. Only changed scenes construct a new
; signature and invalidate entries; ordinary poses never freeze cached pixels.
	ld a, [wMobileVirtualPlanSignatureValid]
	cp 1
	jr nz, .changed
	call MobilePlanCompareSignature
	ret z
.changed
	xor a
for slot, MOBILE_PLAN_COUNT
	ld [wMobileVirtualPlanEntries + slot * MOBILE_PLAN_CACHE_SIZE + 26], a
endr
	xor a
	ld [wMobileVirtualPlanExpandedValid], a
	ld [wMobileVirtualPlanSlot], a
	ld [wMobileVirtualPlanEntriesUsed], a
	ld [wMobileVirtualPlanSignatureValid], a
	mobile_plan_signature_view 0
	ld a, 1
	ld [wMobileVirtualPlanSignatureValid], a
	scf ; the previous aligned proof must be checked against the changed scene
	ret

MobilePlanCompareSignature:
	mobile_plan_signature_view 1
	xor a
	ret
PURGE mobile_plan_signature_view, mobile_plan_signature_field

MobilePlanEntryPointer:
; slot*28 = slot*4 + slot*8 + slot*16, with full sixteen-bit addresses.
	ld l, a
	ld h, 0
	add hl, hl
	add hl, hl
	ld d, h
	ld e, l
	add hl, hl
	add hl, de
	add hl, hl
	add hl, de
	ld de, wMobileVirtualPlanEntries
	add hl, de
	ret

MobilePlanSeedResources:
; Keep the aligned proof in Lines14..15. All rows and color ownership below
; are private metadata; native OAM, pixels and game object RAM are immutable.
	xor a
	ld hl, wMobileVirtualLines + 16
rept 16
	ld [hli], a
endr
	ld [wMobileVirtualRectangleCount], a
	ld [wMobileVirtualCursor], a
	ld [wMobileVirtualLength], a
	ld [wMobileRenderedCount], a
	ld [wMobileVirtualBaseReady], a
	ld [wMobileVirtualPlannedPrivateColors], a
	ld hl, wMobileVirtualBindings
rept 8
	ld [hli], a
endr
	ld a, 255
	ld [wMobileVirtualNativeNext], a
	ld [wMobileVirtualGroundNext], a
	ld a, [wMobileVirtualPlanPreviousSignature + 2]
	ld e, a
	ld d, 0
	ld hl, MobilePlanPaletteBits
	add hl, de
	ld a, [hl]
	ld [wMobileVirtualPaletteUsed], a ; local palette is reserved first
	ld a, [wMobileVirtualPlanPreviousSignature + 1]
	ld [wMobileVirtualRectangleTop], a
	call MobilePlanAdmitAlignedRectangle
	jr c, .unavailable
	ld a, [wMobileAvatarPalette]
	cp 8
	jr c, .native_color
	call MobilePlanChoosePrivatePalette
	jr c, .unavailable
	ld a, 1
	ld [wMobileVirtualPlannedPrivateColors], a
	ld a, [wMobileAvatarPalette]
	ld [wMobileVirtualDroppedSlot], a
	inc a
	and 7
	ld [wMobileVirtualPaletteRotation], a
	jr .peer_row
.native_color
	ld a, 2
	ld [wMobileVirtualCacheHit], a ; use the exact current native RGB source
.peer_row
	ld a, [wMobileAvatarPalette]
	ld e, a
	ld d, 0
	ld hl, MobilePlanPaletteBits
	add hl, de
	ld a, [wMobileVirtualPaletteUsed]
	or [hl]
	ld [wMobileVirtualPaletteUsed], a
	ld a, [wMobileOAMDemand]
	ld [wMobileVirtualRectangleTop], a
	call MobilePlanAdmitAlignedRectangle
	jr c, .unavailable
	ld hl, wMobileVirtualPlanPending + 13
	ld a, 255
rept NUM_OBJECT_STRUCTS
	ld [hli], a
endr
	xor a
	ld [wMobileVirtualPlanPending + 13], a ; local output always precedes the guest
	ld a, 1
	ld [wMobileVirtualPlanCount], a
	ld [wMobileRenderedCount], a
	ld a, 32
	ld [wMobileVirtualLength], a
	and a
	ret
.unavailable
	ld a, [wMobileVirtualPlanPending + 3]
	ld [wMobileVirtualPaletteRotation], a
	xor a
	ld [wMobileVirtualPlanMode], a
	scf
	ret

MobilePlanBuildOrder:
	mobile_virtual_build_order
MobilePlanChoosePrivatePalette:
	mobile_virtual_choose_private_palette MobilePlanPaletteBits, MobilePlanFreeColor
MobilePlanAdmitAlignedRectangle:
	mobile_virtual_admit_aligned_rectangle
MobilePlanFreeColor:
	mobile_virtual_free_palette_table

MobilePlanSelectNatives::
; Select from the exact validated resource signature, without materializing
; raw/template pixels. The common aligned buckets and class order are already
; proved; ordinary NPC RAM remains immutable. One bounded bank entry handles
; the whole selection, so a saturated scene can seed its first plan.
	ldh a, [rLY]
	cp 134
	ret nc
	ld a, 1
	ld [wMobileVirtualPlanAllNPC], a
; A sentinel outside 0..7 represents no private palette loan, removing a
; redundant per-candidate flag read without changing color admission.
	ld a, [wMobileVirtualPlannedPrivateColors]
	and a
	ld a, 255
	jr z, .loan_ready
	ld a, [wMobileVirtualDroppedSlot]
.loan_ready
	ld [wMobileVirtualPlanPending + 7], a
	ld a, [wMobileVirtualLength]
	ld e, a
	ld a, [wMobileVirtualCursor]
	ld c, a
.loop
	ld a, e
	cp OAM_SIZE
	jr nc, .done
	ld a, [wMobileVirtualOrderCount]
	cp c
	jr z, .done
	ld a, c
	add LOW(wMobileVirtualOrder)
	ld l, a
	ld h, HIGH(wMobileVirtualOrder)
	ld a, [hl]
	ld b, a
	add a
	add b
	add LOW(wMobileVirtualPlanPreviousSignature)
	ld l, a
	ld h, HIGH(wMobileVirtualPlanPreviousSignature)
	ld a, [hli]
	and $1f
	jr z, .next
	ld a, [hli]
	and a
	jr z, .next
	cp SCREEN_HEIGHT_PX + OAM_Y_OFS
	jr nc, .next
	ld d, a
	ld a, [wMobileVirtualPlanPending + 7]
	cp [hl]
	jr z, .next
	ld a, d
	swap a
	and $f
	add LOW(wMobileVirtualLines + 16)
	ld l, a
	ld h, HIGH(wMobileVirtualLines)
	ld a, [hl]
	cp 5
	jr nc, .row_rejected
	inc [hl]
	ld a, b
	add LOW(wMobileVirtualPlanPending + 13)
	ld l, a
	ld h, HIGH(wMobileVirtualPlanPending)
	ld [hl], e ; actor-indexed destination preserves admitted presentation order
	ld hl, wMobileVirtualPlanCount
	inc [hl]
	ld a, e
	add 16
	ld e, a
; Only the first selected ordinal in each class changes its successor.
; Later accepted figures must not pay the shared modulo helper's stack cost.
	ld a, [wMobileVirtualNPCCount]
	cp c
	jr z, .ground_successor
	jr c, .ground_successor
	ld a, [wMobileVirtualNativeNext]
	jr .successor_needed
.ground_successor
	ld a, [wMobileVirtualGroundNext]
.successor_needed
	cp 255
	jr nz, .next
	ld a, c
	ld [wMobileVirtualSelectedOrdinal], a
	push bc
	push de
	call MobilePlanAdvanceClass
	pop de
	pop bc
	jr .next
.row_rejected
	ld a, [wMobileVirtualNPCCount]
	cp c
	jr z, .next
	jr c, .next
	xor a
	ld [wMobileVirtualPlanAllNPC], a
.next
	inc c
	jp .loop
.done
; An early capacity exit has not proved admission of the remaining NPCs.
	ld a, [wMobileVirtualNPCCount]
	cp c
	jr z, .npc_prefix_done
	jr c, .npc_prefix_done
	xor a
	ld [wMobileVirtualPlanAllNPC], a
.npc_prefix_done
	ld a, e
	ld [wMobileVirtualLength], a
	ld a, c
	ld [wMobileVirtualCursor], a
	jp MobilePlanComplete

MobilePlanAdvanceClass:
	mobile_virtual_advance_class

MobilePlanComplete::
	ld a, [wMobileVirtualPaletteRotation]
	ld [wMobileVirtualPlanPending + 11], a
	ld a, [wMobileVirtualPlanPending + 3]
	ld [wMobileVirtualPaletteRotation], a ; selection is not a publication
	ld a, [wMobileRenderedCount]
	cp 1
	jp nz, .abort
	ld a, [wMobileBudgetIndex]
	ld hl, wMobileVirtualPlanPending + 2
	cp [hl]
	jp nz, .abort ; never cache a fallback for a different starting phase
	ld a, [wMobileVirtualPlanCount]
	and a
	jp z, .abort
	cp OAM_COUNT / 4
	jp nc, .abort
	ld [wMobileVirtualPlanPending + 12], a
	ld a, [wMobileVirtualOrderCount]
	cp NUM_OBJECT_STRUCTS
	jp nc, .abort
	ld a, [wMobileAvatarPalette]
	ld [wMobileVirtualPlanPending + 7], a
	ld a, [wMobileVirtualCacheHit]
	ld [wMobileVirtualPlanPending + 8], a
	ld a, [wMobileVirtualNativeNext]
	ld [wMobileVirtualPlanPending + 9], a
	ld a, [wMobileVirtualGroundNext]
	ld [wMobileVirtualPlanPending + 10], a
	ld a, [wMobileVirtualPlanAllNPC]
	and a
	jr z, .successor_ready
	ld a, 255
	ld [wMobileVirtualPlanPending], a
	ld a, 254
	ld [wMobileVirtualPlanPending + 9], a
.successor_ready
	call MobilePlanInsertionSlot
	ld a, [wMobileVirtualPlanSlot]
	call MobilePlanEntryPointer
	push hl
	ld de, 26
	add hl, de
	ld a, [hl]
	cp 1
	jr z, .occupied
	push hl
	ld hl, wMobileVirtualPlanEntriesUsed
	ld a, [hl]
	cp MOBILE_PLAN_COUNT
	jr nc, .counted
	inc [hl]
.counted
	pop hl
.occupied
	xor a
	ld [hl], a
	pop de
for field, 26
	ld a, [wMobileVirtualPlanPending + field]
	ld [de], a
	inc de
endr
	ld a, 1
	ld [de], a ; commit complete exact keys and payload last
	ld a, [wMobileVirtualNativeNext]
	ld [wMobileVirtualPlanPending + 9], a
	ld a, [wMobileVirtualNativeRotation]
	ld [wMobileVirtualPlanPending], a
	call MobilePlanStampExpanded
	call MobilePlanBeginEmission
	ret
.abort
	xor a
	ld [wMobileVirtualPlanMode], a
	ld [wMobileVirtualStage], a
	ret

MobilePlanBeginEmission:
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	xor a
	ld [wMobileVirtualPlanEmitCursor], a
	ld [wMobileVirtualLength], a
	ld [wMobileVirtualPeerMask], a
	ld [wMobileRenderedCount], a
	ld [wMobileVirtualPlannedPrivateColors], a
	ld hl, wMobileVirtualBindings
rept 8
	ld [hli], a
endr
	ld a, [wMobileVirtualPlanPending + 9]
	ld [wMobileVirtualNativeNext], a
	ld a, [wMobileVirtualPlanPending + 10]
	ld [wMobileVirtualGroundNext], a
	ld a, 2
	ld [wMobileVirtualPlanMode], a
	ld a, 9
	ld [wMobileVirtualStage], a
	ret

MobilePlanEmit::
; Validate the complete native resource view while writing selected raw groups.
; Every destination is private; no output becomes visible on a mismatch.
	ld a, [wMobileVirtualPlanMode]
	cp 2
	jp nz, .abort
	ld a, [wMobileVirtualPlanPending + 12]
	and a
	jp z, .abort
	cp OAM_COUNT / 4
	jp nc, .abort
	ld a, [wMobileVirtualPlanPending + 13]
	and a
	jp nz, .abort
; Gathered live guest inputs materialize first into reserved offset16, while
; the private page remains unpublished. The following native pass validates all
; resource fields before any palette successor or DMA token can commit.
	ldh a, [rLY]
	cp 137
	jr nc, .busy
	ld a, 16
	ld [wMobileVirtualLength], a
	ld a, [wMobileVirtualPlanPending + 7]
	cp 8
	jr nc, .abort
	ld [wMobileAvatarPalette], a
	ld a, [wMobileVirtualPlanPending + 8]
	ld [wMobileVirtualCacheHit], a
	call MobilePlanWriteGuest
	jr c, .abort
	call MobilePlanEmitNativeView
	jr c, .abort
	ld a, [wMobileVirtualPlanPending + 12]
	inc a
	swap a
	ld [wMobileVirtualLength], a
	ld a, [wMobileVirtualPlanPending + 11]
	ld [wMobileVirtualPaletteRotation], a
	ld a, 7
	ld [wMobileVirtualStage], a
	jp MobilePlanPublish
.busy
	ret
.abort
	ld a, [wMobileVirtualPlanPending + 3]
	ld [wMobileVirtualPaletteRotation], a
	xor a
	ld [wMobileVirtualStage], a
	ld [wMobileVirtualPlanMode], a
	ret

MobilePlanWriteGuest::
; This one-guest plan has already admitted its exact palette and four quads.
; Emit only that committed pose, omitting ordinary admission and bank dispatch.
	ld a, [wMobileBudgetIndex]
	cp MOBILE_PEERS
	jp nc, .invalid
	ld a, [wMobileVirtualWindow]
	cp MOBILE_PEERS
	jp nc, .invalid
	ld a, [wMobileVirtualPlanPending + 8]
	dec a
	cp 2
	jp nc, .invalid
	ld a, [wMobileAvatarPalette]
	cp 8
	jp nc, .invalid
	ld a, [wMobileVirtualFrame]
	and $c
	cp $c
	jr z, .mirror
	cp 8
	ld a, 0
	jr nc, .mirror_ready
	ld a, [wMobileVirtualFrame]
	and 3
	cp 3
	ld a, 0
	jr nz, .mirror_ready
.mirror
	ld a, OAM_XFLIP
.mirror_ready
	ld c, a
	ld a, [wMobileVirtualWindow]
	add a
	add a
	add $64
	ld b, a
	bit 5, c
	jr z, .columns_ready
	inc b
.columns_ready
	ld a, [wMobileAvatarPalette]
	or c
	ld c, a
	ld a, [wMobileOAMWriteCursor]
	ld e, a
	ld a, [wMobileOAMDemand]
	ld d, a
	ld hl, wMobileVirtualOutput + 16
	call MobilePlanWriteGuestQuads
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, MobilePlanPaletteBits
	add hl, de
	ld a, [hl]
	ld [wMobileVirtualPeerMask], a
	ld a, [wMobileVirtualPlanPending + 8]
	cp 2
	jr z, .metadata
	ld a, [wMobileAvatarPalette]
	add LOW(wMobileVirtualBindings)
	ld l, a
	ld h, HIGH(wMobileVirtualBindings)
	ld a, [wMobileBudgetIndex]
	inc a
	ld [hl], a
	ld a, 1
	ld [wMobileVirtualPlannedPrivateColors], a
	ld a, [wMobileBudgetIndex]
	add a
	add a
	add a
	ld e, a
	ld d, 0
	ld hl, wMobilePrivatePalColors
	add hl, de
	push hl
	ld hl, wMobileVirtualFrameColors
	add hl, de
	ld d, h
	ld e, l
	pop hl
rept 1 palettes
	ld a, [hli]
	ld [de], a
	inc de
endr
.metadata
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	call MobilePlanPeerPointer
	ld bc, MOBILE_PEER_OAM_START + 1
	add hl, bc
	ld a, 16
	ld [hl], a
	ld bc, MOBILE_PEER_PALETTE - MOBILE_PEER_OAM_START - 1
	add hl, bc
	ld a, [wMobileAvatarPalette]
	ld [hl], a
	ld a, 1
	ld [wMobileRenderedCount], a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	and a
	ret
.invalid
	scf
	ret

MobilePlanWriteGuestQuads:
	mobile_virtual_write_peer_quads

MACRO mobile_plan_compare_live
	ld b, a
	ld a, [wMobileVirtualPlanPreviousSignature + \1]
	cp b
	jp nz, .invalid
ENDM

MobilePlanEmitNativeView::
; The cached actor->offset map removes ordered-ID arithmetic and combines raw
; source decoding, Y/color validation and fresh copying in one bounded pass.
for actor, NUM_OBJECT_STRUCTS
.actor{d:actor}
	ld a, [wMobileVirtualDescriptors + actor * 8 + 6]
	ld c, a
	and $3f
	mobile_plan_compare_live actor * 3
	bit 7, c
	jp z, .template{d:actor}
	ld a, [wMobileVirtualDescriptors + actor * 8 + 7]
	ld l, a
	ld h, HIGH(wShadowOAM)
	ld a, [wMobileVirtualPlanPending + 13 + actor]
	cp 255
	jr z, .unselected_raw{d:actor}
	cp OAM_SIZE - 16 + 1
	jp nc, .invalid
	ld e, a
	and $f
	jp nz, .invalid
	ld d, HIGH(wMobileVirtualOutput)
.raw_guard{d:actor}
	ldh a, [rLY]
if actor >= 11
	cp 142
elif actor >= 8
	cp 141
elif actor >= 4
	cp 140
elif actor >= 1
	cp 139
else
	cp 138
endc
	jp nc, .busy
for byte, 16
	ld a, [hli]
if byte == 8
	mobile_plan_compare_live actor * 3 + 1
elif byte == 11
	ld c, a
	and OAM_PALETTE
	mobile_plan_compare_live actor * 3 + 2
	ld a, c
endc
	ld [de], a
	inc e
endr
	jp .next{d:actor}
.unselected_raw{d:actor}
	ld a, l
	add 8
	ld l, a
	ld a, [hli]
	mobile_plan_compare_live actor * 3 + 1
	inc l
	inc l
	ld a, [hl]
	and OAM_PALETTE
	mobile_plan_compare_live actor * 3 + 2
	jp .next{d:actor}
.template{d:actor}
	ld a, [wMobileVirtualDescriptors + actor * 8 + 1]
	mobile_plan_compare_live actor * 3 + 1
	ld a, [wMobileVirtualDescriptors + actor * 8 + 3]
	and OAM_PALETTE
	mobile_plan_compare_live actor * 3 + 2
	ld a, [wMobileVirtualPlanPending + 13 + actor]
	cp 255
	jr z, .next{d:actor}
	cp OAM_SIZE - 16 + 1
	jp nc, .invalid
	ld e, a
	and $f
	jp nz, .invalid
	ld d, HIGH(wMobileVirtualOutput)
if actor == 0
.local_template_guard
	ldh a, [rLY]
	cp 137
	jp nc, .busy
endc
if actor == 0
	ld bc, wMobileVirtualDescriptors
	ld a, [wMobileVirtualDescriptors + 4]
	ld l, a
	ld a, [wMobileVirtualDescriptors + 5]
	ld h, a
	xor a
	scf
	call MobileNativeRectangleTrampoline
else
	call MobilePlanNativeShape{d:actor}
	jp c, .busy
endc
.next{d:actor}
endr
	ld a, BANK(wUsedObjectPals)
	ldh [rWBK], a
	ld a, [wUsedObjectPals]
	ld b, a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, b
	mobile_plan_compare_live NUM_OBJECT_STRUCTS * 3
	ld a, [wMobileVirtualNativeAvatarColors]
	mobile_plan_compare_live NUM_OBJECT_STRUCTS * 3 + 1
	and a
	ret
.invalid
	xor a
	ld [wMobileVirtualPlanSignatureValid], a
	ld [wMobileVirtualForceFull], a
.busy
	scf
	ret
PURGE mobile_plan_compare_live

; Fixed native actor slots need no eviction or identity inference. The complete
; five-field key covers every non-coordinate input to the immutable facing ROM.
for actor, 1, NUM_OBJECT_STRUCTS
MobilePlanNativeShape{d:actor}::
	ld a, [wMobileNativeShapeKeys + (actor - 1) * 6 + 5]
	cp 1
	jp nz, .miss
for field, 5
if field == 4
	DEF shape_field = 7
else
	DEF shape_field = field + 2
endc
	ld a, [wMobileVirtualDescriptors + actor * 8 + shape_field]
	ld b, a
	ld a, [wMobileNativeShapeKeys + (actor - 1) * 6 + field]
	cp b
	jp nz, .miss
endr
; Hit replay is smaller than a transform. Include the complete remaining native
; validation tail and return in the independently executed deadline tests.
.hit_guard
	ldh a, [rLY]
if actor >= 11
	cp 142
elif actor >= 8
	cp 141
elif actor >= 4
	cp 140
else
	cp 139
endc
	jp nc, .busy
	ld a, [wMobileVirtualDescriptors + actor * 8]
	ld b, a
	ld a, [wMobileVirtualDescriptors + actor * 8 + 1]
	ld c, a
	ld hl, wMobileNativeShapes + (actor - 1) * 16
for byte, 16
	ld a, [hli]
if byte % 4 == 0
	add c
elif byte % 4 == 1
	add b
endc
	ld [de], a
	inc e
endr
	and a
	ret
.miss
	ldh a, [rLY]
if actor >= 10
	cp 140
elif actor >= 7
	cp 139
elif actor >= 4
	cp 138
else
	cp 137
endc
	jp nc, .busy
	xor a
	ld [wMobileNativeShapeKeys + (actor - 1) * 6 + 5], a
	ld bc, wMobileVirtualDescriptors + actor * 8
	ld a, [wMobileVirtualDescriptors + actor * 8 + 4]
	ld l, a
	ld a, [wMobileVirtualDescriptors + actor * 8 + 5]
	ld h, a
	xor a
	scf
	call MobileNativeRectangleTrampoline
; Normalize the just-produced exact quads. Coordinates are added afresh on hit,
; so walking, camera scrolling and byte wrapping cannot freeze a cached actor.
	ld a, e
	sub 16
	ld l, a
	ld h, HIGH(wMobileVirtualOutput)
	ld a, [wMobileVirtualDescriptors + actor * 8]
	ld b, a
	ld a, [wMobileVirtualDescriptors + actor * 8 + 1]
	ld c, a
	ld de, wMobileNativeShapes + (actor - 1) * 16
for byte, 16
	ld a, [hli]
if byte % 4 == 0
	sub c
elif byte % 4 == 1
	sub b
endc
	ld [de], a
	inc de
endr
for field, 5
if field == 4
	DEF shape_field = 7
else
	DEF shape_field = field + 2
endc
	ld a, [wMobileVirtualDescriptors + actor * 8 + shape_field]
	ld [wMobileNativeShapeKeys + (actor - 1) * 6 + field], a
endr
	ld a, 1
	ld [wMobileNativeShapeKeys + (actor - 1) * 6 + 5], a
	and a
	ret
.busy
	scf
	ret
endr
PURGE shape_field

MobilePlanGatherPeer:
	mobile_virtual_gather_peer MobilePlanPeerPointer, MobilePlanRejectUnprovenRows, 1
MobilePlanPeerPointer:
	ld hl, wMobilePeer1
	ld bc, MOBILE_PEER_SIZE
.loop
	and a
	ret z
	add hl, bc
	dec a
	jr .loop
MobilePlanRejectUnprovenRows:
; Cached admission exists only for the proved full aligned scene. Reject any
; accidental call which reaches the ordinary unproved row-check branch.
	scf
	ret

MobilePlanEnd::

MobileVirtualCurrentColors:
	xor a
	ld [wMobileVirtualNativeAvatarColors], a
; Audit native hardware color metadata once for this composition. Peers reuse
; current exact sources, including a slot that moved since the last upload job.
; Private construction remains isolated from native save and palette metadata.
	ld a, 255
for avatar, NUM_PLAYER_GENDERS / 2
	ld [wMobileVirtualNativeColors + avatar], a
endr
	ld a, 1
	ldh [rWBK], a
for slot, 8
if slot == PAL_OW_WEATHER
	ld a, [wCurWeather]
	ld b, a
	ld a, [wOverworldWeatherCooldown]
	or b
	jr z, .ordinary_color{d:slot}
	ld a, [wMobileVirtualNativeAvatarColors]
	or 1 << slot
	ld [wMobileVirtualNativeAvatarColors], a
	jp .next{d:slot}
.ordinary_color{d:slot}
endc
; Non-avatar IDs cannot be exact avatar sources, irrespective of their glow
; or type. Reject them before reading metadata irrelevant to this audit.
	ld a, [wLoadedObjPal0 + slot]
	cp NUM_PLAYER_GENDERS
	jp nc, .next{d:slot}
	ld b, a
	ld a, [wLoadedObjPalType]
	and 1 << slot
	jp nz, .next{d:slot}
	ld a, [wLoadedObjPalGlows + slot]
	ld c, a
	ld a, [wLoadedObjPalPrevGlows + slot]
	or c
	jp nz, .next{d:slot}
	ld a, [wMobileVirtualNativeAvatarColors]
	or 1 << slot
	ld [wMobileVirtualNativeAvatarColors], a
for avatar, NUM_PLAYER_GENDERS
	ld a, b
	cp avatar
	jr nz, .not_avatar{d:slot}_{d:avatar}
	ld a, [wMobileVirtualNativeColors + avatar / 2]
if avatar & 1
	and $0f
	or slot << 4
else
	and $f0
	or slot
endc
	ld [wMobileVirtualNativeColors + avatar / 2], a
	jr .next{d:slot}
.not_avatar{d:slot}_{d:avatar}
endr
.next{d:slot}
endr
	xor a
	ld [wMobileVirtualNeedColorAudit], a
	ld a, [wMobileVirtualNativeColors]
	ld b, a
	ld a, [wMobileVirtualNativeColors + 1]
	or b
	and $88 ; any missing ($f) nibble
	jr nz, .needs_audit
	jr .done
.needs_audit
	ld a, 1
	ld [wMobileVirtualNeedColorAudit], a
.done
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ret

MobilePlanPaletteBits: db 1, 2, 4, 8, 16, 32, 64, 128


MobilePlanPublish::
	mobile_virtual_publish_page MobilePlanEmit.abort
