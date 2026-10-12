; Preserve a complete current native scene when a coarse proof can admit it.
; Only presentation is shared when total OAM is full; no object record changes.
SECTION "Mobile Virtual Fast Scene", ROMX, BANK[MOBILE_ROM_BANK]
MobileVirtualFastSceneStart::
MobileVirtualFastScene:
	ld a, 1
	ld [wMobileVirtualCoarseTrial], a
	ld a, 255
	ld [wMobileVirtualDroppedSlot], a
	ld [wMobileVirtualDroppedSlot2], a
	xor a
	ld [wMobileVirtualCursor], a
MobileVirtualFastSceneContinue::
	ld a, [wMobileVirtualCursor]
	add a
	ld e, a
	ld d, 0
	ld hl, .entries
	add hl, de
	ld a, [hli]
	ld h, [hl]
	ld l, a
	jp hl
.entries
for slot, OAM_COUNT / 4
	dw .group{d:slot}
endr
for slot, OAM_COUNT / 4
.group{d:slot}
; Fixed hardware offsets avoid per-figure pointer construction and counters.
	ld a, [wShadowOAM + slot * 16]
	ld b, a
	ld a, [wShadowOAM + slot * 16 + 8]
	cp b
	jr c, .top{d:slot}
	ld a, b
.top{d:slot}
	and a
	jr z, .next{d:slot}
	cp 160
	jr nc, .next{d:slot}
	ld c, a
	swap a
	and $f
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualLines + 16
	add hl, de
	inc [hl]
	ld a, [hl]
	cp 6
	jp nc, .failed
	ld a, c
	and $f
	jr z, .row_ready{d:slot}
	inc hl
	inc [hl]
	ld a, [hl]
	cp 6
	jp nc, .failed
.row_ready{d:slot}
	ld a, [wMobileVirtualNeedColorAudit]
	and a
	jr z, .color_ready{d:slot}
	ld a, [wShadowOAM + slot * 16 + 3]
	and OAM_PALETTE
	ld e, a
	ld d, 0
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [wMobileVirtualPaletteUsed]
	or [hl]
	ld [wMobileVirtualPaletteUsed], a
.color_ready{d:slot}
	ld a, 1
	ld [wMobileVirtualKeepSlots + slot], a
	ld hl, wMobileVirtualRectangleCount
	inc [hl]
.next{d:slot}
if slot + 1 < OAM_COUNT / 4
	ld a, slot + 1
	ld [wMobileVirtualCursor], a
	ldh a, [rLY]
	cp 134
	ret nc
endc
endr
.finish
	ld a, [wMobileVirtualRectangleCount]
	swap a
	ld [wMobileVirtualLength], a
	ld a, 1
	ld [wMobileVirtualBaseReady], a
	xor a
	ld [wMobileVirtualCursor], a
	ld a, [wMobileVirtualRectangleCount]
	ld b, a
	ld a, OAM_COUNT / 4
	sub b
	jr z, .two_holes
	cp 2
	jr c, .one_hole
	cp MOBILE_PEERS + 1
	jr c, .limit
	ld a, MOBILE_PEERS
.limit
	ld [wMobileVirtualRemoteLimit], a
	and a
	ret
.failed
	xor a
	ld [wMobileVirtualCoarseTrial], a
	scf
	ret
.two_holes
	ld a, 2
	jr .drop_count
.one_hole
	ld a, 1
.drop_count
	ld [wMobileVirtualDropRemaining], a
.drop
; Rotate two guests under total OAM pressure. Whole native figures share only
; their presentation; local OAM stays intact and simulation remains unchanged.
	ld a, 2
	ld [wMobileVirtualRemoteLimit], a
	ld a, [wMobileVirtualNativeRotation]
.modulo
	cp OAM_COUNT / 4
	jr c, .hole
	sub OAM_COUNT / 4
	jr .modulo
.hole
	ld c, a
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualKeepSlots
	add hl, de
	ld a, [hl]
	and a
	jr z, .next_hole
	ld a, c
	swap a
	ld b, a
	ld a, [wPlayerCurrentOAMSlot]
	cp b
	jr nz, .selected
.next_hole
	ld a, c
	inc a
	jr .modulo
.selected
	ld a, [wMobileVirtualDroppedSlot]
	cp 255
	ld a, c
	jr nz, .second_hole
	ld [wMobileVirtualDroppedSlot], a
	jr .hole_saved
.second_hole
	ld [wMobileVirtualDroppedSlot2], a
.hole_saved
	ld a, c
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualKeepSlots
	add hl, de
	ld [hl], 0
	ld a, c
	swap a
	ld l, a
	ld h, HIGH(wShadowOAM)
	ld b, [hl]
	ld de, 8
	add hl, de
	ld a, [hl]
	cp b
	jr c, .hole_top
	ld a, b
.hole_top
	ld c, a
	swap a
	and $f
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualLines + 16
	add hl, de
	dec [hl]
	ld a, c
	and $f
	jr z, .bins_ready
	inc hl
	dec [hl]
.bins_ready
	ld hl, wMobileVirtualRectangleCount
	dec [hl]
	ld a, [wMobileVirtualLength]
	sub 16
	ld [wMobileVirtualLength], a
	ld hl, wMobileVirtualDropRemaining
	dec [hl]
	jr z, .dropped
	ld a, [wMobileVirtualDroppedSlot]
	inc a
	jp .modulo
.dropped
	and a
	ret

MobileVirtualRestoreUnusedHole:
	ld a, [wMobileVirtualLength]
	cp OAM_SIZE
	ret nc
	ld a, [wMobileVirtualDroppedSlot]
	call .restore
	ld a, [wMobileVirtualDroppedSlot2]
.restore
	cp OAM_COUNT / 4
	ret nc
	push af
	swap a
	ld l, a
	ld h, HIGH(wShadowOAM)
	ld b, [hl]
	ld de, 8
	add hl, de
	ld a, [hl]
	cp b
	jr c, .top
	ld a, b
.top
	ld [wMobileVirtualRectangleTop], a
	call MobileVirtualAdmitRectangleTop
	pop bc
	ret c
	ld a, b
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualKeepSlots
	add hl, de
	ld [hl], 1
	ld a, [wMobileVirtualLength]
	add 16
	ld [wMobileVirtualLength], a
	ret
MobileVirtualPublishFastGuests:
; The preserved native base needs no second inventory scan. Guest groups are
; a contiguous suffix, and each free OAM hole is searched at most once.
	ld a, [wMobileRenderedCount]
	and a
	ret z
	ld [wMobileVirtualIndex], a
	xor a
	ld [wMobileVirtualHeaderOAM], a
	ld a, [wMobileVirtualDroppedSlot]
	call .hide_dropped
	ld a, [wMobileVirtualDroppedSlot2]
	call .hide_dropped
.guest
	ld a, [wMobileVirtualPreviousOAM]
	swap a
	and $f
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualExtraOwners
	add hl, de
	ld a, [hl]
	and a
	ret z
	cp MOBILE_PEERS + 1
	ret nc
	ld [wMobileVirtualGroupOwner], a
	ld a, [wMobileVirtualHeaderOAM]
	ld c, a
	ld e, a
	ld hl, wMobileVirtualKeepSlots
	add hl, de
.find
	ld a, c
	cp OAM_COUNT / 4
	ret nc
	ld a, [hli]
	and a
	jr z, .found
	inc c
	jr .find
.found
	dec hl
	ld [hl], 1
	ld a, c
	inc a
	ld [wMobileVirtualHeaderOAM], a
	ld a, c
	swap a
	ld [wMobileOAMReadCursor], a
	ld e, a
	ld d, HIGH(wShadowOAM)
	ld a, [wMobileVirtualPreviousOAM]
	add LOW(wMobileVirtualOutput)
	ld l, a
	adc HIGH(wMobileVirtualOutput)
	sub l
	ld h, a
rept 16
	ld a, [hli]
	ld [de], a
	inc e
endr
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileVirtualGroupOwner]
	dec a
	call MobileNativePeerPointer
	ld bc, MOBILE_PEER_OAM_START
	add hl, bc
	ld a, [wMobileOAMReadCursor]
	ld [hl], a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileVirtualPreviousOAM]
	add 16
	ld [wMobileVirtualPreviousOAM], a
	ld hl, wMobileVirtualIndex
	dec [hl]
	jp nz, .guest
	ret
.hide_dropped
	cp OAM_COUNT / 4
	ret nc
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualKeepSlots
	add hl, de
	ld a, [hl]
	and a
	ret nz ; publication restored this native group when no guest needed it
	ld a, e
	swap a
	ld l, a
	ld h, HIGH(wShadowOAM)
rept 4
	ld [hl], 160
	inc l
	inc l
	inc l
	inc l
endr
	ret

MobileVirtualFastSceneEnd::
ASSERT MobileVirtualFastSceneEnd - MobileVirtualFastSceneStart <= 2048
