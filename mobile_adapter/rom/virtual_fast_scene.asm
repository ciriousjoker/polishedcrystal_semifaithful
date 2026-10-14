; Exact aligned-scene admission for a single guest and ordinary rectangles.
SECTION "Mobile Virtual Fast Scene", ROMX, BANK[MOBILE_ROM_BANK]
MobileVirtualFastSceneStart::

MACRO mobile_virtual_select_raw_native
; Canonical ordinary rectangle, HL=descriptor+6. Admission and output are
; identical to the local helper, without its general call/return protocol.
	inc l
	ld b, [hl]
	ld a, b
	add 8
	ld l, a
	ld h, HIGH(wShadowOAM)
	ld c, [hl]
	ld a, c
	and a
	jp z, \1
	cp SCREEN_HEIGHT_PX + OAM_Y_OFS
	jp nc, \1
	inc l
	inc l
	inc l
	ld a, [hl]
	and OAM_PALETTE
	ld e, a
	ld a, [wMobileVirtualPlannedPrivateColors]
	and a
	jr z, .color_available\@
	ld a, [wMobileVirtualDroppedSlot]
	cp e
	jp z, \1
.color_available\@
	ld a, c
	swap a
	and $f
	add LOW(wMobileVirtualLines + 16)
	ld l, a
	ld h, HIGH(wMobileVirtualLines)
	ld a, [hl]
	cp 5
	jp nc, \1
	ldh a, [rLY]
	cp 141
	jp nc, \2
	inc [hl]
	ld a, [wMobileVirtualLength]
	ld e, a
	ld l, b
	ld h, HIGH(wShadowOAM)
	ld d, HIGH(wMobileVirtualOutput)
rept 16
	ld a, [hli]
	ld [de], a
	inc e
endr
	ld a, e
	ld [wMobileVirtualLength], a
	call MobileVirtualAdvanceNativeClass
	jp \1
ENDM

MobileVirtualAlignedScene:
; Gather one compact, immutable view of every canonical native source. The
; accelerated selector is used only when more than ten visible native figures
; already prove total OAM pressure, with exactly one admitted guest. Otherwise
; the full exact-row path remains responsible for actual hardware admission.
	xor a
	ld [wMobileVirtualForceFull], a
	ld [wMobileVirtualLines + 14], a
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	ret nz
	ld a, [wMobileVirtualRemoteLimit]
	cp 1
	ret nz
	ldh a, [hUsedOAMIndex]
	cp OAM_SIZE
	ret nz
	ld a, [wMobileVirtualDescriptors + 7]
	add 8 ; native four-quad order is reversed: the third quad has the top Y
	ld l, a
	ld h, HIGH(wShadowOAM)
	ld a, [hl]
	and $f
	ld [wMobileVirtualLines + 15], a
	ld d, a
	ld b, 0 ; accumulate only actual visible-source alignment differences
	ld c, 0 ; visible native rectangles
; The completed native page already proves ten complete canonical rectangles.
; Inspect their literal native tops directly, then inspect only overflow
; templates. Do not duplicate thirteen groups into another metadata view.
for raw_slot, OAM_COUNT / 4
	ld a, [wShadowOAM + raw_slot * 16 + 8]
	and a
	jr z, .raw_next{d:raw_slot}
	cp SCREEN_HEIGHT_PX + OAM_Y_OFS
	jr nc, .raw_next{d:raw_slot}
	inc c
	xor d
	and $f
	or b
	ld b, a
.raw_next{d:raw_slot}
endr
for native_slot, NUM_OBJECT_STRUCTS
	ld a, [wMobileVirtualDescriptors + native_slot * 8 + 6]
	bit 7, a
	jr nz, .next{d:native_slot}
	and a
	jr z, .next{d:native_slot}
	ld a, [wMobileVirtualDescriptors + native_slot * 8 + 1]
	and a
	jr z, .next{d:native_slot}
	cp SCREEN_HEIGHT_PX + OAM_Y_OFS
	jr nc, .next{d:native_slot}
	inc c
	xor d
	and $f
	or b
	ld b, a
.next{d:native_slot}
endr
	ld a, c
	ld [wMobileVirtualLines + 14], a
	cp OAM_COUNT / 4 + 1
	ret c
	ld a, b
	and a
	ret nz
	ld a, 1
	ld [wMobileVirtualForceFull], a
	ret

MobileVirtualAlignedNativePass:
; Keep the output and class-list cursors live across raw native groups. Every
; source is still this frame's immutable native OAM; admission never reuses
; coordinates, palettes or pixels from an earlier frame.
	ld a, [wMobileVirtualLength]
	ld e, a
	ld d, HIGH(wMobileVirtualOutput)
	ld a, [wMobileVirtualCursor]
	ld c, a
.loop
	ld a, e
	cp OAM_SIZE
	jp nc, .done
	ld a, [wMobileVirtualOrderCount]
	cp c
	jp z, .done
	ldh a, [rLY]
	cp 140
	jp nc, .busy
	ld a, c
	add LOW(wMobileVirtualOrder)
	ld l, a
	ld h, HIGH(wMobileVirtualOrder)
	ld a, [hl]
	ld [wMobileVirtualIndex], a
	add a
	add a
	add a
	add LOW(wMobileVirtualDescriptors + 6)
	ld l, a
	ld h, HIGH(wMobileVirtualDescriptors)
	bit 7, [hl]
	jp z, .template
	inc l
	ld b, [hl]
	ld a, b
	add 8
	ld l, a
	ld h, HIGH(wShadowOAM)
	ld a, [hl]
	and a
	jr z, .advance
	cp SCREEN_HEIGHT_PX + OAM_Y_OFS
	jr nc, .advance
	inc l
	inc l
	inc l
	ld a, [hl]
	and OAM_PALETTE
	ld h, a
	ld a, [wMobileVirtualPlannedPrivateColors]
	and a
	jr z, .color_available
	ld a, [wMobileVirtualDroppedSlot]
	cp h
	jr z, .advance
.color_available
	ld a, b
	add 8
	ld l, a
	ld h, HIGH(wShadowOAM)
	ld a, [hl]
	swap a
	and $f
	add LOW(wMobileVirtualLines + 16)
	ld l, a
	ld h, HIGH(wMobileVirtualLines)
	ld a, [hl]
	cp 5
	jr nc, .advance
	inc [hl]
	ld a, [wMobileVirtualPlanMode]
	and a
	jr nz, .collect_raw
	ld l, b
	ld h, HIGH(wShadowOAM)
rept 16
	ld a, [hli]
	ld [de], a
	inc e
endr
	jr .raw_recorded
.collect_raw
	push bc
	push de
	call MobilePlanRecordNative
	pop de
	pop bc
	ld a, e
	add 16
	ld e, a
.raw_recorded
	ld a, c
	ld [wMobileVirtualSelectedOrdinal], a
	push bc
	call MobileVirtualAdvanceNativeClass
	pop bc
.advance
	inc c
	jp .loop
.template
; Unusual/overflow materialization retains the existing bounded helper. Flush
; live cursors before it and restart the register batch from authoritative state.
	ld a, e
	ld [wMobileVirtualLength], a
	push af
	ld a, c
	ld [wMobileVirtualCursor], a
	ld [wMobileVirtualSelectedOrdinal], a
	call MobileVirtualAlignedNativeDescriptor
	pop bc
	ret c
	ld a, [wMobileVirtualLength]
	cp b
	call nz, MobileVirtualAdvanceNativeClass
	ld hl, wMobileVirtualCursor
	inc [hl]
	jp MobileVirtualAlignedNativePass
.done
	ld a, e
	ld [wMobileVirtualLength], a
	xor a
	ld [wMobileVirtualCursor], a
	ld a, [wMobileVirtualPlanMode]
	and a
	jr z, .publish
	farcall MobilePlanComplete
	ret
.publish
	ld a, 7
	ld [wMobileVirtualStage], a
	ret
.busy
	ld a, e
	ld [wMobileVirtualLength], a
	ld a, c
	ld [wMobileVirtualCursor], a
	scf
	ret

MobileVirtualAlignedNativeGroup:
; Proven canonical native sources share exact shifted row buckets. After the
; urgent guest, later native colors cannot affect another guest allocation.
	ld [wMobileVirtualIndex], a
	ld a, [wMobileVirtualLength]
	cp OAM_SIZE - 16 + 1
	ret nc
	ld a, [wMobileVirtualIndex]
	add a
	add a
	add a
	add LOW(wMobileVirtualDescriptors + 6)
	ld l, a
	ld h, HIGH(wMobileVirtualDescriptors)
MobileVirtualAlignedNativeDescriptor:
; HL points to the exact descriptor selected above, or to the local prefix.
	bit 7, [hl]
	jp nz, MobileVirtualAlignedRawRectangle
	ld a, 255
	ld [wMobileVirtualHeaderOAM], a
	ld a, l
	sub 5
	ld l, a
	ld a, [hl]
	ld c, a
	inc l
	inc l
	ld a, [hl]
	and OAM_PALETTE
.source_color
	ld e, a
	ld d, 0
	ld a, c
	and a
	ret z
	cp SCREEN_HEIGHT_PX + OAM_Y_OFS
	ret nc
	ld a, [wMobileVirtualPlannedPrivateColors]
	and a
	jr z, .color_available
	ld a, [wMobileVirtualDroppedSlot]
	cp e
	ret z
.color_available
	ld a, [wMobileVirtualIndex]
	and a
	jr nz, .ordinary_color
; No later guest follows aligned native admission. Only the local prefix
; needs to reserve a native color before the one urgent private loan.
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [wMobileVirtualPaletteUsed]
	or [hl]
	ld [wMobileVirtualPaletteUsed], a
.ordinary_color
	ld a, c
	swap a
	and $f
	ld e, a
	ld hl, wMobileVirtualLines + 16
	add hl, de
	ld a, [hl]
	cp 5
	ret nc
	ld a, [wMobileVirtualIndex]
	and a
	jr z, .admitted ; the caller already admits the bounded local prefix
; Reject full rows and occupied colors before applying the copy deadline.
; Such candidates require no pixel work and must not strand a complete scene
; merely because their hypothetical template would have been expensive.
	ld b, 140 ; bounded descriptor program plus publication fits before VBlank
	ld a, [wMobileVirtualHeaderOAM]
	cp 255
	jr z, .deadline
	inc b
.deadline
	ldh a, [rLY]
	cp b
	jr nc, .busy
.admitted
	inc [hl]
	ld a, [wMobileVirtualPlanMode]
	and a
	jp nz, MobileVirtualCollectNative
	ld a, [wMobileVirtualHeaderOAM]
	cp 255
	jr z, .template
	ld [wMobileVirtualGroupPointer], a
	jp MobileVirtualCopyRawRectangle
.template
	ld a, [wMobileVirtualIndex]
	add a
	add a
	add a
	add LOW(wMobileVirtualDescriptors)
	ld c, a
	ld b, HIGH(wMobileVirtualDescriptors)
	ld h, b
	add 4
	ld l, a
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld a, [wMobileVirtualLength]
	ld e, a
	ld d, HIGH(wMobileVirtualOutput)
	xor a ; carry plus zero selects direct immutable-descriptor input
	scf
	mobile_native_rectangle_call
	ld hl, wMobileVirtualLength
	ld a, [hl]
	add 16
	ld [hl], a
	ret
.busy
	scf
	ret

MobileVirtualAlignedRawRectangle::
; Preserve the descriptor's literal reversed native OAM offset in B. Read its
; already complete pixels directly, without a second header/pointer transaction.
; This entry is exclusively for sources proven canonical by AlignedScene.
	inc l
	ld b, [hl]
	ld a, b
	add 8
	ld l, a
	ld h, HIGH(wShadowOAM)
	ld c, [hl]
	ld a, c
	and a
	ret z
	cp SCREEN_HEIGHT_PX + OAM_Y_OFS
	ret nc
	inc l
	inc l
	inc l
	ld a, [hl]
	and OAM_PALETTE
	ld e, a
	ld a, [wMobileVirtualPlannedPrivateColors]
	and a
	jr z, .color_available
	ld a, [wMobileVirtualDroppedSlot]
	cp e
	ret z
.color_available
	ld a, [wMobileVirtualIndex]
	and a
	jr nz, .ordinary
	ld d, 0
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [wMobileVirtualPaletteUsed]
	or [hl]
	ld [wMobileVirtualPaletteUsed], a
.ordinary
	ld a, c
	swap a
	and $f
	add LOW(wMobileVirtualLines + 16)
	ld l, a
	ld h, HIGH(wMobileVirtualLines)
	ld a, [hl]
	cp 5
	ret nc
	ld a, [wMobileVirtualLength]
	cp OAM_SIZE - 16 + 1
	ret nc
	ld e, a
	ld a, [wMobileVirtualIndex]
	and a
	jr z, .admitted
	ldh a, [rLY]
	cp 141 ; raw copy has the smaller measured bound
	jr nc, .busy
.admitted
	inc [hl]
	ld a, [wMobileVirtualPlanMode]
	and a
	jp nz, MobileVirtualCollectNative
	ld l, b
	ld h, HIGH(wShadowOAM)
	ld d, HIGH(wMobileVirtualOutput)
rept 16
	ld a, [hli]
	ld [de], a
	inc e
endr
	ld a, e
	ld [wMobileVirtualLength], a
	and a
	ret
.busy
	scf
	ret
MobilePlanRecordNative::
; Called after exact admission; record the actor's current private destination.
	ld a, [wMobileVirtualPlanCount]
	cp OAM_COUNT / 4 - 1
	ret nc
	ld a, [wMobileVirtualIndex]
	cp NUM_OBJECT_STRUCTS
	ret nc
	add LOW(wMobileVirtualPlanPending + 13)
	ld l, a
	ld h, HIGH(wMobileVirtualPlanPending)
	ld a, [wMobileVirtualLength]
	ld [hl], a
	ld hl, wMobileVirtualPlanCount
	inc [hl]
	ret

MobileVirtualCollectNative:
	call MobilePlanRecordNative
	ld hl, wMobileVirtualLength
	ld a, [hl]
	add 16
	ld [hl], a
	and a
	ret

ASSERT LOW(wMobileVirtualBindings) + 7 < $100
ASSERT LOW(wMobileVirtualLines + 16) + 15 < $100

MobileVirtualAdmitAlignedRectangle:
	mobile_virtual_admit_aligned_rectangle

MobileVirtualFastSceneEnd::
ASSERT MobileVirtualFastSceneEnd - MobileVirtualFastSceneStart <= 4096
