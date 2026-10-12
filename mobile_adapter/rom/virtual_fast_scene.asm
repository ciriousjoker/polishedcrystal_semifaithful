; Exact aligned-scene admission for a single guest and ordinary rectangles.
SECTION "Mobile Virtual Fast Scene", ROMX, BANK[MOBILE_ROM_BANK]
MobileVirtualFastSceneStart::

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
	ld a, 1
	ld [wMobileVirtualForceFull], a
for native_slot, NUM_OBJECT_STRUCTS
	xor a
	ld [wMobileVirtualNativeRaw + native_slot * 4], a
	ld a, [wMobileVirtualDescriptors + native_slot * 8 + 6]
	and a
	jr z, .next{d:native_slot}
	bit 7, a
	jr z, .template{d:native_slot}
	ld a, [wMobileVirtualDescriptors + native_slot * 8 + 7]
	ld [wMobileVirtualNativeRaw + native_slot * 4 + 2], a
	ld l, a
	ld h, HIGH(wShadowOAM)
	ld de, 8
	add hl, de
	ld a, [hl]
	ld c, a
	inc l
	inc l
	inc l
	ld a, [hl]
	and OAM_PALETTE
	ld [wMobileVirtualNativeRaw + native_slot * 4 + 1], a
	ld a, c
	jr .top{d:native_slot}
.template{d:native_slot}
	ld a, 255
	ld [wMobileVirtualNativeRaw + native_slot * 4 + 2], a
; Every canonical native facing has the same palette for all four quadrants,
; with no NEXT_PALETTE flag. The compiled-data host contract proves this.
	ld a, [wMobileVirtualDescriptors + native_slot * 8 + 3]
	and OAM_PALETTE
	ld [wMobileVirtualNativeRaw + native_slot * 4 + 1], a
	ld a, [wMobileVirtualDescriptors + native_slot * 8 + 1]
.top{d:native_slot}
	and a
	jr z, .next{d:native_slot}
	cp SCREEN_HEIGHT_PX + OAM_Y_OFS
	jr nc, .next{d:native_slot}
	ld [wMobileVirtualNativeRaw + native_slot * 4], a
	ld hl, wMobileVirtualLines + 14
	inc [hl]
	and $f
	ld b, a
	ld a, [wMobileVirtualLines + 15]
	cp b
	jr z, .next{d:native_slot}
	xor a
	ld [wMobileVirtualForceFull], a
.next{d:native_slot}
endr
	ld a, [wMobileVirtualLines + 14]
	cp OAM_COUNT / 4 + 1
	ret nc
	xor a
	ld [wMobileVirtualForceFull], a
	ret

MobileVirtualAlignedNativeGroup:
; A ordinary slot in a proven aligned scene. Read its metadata once, admit
; before constructing any quads, and preserve already generated native OAM.
	ld [wMobileVirtualIndex], a
	and a
	ld a, 255
	jr nz, .owner
	ld a, MOBILE_PEERS + 2
.owner
	ld [wMobileVirtualGroupOwner], a
	ld a, [wMobileVirtualLength]
	cp OAM_SIZE - 16 + 1
	ret nc
	ld a, [wMobileVirtualIndex]
	add a
	add a
	add LOW(wMobileVirtualNativeRaw)
	ld l, a
	adc HIGH(wMobileVirtualNativeRaw)
	sub l
	ld h, a
	ld a, [hli]
	and a
	ret z
	ld [wMobileVirtualRectangleTop], a
	ld a, [hli]
	ld [wMobileVirtualGroupPalette], a
	ld e, a
	ld d, 0
	ld a, [hl]
	ld [wMobileVirtualHeaderOAM], a
	ld hl, wMobileVirtualBindings
	add hl, de
	ld a, [hl]
	and a
	ret nz
; Native aligned tops occupy exactly one visible bucket. The guest may span
; two and keeps the general aligned check. Length already bounds total OAM.
	ld a, [wMobileVirtualRectangleTop]
	swap a
	and $f
	ld e, a
	ld hl, wMobileVirtualLines + 16
	add hl, de
	ld a, [hl]
	cp 5
	ret nc
	inc [hl]
	ld hl, wMobileVirtualRectangleCount
	inc [hl]
	ld a, [wMobileVirtualHeaderOAM]
	cp 255
	jr z, .template
	ld [wMobileVirtualGroupPointer], a
	ld a, [wMobileVirtualGroupPalette]
	ld e, a
	ld d, 0
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [wMobileVirtualPaletteUsed]
	or [hl]
	ld [wMobileVirtualPaletteUsed], a
	jp MobileVirtualCopyRawRectangle
.template
	ld a, [wMobileVirtualIndex]
	add a
	add a
	add a
	add LOW(wMobileVirtualDescriptors)
	ld l, a
	adc HIGH(wMobileVirtualDescriptors)
	sub l
	ld h, a
	ld de, wMobileVirtualTemplate
	ld bc, 8
	rst CopyBytes
	ld a, 16
	ld [wMobileVirtualGroupLength], a
	ld a, [wMobileVirtualLength]
	add LOW(wMobileVirtualOutput)
	ld e, a
	adc HIGH(wMobileVirtualOutput)
	sub e
	ld d, a
	ld a, e
	ld [wMobileVirtualGroupPointer], a
	ld a, d
	ld [wMobileVirtualGroupPointer + 1], a
	ld hl, wMobileVirtualTemplate + 4
	ld a, [hli]
	ld h, [hl]
	ld l, a
	scf
	call MobileNativeRectangleTrampoline
	jp MobileVirtualTemplateRectangleDone

MobileVirtualAdmitAlignedRectangle:
; Exact only under AlignedScene's proof. Check both touched bins before
; changing either. The clipped bottom bin never consumes invisible scanlines.
	ld a, [wMobileVirtualRectangleCount]
	cp OAM_COUNT / 4
	jr nc, .rejected
	ld a, [wMobileVirtualRectangleTop]
	ld c, a
	and a
	jr z, .record
	cp SCREEN_HEIGHT_PX + OAM_Y_OFS
	jr nc, .record
	ld a, [wMobileVirtualLines + 15]
	ld b, a
	ld a, c
	sub b
	jr nc, .coordinate
	xor a
.coordinate
	ld b, a
	swap a
	and $f
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualLines + 16
	add hl, de
	ld a, [hl]
	cp 5
	jr nc, .rejected
	ld a, e
	cp 9
	jr nc, .one_bin
	ld a, b
	and $f
	jr z, .one_bin
	inc hl
	ld a, [hl]
	cp 5
	jr nc, .rejected
	inc [hl]
	dec hl
.one_bin
	inc [hl]
.record
	ld a, [wMobileVirtualRectangleCount]
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualLines
	add hl, de
	ld [hl], c
	ld hl, wMobileVirtualRectangleCount
	inc [hl]
	and a
	ret
.rejected
	scf
	ret

MobileVirtualFastSceneEnd::
ASSERT MobileVirtualFastSceneEnd - MobileVirtualFastSceneStart <= 4096
