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
; The completed native page already proves ten complete canonical rectangles.
; Inspect their literal native tops directly, then inspect only overflow
; templates. Do not duplicate thirteen groups into another metadata view.
for raw_slot, OAM_COUNT / 4
	ld a, [wShadowOAM + raw_slot * 16 + 8]
	and a
	jr z, .raw_next{d:raw_slot}
	cp SCREEN_HEIGHT_PX + OAM_Y_OFS
	jr nc, .raw_next{d:raw_slot}
	ld hl, wMobileVirtualLines + 14
	inc [hl]
	and $f
	ld b, a
	ld a, [wMobileVirtualLines + 15]
	cp b
	jr z, .raw_next{d:raw_slot}
	xor a
	ld [wMobileVirtualForceFull], a
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

MobileVirtualAlignedNativePass:
; AlignedScene proves every source is a canonical rectangle. Reuse the same
; immutable descriptor pointer for eligibility, its deadline and admission.
; Irregular scenes continue through the general selector with its larger bound.
	ld a, [wMobileVirtualLength]
	cp OAM_SIZE
	jr nc, .done
	ld a, [wMobileVirtualCursor]
	cp NUM_OBJECT_STRUCTS - 1
	jr nc, .done
	ld b, a
	ld a, [wMobileVirtualNativeRotation]
	inc a
	add b
	cp NUM_OBJECT_STRUCTS - 1
	jr c, .index_ready
	sub NUM_OBJECT_STRUCTS - 1
.index_ready
	inc a
	ld [wMobileVirtualIndex], a
	add a
	add a
	add a
	add LOW(wMobileVirtualDescriptors + 6)
	ld l, a
	ld h, HIGH(wMobileVirtualDescriptors)
	ld a, [hl]
	and $1f
	jr z, .advance
	ld b, 139
	bit 7, [hl]
	jr z, .deadline
	inc b
.deadline
	ldh a, [rLY]
	cp b
	ret nc
	call MobileVirtualAlignedNativeDescriptor
.advance
	ld hl, wMobileVirtualCursor
	inc [hl]
	jr MobileVirtualAlignedNativePass
.done
	xor a
	ld [wMobileVirtualCursor], a
	ld a, 7
	ld [wMobileVirtualStage], a
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
	jr z, .template_color
	inc l
	ld a, [hl]
	ld [wMobileVirtualHeaderOAM], a
	add 8
	ld l, a
	ld h, HIGH(wShadowOAM)
	ld a, [hl]
	ld c, a
	inc l
	inc l
	inc l
	ld a, [hl]
	and OAM_PALETTE
	jr .source_color
.template_color
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
	ld hl, wMobileVirtualBindings
	add hl, de
	ld a, [hl]
	and a
	ret nz
	ld a, [wMobileVirtualIndex]
	and a
	jr nz, .ordinary_color
; Only the local prefix precedes guest color allocation. Protect its exact color.
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
	inc [hl]
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
	ld hl, wMobileVirtualLength
	ld a, [hl]
	add 16
	ld [hl], a
	ret

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
