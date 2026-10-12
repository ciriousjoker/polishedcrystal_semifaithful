; Preserve the native scan registers; substitute only a still-temporary lease.
; A compact unbanked owner table avoids bank-switch calls for all ordinary NPCs
; during the per-frame collision/glow scan.
MACRO mobile_native_palette_source
if !DEF(MOBILE_VIRTUAL_ACTORS)
	push af
	push hl
	ld h, d
	ld l, e
	inc hl ; OBJECT_MAP_OBJECT_INDEX
	ld a, [hl]
	cp TEMP_OBJECT
	jr nz, .done\@
	ld a, NUM_OBJECT_STRUCTS
	sub b
	add LOW(wMobileNativePaletteOwners)
	ld l, a
	ld h, HIGH(wMobileNativePaletteOwners)
	ld a, [hl]
	and a
	jr z, .done\@
	cp MOBILE_PEERS + 1
	jr nc, .done\@
	ld h, d
	ld l, e
	rept OBJECT_FLAGS2
	inc hl
	endr
	ld a, [hl]
	and HIGH_PRIORITY | FROZEN
	cp HIGH_PRIORITY | FROZEN
	jr nz, .done\@
	ld a, NUM_OBJECT_STRUCTS
	sub b
	add LOW(wMobileNativePaletteOwners)
	ld l, a
	ld h, HIGH(wMobileNativePaletteOwners)
	ld a, [hl]
	dec a
	ld l, a
	add a
	add LOW(wMobileNativeOriginals)
	ld e, a
	ld a, l
	swap a
	add a
	add e
	ld e, a
	ld d, HIGH(wMobileNativeOriginals)
.done\@
	pop hl
	pop af
endc
ENDM

MACRO mobile_virtual_capture
; Immutable render metadata only. Reuse the native ShadowOAM group whenever
; it fits; a ROM template is retained for actors beyond vanilla OAM capacity.
	ld a, [wMobileVirtualStage]
	and a
	jr z, .capture_done\@
	push bc
	push de
	push hl
	ld a, c
	ld [wMobileVirtualHeaderOAM], a
	ld b, h
	ld c, l
	ld e, d
	ld a, [wMobileVirtualIndex]
	add a
	add a
	add a
	add LOW(wMobileVirtualDescriptors)
	ld l, a
	adc HIGH(wMobileVirtualDescriptors)
	sub l
	ld h, a
	ldh a, [hCurSpriteXPixel]
	ld [hli], a
	ldh a, [hCurSpriteYPixel]
	ld [hli], a
	ldh a, [hCurSpriteTile]
	ld [hli], a
	ld [hl], e
	inc hl
	ld [hl], c
	inc hl
	ld [hl], b
	inc hl
	ldh a, [hUsedSpriteTile]
	ld b, a
; All native four-quad facings are canonical two-column rectangles. The host
; contract checks the emitted facing data, including every future addition.
	ld a, b
	cp 4
	jr nz, .shape_ready\@
	set 6, b
.shape_ready\@

	ld a, b
	and $1f
	add a
	add a
	sub OBJ_SIZE
	ld c, a
	ld a, [wMobileVirtualHeaderOAM]
	cp OAM_SIZE - OBJ_SIZE + 1
	jr nc, .template\@
	cp c
	jr c, .template\@
	sub c
	ld c, a
	ld a, b
	or $80
	ld [hli], a
	ld [hl], c
	jr .restore\@
.template\@
	ld [hl], b
	inc hl
	ldh a, [hCurSpriteOAMFlags]
	ld [hl], a
.restore\@
	pop hl
	pop de
	pop bc
.capture_done\@
	ldh a, [hUsedSpriteTile]
ENDM
