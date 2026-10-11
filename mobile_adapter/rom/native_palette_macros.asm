; Preserve the native scan registers; substitute only a still-temporary lease.
; A compact unbanked owner table avoids bank-switch calls for all ordinary NPCs
; during the per-frame collision/glow scan.
MACRO mobile_native_palette_source
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
ENDM
