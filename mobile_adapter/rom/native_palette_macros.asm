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
	ld a, [wMobileVirtualIndex]
	add a
	add a
	add a
	add LOW(wMobileVirtualDescriptors + 6)
	ld l, a
	adc HIGH(wMobileVirtualDescriptors + 6)
	sub l
	ld h, a
	ldh a, [hUsedSpriteTile]
	ld b, a
; All native four-quad facings are canonical two-column rectangles. The host
; contract checks the emitted facing data, including every future addition.
	ld a, b
	cp 4
	jr z, .rectangle\@
	xor a
	ld [wMobileVirtualFastAdmission], a
	jr .shape_ready\@
.rectangle\@
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
; Only a group that does not fit native OAM needs the materialization template.
; Raw groups retain just their current count and ShadowOAM offset.
	ld de, -7
	add hl, de
	ldh a, [hCurSpriteXPixel]
	ld [hli], a
	ldh a, [hCurSpriteYPixel]
	ld [hli], a
	ldh a, [hCurSpriteTile]
	ld [hli], a
	ld d, h
	ld e, l
	ld hl, sp + 3 ; saved native D contains the complete palette/bank flags
	ld a, [hl]
	ld [de], a
	inc de
	ld hl, sp + 0 ; saved native HL is the original facing ROM template
	ld a, [hli]
	ld [de], a
	inc de
	ld a, [hl]
	ld [de], a
.restore\@
	pop hl
	pop de
	pop bc
.capture_done\@
	ldh a, [hUsedSpriteTile]
ENDM
