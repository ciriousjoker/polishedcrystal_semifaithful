MACRO mobile_virtual_capture
; Immutable render metadata only. Reuse the native ShadowOAM group whenever
; it fits; a ROM template is retained for actors beyond vanilla OAM capacity.
	ld a, [wMobileVirtualStage]
	and a
	jp z, .capture_done\@
; The emitted facing contract proves every ordinary four-quad figure is a
; rectangle. Its completed native OAM needs only a count and start offset.
; Preserve BC/DE directly instead of constructing the general template path.
	ldh a, [hUsedSpriteTile]
	cp 4
	jr nz, .general\@
	ld a, c
	cp OAM_SIZE - OBJ_SIZE + 1
	jr nc, .general\@
	cp 3 * OBJ_SIZE
	jr c, .general\@
	push hl
	ld a, [wMobileVirtualIndex]
	add a
	add a
	add a
	add LOW(wMobileVirtualDescriptors + 6)
	ld l, a
	adc HIGH(wMobileVirtualDescriptors + 6)
	sub l
	ld h, a
	ld [hl], $c4 ; current native OAM, canonical rectangle, four quads
	inc hl
	ld a, c
	sub 3 * OBJ_SIZE
	ld [hl], a
	pop hl
	jr .capture_done\@
.general\@
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
	ld h, [hl]
	ld l, a
	ld bc, MobileCopiedFacingStepDown0 - FacingStepDown0
	add hl, bc
	ld a, l
	ld [de], a
	inc de
	ld a, h
	ld [de], a

.restore\@
	pop hl
	pop de
	pop bc
.capture_done\@
	ldh a, [hUsedSpriteTile]
ENDM

MACRO mobile_virtual_bin_coordinate
; Shift the conservative buckets with the current native scene. Clamp a clipped
; top edge instead of wrapping an unsigned coordinate into another counter.
	ld e, a
	ld a, [wMobileVirtualLines + 15]
	ld d, a
	ld a, e
	sub d
	jr nc, :+
	xor a
:
ENDM

; This six-byte resident integration avoids the general farcall's stack work
; for a validated rectangle. Carry selects private output, otherwise native.
; Both upper programs return via Bankswitch.
MACRO mobile_native_rectangle_trampoline
MobileNativeRectangleTrampoline::
	ld a, BANK(MobileVirtualWriteRectangleDispatch)
	rst Bankswitch
	jp MobileVirtualWriteRectangleDispatch
ENDM
