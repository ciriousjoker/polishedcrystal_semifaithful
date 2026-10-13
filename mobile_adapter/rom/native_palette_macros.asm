MACRO mobile_virtual_capture_kind
; Classify physical world-object sprites without changing their simulation.
; Character and unknown sprites retain NPC priority. The renderer, not maps,
; owns this built-in distinction.
	ld a, [bc]
	cp SPRITE_BALL_CUT_TREE
	jr z, .ground\@
	cp SPRITE_BOULDER_ROCK
	jr z, .ground\@
	cp SPRITE_BLANK_FRUIT
	jr z, .ground\@
	xor a
	jr .kind\@
.ground\@
	ld a, $20
.kind\@
	ld [wMobileVirtualCaptureClass], a
ENDM

ASSERT LOW(wMobileVirtualDescriptors) + NUM_OBJECT_STRUCTS * 8 <= $100

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
	if _NARG >= 3
	jr nc, .rectangle_template\@
	else
	jr nc, .general\@
	endc
	cp 3 * OBJ_SIZE
	if _NARG >= 3
	jr c, .rectangle_template\@
	else
	jr c, .general\@
	endc
	push hl
	ld a, [wMobileVirtualIndex]
	add a
	add a
	add a
	add LOW(wMobileVirtualDescriptors + 6)
	ld l, a
	ld h, HIGH(wMobileVirtualDescriptors)
	ld a, [wMobileVirtualCaptureClass]
	or $c4 ; current native OAM, canonical rectangle, four quads
	ld [hl], a
	inc hl
	ld a, c
	sub 3 * OBJ_SIZE
	ld [hl], a
	pop hl
	if _NARG >= 2
	jp \2 ; the captured four-quad bounds are already proved
	else
	jr .capture_done\@
	endc
	if _NARG >= 3
.rectangle_template\@
; Four quads cannot fit the native page. Capture their immutable header once
; and return directly to native overflow handling, without testing it again.
	push hl
	push bc
	ld a, [wMobileVirtualIndex]
	add a
	add a
	add a
	add LOW(wMobileVirtualDescriptors)
	ld l, a
	ld h, HIGH(wMobileVirtualDescriptors)
	ldh a, [hCurSpriteXPixel]
	ld [hli], a
	ldh a, [hCurSpriteYPixel]
	ld [hli], a
	ldh a, [hCurSpriteTile]
	ld [hli], a
	ld a, d
	ld [hli], a
	ld d, h
	ld e, l
	ld hl, sp + 2
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld bc, MobileCopiedFacingStepDown0 - \1
	add hl, bc
	ld a, l
	ld [de], a
	inc e
	ld a, h
	ld [de], a
	inc e
	ld a, [wMobileVirtualCaptureClass]
	or $44
	ld [de], a
	inc e
	ldh a, [hCurSpriteOAMFlags]
	ld [de], a
	pop bc
	pop hl
	jp \3
	endc
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
	ld h, HIGH(wMobileVirtualDescriptors)
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
	ld a, [wMobileVirtualCaptureClass]
	or b
	ld b, a

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
	if _NARG
	ld bc, MobileCopiedFacingStepDown0 - \1
	else
	ld bc, MobileCopiedFacingStepDown0 - FacingStepDown0
	endc
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
