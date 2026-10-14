; Immutable14/17 general emitter, bounded to the existing transient group.
MobileVirtualEmitImmutableGroup::
 ld a, [wMobileVirtualTemplate + 6]
 bit 7, a
 jp z, .reject
 ld a, [wMobileVirtualTemplate + 4]
 cp NUM_FACINGS
 jp nc, .reject
 ld l, a
 ld h, 0
 add hl, hl
 ld bc, MobileCopiedFacings
 add hl, bc
 ld a, [hli]
 ld h, [hl]
 ld l, a
 ld a, [hli]
 and a
 jp z, .reject
 cp 17
 jp nc, .reject
 ld e, a
 ld a, [wMobileVirtualTemplate + 6]
 and $1f
 cp e
 jp nz, .reject
 ld a, e
 add a
 add a
 ld e, a
 ld a, [wMobileVirtualTemplate + 7]
 cp $ff
 jr z, .source_mode_ok
 and 3
 jp nz, .reject
 ld a, [wMobileVirtualTemplate + 7]
 add e
 jp c, .reject
 cp 161
 jp nc, .reject
.source_mode_ok
 ld a, e
 ld [wMobileVirtualGroupLength], a
 add LOW(wMobileVirtualGroup) - 4
 ld c, a
 ld b, HIGH(wMobileVirtualGroup)
 ldh a, [$ff70]
 push af
 ld a, 5
 ldh [$ff70], a
 ldh a, [hUsedOAMIndex]
 push af
 ldh a, [hUsedSpriteTile]
 push af
 ldh a, [hCurSpriteTile]
 push af
 ldh a, [hCurSpriteOAMFlags]
 push af
 ldh a, [hCurSpriteXPixel]
 push af
 ldh a, [hCurSpriteYPixel]
 push af
 ld a, [wMobileVirtualTemplate + 6]
 and $1f
 ldh [hUsedSpriteTile], a
 ld a, [wMobileVirtualTemplate + 1]
 ldh [hCurSpriteYPixel], a
 ld a, [wMobileVirtualTemplate + 0]
 ldh [hCurSpriteXPixel], a
 ld a, [wMobileVirtualTemplate + 2]
 ldh [hCurSpriteTile], a
 ld a, [wMobileVirtualTemplate + 5]
 ldh [hCurSpriteOAMFlags], a
 ld a, [wMobileVirtualTemplate + 3]
 ld d, a
.addsprite
	ldh a, [hCurSpriteYPixel]
	add [hl]
	inc hl
	ld [bc], a
	inc c
	ldh a, [hCurSpriteXPixel]
	add [hl]
	inc hl
	ld [bc], a
	inc c
	ld a, [hli]
	ld e, a
	ldh a, [hCurSpriteTile]
	bit ABSOLUTE_TILE_ID_F, e
	jr z, .nope1
	xor a
.nope1
	add [hl]
	inc hl
	ld [bc], a
	ld a, d
	and OAM_BANK1
	jr z, .vram0
	ld a, [bc]
	cp $80
	jr c, .standing
	sub $40
	ld [bc], a
.standing
.vram0
	inc c
	xor a
	bit RELATIVE_ATTRIBUTES_F, e
	jr z, .nope2
	ldh a, [hCurSpriteOAMFlags]
.nope2
	or e
	and OAM_XFLIP | OAM_YFLIP | OAM_PRIO
	or d
	bit NEXT_PALETTE_F, e
	jr z, .nope3
	inc a
.nope3
	ld [bc], a
	inc c
	ld a, c
	sub OBJ_SIZE * 2
	ld c, a
	ldh a, [hUsedSpriteTile]
	dec a
	ldh [hUsedSpriteTile], a
	jr nz, .addsprite
.loopEnd:
 pop af
 ldh [hCurSpriteYPixel], a
 pop af
 ldh [hCurSpriteXPixel], a
 pop af
 ldh [hCurSpriteOAMFlags], a
 pop af
 ldh [hCurSpriteTile], a
 pop af
 ldh [hUsedSpriteTile], a
 pop af
 ldh [hUsedOAMIndex], a
 pop af
 ldh [$ff70], a
 ld a, [wMobileVirtualTemplate + 6]
 and $1f
 add a
 add a
 ld [wMobileVirtualGroupLength], a
 ld a, 1
 ld [wMobileVirtualRejected], a
 ret
.reject
 ld a, 2
 ld [wMobileVirtualRejected], a
 ret
MobileVirtualEmitImmutableGroupEnd::
