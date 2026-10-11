; Selected player figures, with separate graphics for each remote peer.
MobileResetAvatarCaches:
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	xor a
	ld [wMobilePeer1 + MOBILE_PEER_LOADED_SPRITE], a
	ld [wMobilePeer2 + MOBILE_PEER_LOADED_SPRITE], a
	inc a
	ld [wMobileGraphicsReady], a
	ldh [rWBK], a ; caller uses bank 1
	ret

MobilePreparePeerGraphics:
; WBK=2, HL=peer, DE=its persistent cache, C=four-tile VRAM0 base.
	ld a, c
	ld [wMobileAvatarTile], a
	push hl
	ld bc, MOBILE_PEER_VISIBLE
	add hl, bc
	ld a, [hl]
	and a
	pop hl
	ret z
	push hl
	push de
	ld bc, 18 ; selected figure
	add hl, bc
	ld a, [hl]
	call MobileAvatarPalette
	pop de
	pop hl
	jp c, .no_palette
	push hl
	ld bc, MOBILE_PEER_PALETTE
	add hl, bc
	ld [hl], a
	pop hl
	push hl
	ld bc, 18
	add hl, bc
	ld a, [hli]
	ld b, a
	add a
	add b
	add a ; figure * NUM_PLAYER_STATES
	add [hl]
	ld c, a
	ld b, 0
	ld hl, PlayerStateSprites
	add hl, bc
	ld a, BANK(PlayerStateSprites)
	call GetFarByte
	pop hl
	push hl
	ld bc, MOBILE_PEER_LOADED_SPRITE
	add hl, bc
	cp [hl]
	jr z, .loaded
	ld [hli], a
	ld [hl], $ff ; force upload even if the new sprite uses the same facing
	dec hl
	push hl
	push de
	farcall GetSprite
	ld h, d
	ld l, e
	pop de
	push de
	ld a, b
	call FarDecompressToDE
	pop de
	pop hl
.loaded
	pop hl
	push hl
	ld bc, 16
	add hl, bc
	ld a, [hl]
	ld b, a
	and $c
	cp $c
	jr nz, .direction
	ld a, 8 ; left mirrors the right-facing source
.direction
	ld c, a
	bit 0, b
	jr z, .standing
	add 12
.standing
	ld b, a ; source tile within the 24-tile decompressed sprite
	pop hl
	push hl
	ld a, b
	ld bc, MOBILE_PEER_LOADED_FRAME
	add hl, bc
	cp [hl]
	jr z, .done
	ld [hl], a
	ld c, a
	ld b, 0
	ld h, b
	ld l, c
rept 4
	add hl, hl
endr
	add hl, de
	ld d, h
	ld e, l
	ldh a, [rVBK]
	push af
	xor a
	ldh [rVBK], a
	ld hl, vTiles0 tile $64
	ld a, [wMobileAvatarTile]
	cp $68
	jr nz, .upload
	ld hl, vTiles0 tile $68
.upload
	lb bc, BANK(MobileCodeStart), 4
	call Get2bpp
	pop af
	ldh [rVBK], a
.done
	pop hl
	ret
.no_palette
	push hl
	ld bc, MOBILE_PEER_VISIBLE
	add hl, bc
	ld [hl], 0
	ld hl, wMobileVisibleCount
	dec [hl]
	ld a, 4
	ld [wMobileDeferred], a
	pop hl
	ret

MobileAvatarPalette:
; A=figure/base palette. Use native dynamic palettes without replacing any NPC.
	ld b, a
	ldh a, [rWBK]
	push af
	ld a, BANK(wUsedObjectPals)
	ldh [rWBK], a
	ld a, b
	ld [wNeededPalIndex], a
	xor a
	ld [wNeededPalType], a
	ld [wNeededObjPalGlow], a
	ld [wPrevNeededObjPalGlow], a
	dec a
	ld [wNeededMonPalLight], a
	ld a, [wUsedObjectPals]
	inc a
	jr nz, .available
; The native allocator falls back to slot 7 when full. Only reuse an exact
; unmodified loaded palette in this case; never recolor a required actor.
	ld hl, wLoadedObjPal0
	ld c, 0
.find
	ld a, c
	cp PAL_OW_WEATHER
	jr z, .next
	ld a, [wNeededPalIndex]
	cp [hl]
	jr nz, .next
	push hl
	ld a, [wLoadedObjPalType]
	ld b, c
	inc b
.type
	dec b
	jr z, .type_ready
	rrca
	jr .type
.type_ready
	and 1
	jr nz, .mismatch
	ld b, 0
	ld hl, wLoadedObjPalGlows
	add hl, bc
	ld a, [hl]
	ld hl, wLoadedObjPalPrevGlows
	add hl, bc
	or [hl]
.mismatch
	pop hl
	jr z, .available
.next
	inc hl
	inc c
	ld a, c
	cp 8
	jr nz, .find
	pop af
	ldh [rWBK], a
	scf
	ret
.available
	ld a, [wNeededPalIndex]
	farcall MarkUsedPal
	ld b, a
	pop af
	ldh [rWBK], a
	ld a, b
	and a
	ret
