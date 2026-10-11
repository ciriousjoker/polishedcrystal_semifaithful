; Selected player figures, with separate graphics for each remote peer.
MobileResetAvatarCaches:
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, $ff ; preserve persistent decompressed graphics across menus/maps
	ld [wMobilePeer1 + MOBILE_PEER_LOADED_FRAME], a
	ld [wMobilePeer2 + MOBILE_PEER_LOADED_FRAME], a
	xor a
	ld [wMobileScanlineValid], a
	call MobileRenderMaskPointer
	ld de, wMobileRenderMapMask
	ld bc, 3
	rst CopyBytes
	ld a, 1
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
	ld bc, MOBILE_PEER_PALETTE
	add hl, bc
	ld c, [hl]
	ld de, 18 - MOBILE_PEER_PALETTE
	add hl, de
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
	ld b, a
	ldh a, [hVBlankCounter]
	ld c, a
	ld a, [wMobileColdFrame]
	cp c
	jp z, .cold_later
	ld a, c
	ld [wMobileColdFrame], a
	ld a, b
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
	push af ; publish the loaded frame only after the VRAM copy completes
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
; Four HBlank lines must fit before the native LY136 cutoff. A short direct
; copy also fits early VBlank. Never let object rendering wait another frame.
	di
	ldh a, [rLY]
	cp 128
	jr c, .hblank
	cp 144
	jr c, .later
	cp 149
	jr nc, .later
	call Copy2bpp
	jr .copied
.hblank
	call Get2bpp
.copied
	pop af
	ldh [rVBK], a
	pop af
	pop hl
	push hl
	ld bc, MOBILE_PEER_LOADED_FRAME
	add hl, bc
	ld [hl], a
.done
	pop hl
	ret
.later
	ei
	pop af
	ldh [rVBK], a
	pop af
	pop hl
	push hl
	ld bc, MOBILE_PEER_LOADED_FRAME
	add hl, bc
	ld a, [hl]
	cp $ff
	jr nz, .done
	pop hl
	ld a, 5
	jr .hide
.cold_later
	pop hl
	ld a, 5 ; at most one cold decompression per frame; keep ordinary game work
	jr .hide
.no_palette
	ld a, 4
.hide
	ld [wMobileDeferred], a
	push hl
	ld bc, MOBILE_PEER_VISIBLE
	add hl, bc
	ld [hl], 0
	ld hl, wMobileVisibleCount
	dec [hl]
	pop hl
	ret

MobileAvatarPalette:
; A=figure/base palette, C=previous hardware slot. Reuse an exact unchanged
; native palette directly; allocation/recoloring still uses the native handler.
	ld b, a
	ldh a, [rWBK]
	push af
	ld a, BANK(wUsedObjectPals)
	ldh [rWBK], a
	ld a, c
	cp 8
	jr nc, .allocate
	cp PAL_OW_WEATHER
	jr z, .allocate
	ld hl, wLoadedObjPal0
	ld e, c
	ld d, 0
	add hl, de
	ld a, b
	cp [hl]
	jr nz, .allocate
	ld hl, wLoadedObjPalGlows
	add hl, de
	ld a, [hl]
	ld hl, wLoadedObjPalPrevGlows
	add hl, de
	or [hl]
	jr nz, .allocate
	ld a, [wLoadedObjPalType]
	push bc
	ld b, c
	inc b
.type_cached
	dec b
	jr z, .type_cached_ready
	rrca
	jr .type_cached
.type_cached_ready
	and 1
	pop bc
	jr nz, .allocate
	push bc
	ld a, 1
	ld b, c
	inc b
.used_cached
	dec b
	jr z, .used_cached_ready
	add a
	jr .used_cached
.used_cached_ready
	ld hl, wUsedObjectPals
	or [hl]
	ld [hl], a
	pop bc
	pop af
	ldh [rWBK], a
	ld a, c
	and a
	ret
.allocate
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
