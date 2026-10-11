; Peer sprites have their own records and OAM bindings, never NPC structures.
; Standing down/up: VRAM0 64..6b. Standing sides: VRAM1 7c..7f.
; Walking: VRAM0 e0..eb, refreshed after font/menu use. The 15-tile special
; sprites overlap the side reservation and explicitly defer remote rendering.

MobilePrepareSprites::
	call MobileTick
	ldh a, [rWBK]
	push af
	ld a, 1
	ldh [rWBK], a
	ld hl, wMobileSavedFacings
	ld bc, NUM_OBJECT_STRUCTS
	ld a, STANDING
	rst ByteFill
	xor a
	ld [wMobileVisibleCount], a
	ld [wMobileDeferred], a
	ld [wMobileClippedWeather], a
	ld a, [wMobileOnline]
	and a
	jp z, .done
	ldh a, [hInMenu]
	and a
	jp nz, .unsafe
	ld a, [wMobileMenuState]
	and a
	jp nz, .unsafe
	call MobileSceneIsSafe
	jp nz, .unsafe
	ld a, [wMobileAvailability]
	and a
	jp z, .unsafe
	ld a, [wMobileLocalValid]
	and a
	jp z, .unsafe
	ld a, [wMobilePeerCount]
	and a
	jp z, .done
	ld hl, wObjectStructs
	ld de, OBJECT_LENGTH
	ld b, NUM_OBJECT_STRUCTS
.large
	ld a, [hl]
	cp SPRITE_BIG_GYARADOS
	jp z, .critical
	cp SPRITE_ALOLAN_EXEGGUTOR
	jp z, .critical
	add hl, de
	dec b
	jr nz, .large
	ld a, [wMobileGraphicsReady]
	and a
	call z, MobileLoadAvatar
	ld a, [wPlayerPalette]
	and OAM_PALETTE
	ld [wMobileAvatarPalette], a
	call MobileStepProgress
	ld c, d
	ld b, 0
	ld a, [wXCoord]
	call MobileWorldPixel
	ld a, l
	ld [wMobileCameraX], a
	ld a, h
	ld [wMobileCameraX + 1], a
	call MobileStepProgress
	ld c, e
	ld b, 0
	ld a, [wYCoord]
	call MobileWorldPixel
	ld a, l
	ld [wMobileCameraY], a
	ld a, h
	ld [wMobileCameraY + 1], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld hl, wMobilePeer1
	call MobileProjectPeer
	ld a, [wMobilePeerCount]
	cp 2
	jr nz, .budget
	ld hl, wMobilePeer2
	call MobileProjectPeer
.budget
	ld a, 1
	ldh [rWBK], a
	ld a, [wMobileVisibleCount]
	and a
	jp z, .done
	call MobileCountObjectOAM
	call MobileHasOAMSpace
	jp nc, .done
; Suppress only declared distant decoration/trainer rendering for this frame.
	ld bc, wObject1Struct
	ld a, 1
.suppress
	push af
	push bc
	call MobileCanSuppress
	pop bc
	jr c, .next
	ld hl, OBJECT_FACING
	add hl, bc
	ld a, [hl]
	cp STANDING
	jr z, .next
	push hl
	call MobileFacingOAMCount
	ld hl, wMobileOAMDemand
	ld a, [hl]
	sub e
	ld [hl], a
	pop hl
	ld a, [hl]
	ld [hl], STANDING
	ld e, a
	pop af
	push af
	ld hl, wMobileSavedFacings
	ld d, 0
	push af
	ld a, e
	pop de ; D=runtime object index
	ld e, d
	ld d, 0
	add hl, de
	ld [hl], a
	call MobileHasOAMSpace
	jr nc, .rest_done
.next
	ld hl, OBJECT_LENGTH
	add hl, bc
	ld b, h
	ld c, l
	pop af
	inc a
	cp NUM_OBJECT_STRUCTS
	jr nz, .suppress
; Required actors won the budget. Restore all ordinary rendering.
	call MobileRestoreFacings
	xor a
	ld [wMobileVisibleCount], a
	ld a, 1
	jr .defer
.critical
	xor a
	ld [wMobileGraphicsReady], a
	ld a, 2
	jr .defer
.unsafe
	ld a, 3
.defer
	ld [wMobileDeferred], a
.done
	pop af
	ldh [rWBK], a
	ret
.rest_done
	pop af
	jr .done

MobileLoadAvatar:
	ldh a, [rVBK]
	push af
	ld a, SPRITE_CHRIS
	farcall GetSprite
	ld h, d
	ld l, e
	call FarDecompressWRA6InB
	xor a
	ldh [rVBK], a
	ld de, wDecompressScratch
	ld hl, vTiles0 tile $64
	ld c, 8
	call Request2bppInWRA6
	ld a, 1
	ldh [rVBK], a
	ld de, wDecompressScratch + 8 tiles
	ld hl, vTiles0 tile $7c
	ld c, 4
	call Request2bppInWRA6
	xor a
	ldh [rVBK], a
	ld de, wDecompressScratch + 12 tiles
	ld hl, vTiles1 tile $60
	ld c, 12
	call Request2bppInWRA6
	pop af
	ldh [rVBK], a
	ld a, 1
	ld [wMobileGraphicsReady], a
	ret

MobileProjectPeer:
; WBK=2; HL=peer. Map/instance/status checks precede interpolation and projection.
	push hl
	ld bc, MOBILE_PEER_VISIBLE
	add hl, bc
	ld [hl], 0
	pop hl
	push hl
	ld bc, 6
	add hl, bc
	ld a, [wMobileLocalStaging]
	cp [hl]
	jp nz, .hidden
	inc hl
	ld a, [wMobileLocalStaging + 1]
	cp [hl]
	jp nz, .hidden
	inc hl
	ld a, [wMobileInstanceKey]
	cp [hl]
	jp nz, .hidden
	inc hl
	ld a, [wMobileInstanceKey + 1]
	cp [hl]
	jp nz, .hidden
	ld bc, 8 ; peer status at offset 17
	add hl, bc
	ld a, [hl]
	cp 1
	jp nz, .hidden
	pop hl
	push hl
	ld bc, MOBILE_PEER_DRAW_VALID
	add hl, bc
	ld a, [hl]
	and a
	jr nz, .smooth
	ld [hl], 1
	pop hl
	push hl
	ld bc, 12
	add hl, bc
	ld d, h
	ld e, l
	ld bc, MOBILE_PEER_DRAW_X - 12
	add hl, bc
rept 2
	ld a, [de]
	ld b, a
	inc de
	ld a, [de]
	inc de
	ld [hli], a
	ld a, b
	ld [hli], a
endr
	jr .project
.smooth
	pop hl
	push hl
	ld bc, 12
	add hl, bc
	ld d, h
	ld e, l
	push hl
	ld bc, MOBILE_PEER_DRAW_X - 12
	add hl, bc
	call SwapHLDE
	call MobileApproachPixel
	inc hl
	inc hl
	inc de
	inc de
	call MobileApproachPixel
	pop hl
.project
	pop hl
	push hl
	ld bc, MOBILE_PEER_DRAW_X
	add hl, bc
	ld a, [hli]
	ld e, a
	ld d, [hl]
	ld a, [wMobileCameraX]
	ld c, a
	ld a, [wMobileCameraX + 1]
	ld b, a
	ld a, e
	sub c
	ld e, a
	ld a, d
	sbc b
	ld d, a
	ld a, e
	add OAM_X_OFS
	ld e, a
	ld a, d
	adc 0
	and a
	jr nz, .hidden
	ld a, e
	cp SCREEN_WIDTH_PX + OAM_X_OFS
	jr nc, .hidden
	push de
	inc hl
	ld a, [hli]
	ld e, a
	ld d, [hl]
	ld a, [wMobileCameraY]
	ld c, a
	ld a, [wMobileCameraY + 1]
	ld b, a
	ld a, e
	sub c
	ld e, a
	ld a, d
	sbc b
	ld d, a
	ld a, e
	add OAM_Y_OFS - 4
	ld e, a
	ld a, d
	adc 0
	and a
	jr nz, .hidden_y
	ld a, e
	cp SCREEN_HEIGHT_PX + OAM_Y_OFS
	jr nc, .hidden_y
	ld b, e
	pop de
	pop hl
	push hl
	ld a, b
	ld bc, MOBILE_PEER_SCREEN_X
	add hl, bc
	ld [hl], e
	inc hl
	ld [hli], a
	ld [hl], 1
	ld hl, wMobileVisibleCount
	inc [hl]
.hidden
	pop hl
	ret
.hidden_y
	pop de
	jr .hidden

MobileApproachPixel:
; HL=big-endian target, DE=little-endian render coordinate. Preserve pointers.
	push hl
	push de
	ld a, [hli]
	ld b, a
	ld c, [hl]
	ld a, [de]
	ld l, a
	inc de
	ld a, [de]
	ld h, a
	push hl
	ld a, c
	sub l
	ld l, a
	ld a, b
	sbc h
	ld h, a
	or l
	jr z, .snap
	ld a, h
	and a
	jr z, .positive
	cp -1
	jr nz, .snap
	ld a, l
	cp -32
	jr c, .snap
	jr .smooth
.positive
	ld a, l
	cp 33
	jr nc, .snap
.smooth
	sra h
	rr l
	ld a, h
	or l
	jr nz, .add
	inc l ; positive one-pixel residual must make progress
.add
	pop de
	add hl, de
	jr .store
.snap
	pop hl
	ld h, b
	ld l, c
.store
	ld b, h
	ld c, l
	pop de
	ld a, c
	ld [de], a
	inc de
	ld a, b
	ld [de], a
	dec de
	pop hl
	ret

MobileCountObjectOAM:
	xor a
	ld [wMobileOAMDemand], a
	ld bc, wObjectStructs
	ld d, NUM_OBJECT_STRUCTS
.loop
	push de
	ld a, [bc]
	and a
	jr z, .next
	ld hl, OBJECT_FACING
	add hl, bc
	ld a, [hl]
	cp NUM_FACINGS
	jr nc, .next
	call MobileFacingOAMCount
	ld a, [wMobileOAMDemand]
	add e
	ld [wMobileOAMDemand], a
.next
	ld hl, OBJECT_LENGTH
	add hl, bc
	ld b, h
	ld c, l
	pop de
	dec d
	jr nz, .loop
	ret

MobileFacingOAMCount:
; A=facing -> E=OAM bytes. BC preserved; vanilla Facings lives in ROMX.
	push bc
	ld l, a
	ld h, 0
	add hl, hl
	ld bc, Facings
	add hl, bc
	ld a, BANK(Facings)
	call GetFarByte
	inc hl
	ld e, a
	ld a, BANK(Facings)
	call GetFarByte
	ld h, a
	ld a, e
	ld l, a
	ld a, BANK(Facings)
	call GetFarByte
	add a
	add a
	ld e, a
	pop bc
	ret

MobileHasOAMSpace:
	ld a, [wMobileVisibleCount]
	swap a ; four 8x8 sprites = 16 OAM bytes per peer
	ld e, a
	ld a, [wMobileOAMDemand]
	add e
	cp OAM_SIZE + 1
	ccf ; carry means the planned layout does not fit
	ret

MobileCanSuppress:
; A=runtime slot, BC=object. Carry means protected. Restore within two tiles.
	ld hl, OBJECT_MAP_X
	add hl, bc
	ld a, [wPlayerMapX]
	sub [hl]
	add 2
	cp 5
	jr nc, .distant
	inc hl
	ld a, [wPlayerMapY]
	sub [hl]
	add 2
	cp 5
	jr c, .protected
.distant
	ld hl, OBJECT_MAP_OBJECT_INDEX
	add hl, bc
	ld a, [hl]
	and a
	jr z, .protected
	cp NUM_OBJECTS
	jr nc, .protected
	ld e, a
	ld a, [wMapGroup]
	dec a
	ld l, a
	ld h, 0
	add hl, hl
	ld bc, MobileRenderGroupOffsets
	add hl, bc
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld a, [wMapNumber]
	dec a
	ld c, a
	ld b, 0
	add hl, bc
	ld a, [hl]
	ld l, a
	ld h, 0
	ld b, h
	ld c, l
	add hl, hl
	add hl, bc
	ld bc, MobileRenderMasks
	add hl, bc
	ld a, e
	srl a
	srl a
	srl a
	ld c, a
	ld b, 0
	add hl, bc
	ld b, [hl]
	ld a, e
	and 7
	jr z, .bit
.shift
	srl b
	dec a
	jr nz, .shift
.bit
	bit 0, b
	jr z, .protected
	and a
	ret
.protected
	scf
	ret

MobileRestoreFacings:
; WBK=1. Only frame-local facing bytes were suppressed; no story state changed.
	ld bc, wObjectStructs
	ld de, wMobileSavedFacings
.loop
	ld a, [de]
	cp STANDING
	jr z, .next
	ld hl, OBJECT_FACING
	add hl, bc
	ld [hl], a
.next
	inc de
	ld hl, OBJECT_LENGTH
	add hl, bc
	ld b, h
	ld c, l
	ld a, e
	cp LOW(wMobileSavedFacings + NUM_OBJECT_STRUCTS)
	jr nz, .loop
MobileRestoreFacingsEnd::
	ret

MobileRenderSprites::
	ldh a, [rWBK]
	push af
	ld a, 1
	ldh [rWBK], a
	call MobileRestoreFacings
	ld a, [wMobileVisibleCount]
	and a
	jp z, .no_peers
	ld a, [wPlayerCurrentOAMSlot]
	ld [wMobileLocalOAMStart], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld hl, wShadowOAM
	ld de, wMobileOAMSnapshot
	ld bc, OAM_SIZE
	rst CopyBytes
	ld a, [wMobileLocalOAMStart]
	ld e, a
	ld d, 0
	ld hl, wMobileOAMSnapshot
	add hl, de
	ld de, wMobileOAMOutput
	ld bc, 4 * OBJ_SIZE
	rst CopyBytes
	ld a, 4 * OBJ_SIZE
	ld [wMobileOAMWriteCursor], a
	ld hl, wMobilePeer1
	call MobileWritePeerOAM
	ld a, [wMobilePeerCount]
	cp 2
	jr nz, .objects
	ld hl, wMobilePeer2
	call MobileWritePeerOAM
.objects
	ldh a, [hUsedOAMIndex]
	cpl
	add OAM_SIZE + 1
	ld [wMobileOAMReadCursor], a
.copy_object
	ld a, [wMobileOAMReadCursor]
	cp OAM_SIZE
	jr nc, .assembled
	ld b, a
	ld a, [wMobileLocalOAMStart]
	ld c, a
	ld a, b
	sub c
	cp 4 * OBJ_SIZE
	jr c, .skip
	ld e, b
	ld d, 0
	ld hl, wMobileOAMSnapshot
	add hl, de
	ld a, [wMobileOAMWriteCursor]
	ld e, a
	ld d, 0
	push hl
	ld hl, wMobileOAMOutput
	add hl, de
	ld d, h
	ld e, l
	pop hl
	ld bc, OBJ_SIZE
	rst CopyBytes
	ld hl, wMobileOAMWriteCursor
	ld a, [hl]
	add OBJ_SIZE
	ld [hl], a
.skip
	ld hl, wMobileOAMReadCursor
	ld a, [hl]
	add OBJ_SIZE
	ld [hl], a
	jr .copy_object
.assembled
	ld a, [wMobileOAMDemand]
	cp 4 * OBJ_SIZE ; only the local 16x16 avatar, at most six quads per line
	jr z, .publish
	call MobileBudgetNPCScanlines
	jr nc, .publish
; Required NPCs keep the exact vanilla OAM when avatar scanlines cannot fit.
	ld hl, wMobileOAMSnapshot
	ld de, wShadowOAM
	ld bc, OAM_SIZE
	rst CopyBytes
	ld a, 1
	ld [wMobileDeferred], a
	xor a
	ld [wMobileVisibleCount], a
	jp .no_peers
.publish
	ld a, [wMobileOAMWriteCursor]
	ldh [hUsedOAMIndex], a
	ld c, a
	ld b, 0
	cpl
	add OAM_SIZE + 1
	ld [wPlayerCurrentOAMSlot], a
	push bc
	ld b, a
	ld hl, wMobilePeer1
	call MobileBindPeerOAM
	ld hl, wMobilePeer2
	call MobileBindPeerOAM
	ld a, b
	pop bc
	ld e, a
	ld d, 0
	ld hl, wShadowOAM
	add hl, de
	ld d, h
	ld e, l
	ld hl, wMobileOAMOutput
	rst CopyBytes
	ld a, [wMobileVisibleCount]
	ld [wMobileRenderedCount], a
	call MobileBudgetWeather
.done
	pop af
	ldh [rWBK], a
	jmp MobileTick
.no_peers
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld hl, wMobilePeer1Visible
	ld [hl], 0
	inc hl
	ld [hl], OAM_SIZE
	ld hl, wMobilePeer2Visible
	ld [hl], 0
	inc hl
	ld [hl], OAM_SIZE
	xor a
	ld [wMobileRenderedCount], a
	jr .done

MobileBindPeerOAM:
; B=final block base; never publish an intermediate relative OAM offset.
	ld de, MOBILE_PEER_VISIBLE
	add hl, de
	ld a, [hli]
	and a
	jr z, .hidden
	inc hl
	ld a, [hld]
	add b
	ld [hl], a
	ret
.hidden
	ld [hl], OAM_SIZE
	ret

MobileWritePeerOAM:
	push hl
	ld bc, MOBILE_PEER_VISIBLE
	add hl, bc
	ld a, [hl]
	and a
	jp z, .hidden
	inc hl
	inc hl ; pending binding, publish only after the layout is assembled
	ld a, [wMobileOAMWriteCursor]
	ld [hl], a
	pop hl
	push hl
	ld bc, MOBILE_PEER_SCREEN_X
	add hl, bc
	ld a, [hli]
	ldh [hCurSpriteXPixel], a
	ld a, [hl]
	ldh [hCurSpriteYPixel], a
	pop hl
	push hl
	ld bc, 16 ; facing/animation
	add hl, bc
	ld l, [hl]
	ld h, 0
	add hl, hl
	ld bc, MobileSocialFacings
	add hl, bc
	ld a, [hli]
	ld h, [hl]
	ld l, a
	inc hl ; approved facings always contain exactly four quads
	ld a, [wMobileOAMWriteCursor]
	ld c, a
	ld b, 0
	push hl
	ld hl, wMobileOAMOutput
	add hl, bc
	ld d, h
	ld e, l
	pop hl
	ld b, 4
.quad
	ldh a, [hCurSpriteYPixel]
	add [hl]
	inc hl
	ld [de], a
	inc de
	ldh a, [hCurSpriteXPixel]
	add [hl]
	inc hl
	ld [de], a
	inc de
	ld a, [hli]
	and OAM_XFLIP | OAM_YFLIP | OAM_PRIO
	ld c, a
	ld a, [hli]
	bit 7, a
	jr nz, .walk
	cp 8
	jr nc, .side
	add $64
	jr .tile
.side
	add $7c - 8
	set B_OAM_BANK1, c
	jr .tile
.walk
	add $60 ; raw 80..8b becomes e0..eb in VRAM0
.tile
	ld [de], a
	inc de
	ld a, [wMobileAvatarPalette]
	or c
	ld [de], a
	inc de
	dec b
	jr nz, .quad
	ld hl, wMobileOAMWriteCursor
	ld a, [hl]
	add 4 * OBJ_SIZE
	ld [hl], a
.hidden
	pop hl
	ret

MobileBudgetWeather:
; Preserve local+remote scanline capacity ahead of lower-priority NPCs. Weather
; occupies the OAM prefix and therefore must not crowd out the avatar block.
	ldh a, [hUsedWeatherSpriteIndex]
	and a
	ret z
	ld hl, wMobileScanlineBudget
	ld bc, SCREEN_HEIGHT_PX
	xor a
	rst ByteFill
	ld a, [wPlayerCurrentOAMSlot]
	ld [wMobileOAMReadCursor], a
	ld b, a
	ld a, [wMobileVisibleCount]
	inc a ; local player also has four sprites
	swap a
	add b
	ld [wMobileOAMWriteCursor], a
.avatar
	call MobileAddScanlineOAM
	ld a, [wMobileOAMReadCursor]
	add OBJ_SIZE
	ld [wMobileOAMReadCursor], a
	ld hl, wMobileOAMWriteCursor
	cp [hl]
	jr c, .avatar
	xor a
	ld [wMobileOAMReadCursor], a
.weather
	ld a, [wPlayerCurrentOAMSlot]
	ld hl, wMobileOAMReadCursor
	cp [hl]
	ret z
	call MobileCheckScanlineOAM
	jr nc, .keep
	ld a, [wMobileOAMReadCursor]
	ld l, a
	ld h, HIGH(wShadowOAM)
	ld [hl], OAM_YCOORD_HIDDEN
	ld hl, wMobileClippedWeather
	inc [hl]
	jr .next
.keep
	call MobileAddScanlineOAM
.next
	ld a, [wMobileOAMReadCursor]
	add OBJ_SIZE
	ld [wMobileOAMReadCursor], a
	jr .weather

MobileBudgetNPCScanlines:
; Reconstruct vanilla's reverse priority order to identify optional NPC quads.
; No object data changes. A scanline sweep costs two deltas per hardware sprite.
	ld hl, wMobileNPCOptional
	ld bc, OAM_COUNT
	xor a
	rst ByteFill
	ld a, [wMobileVisibleCount]
	inc a
	swap a
	ld [wMobileBudgetCursor], a
	ld a, $30 ; high, normal, then low, matching InitSprites' reverse OAM
	ld [wMobileBudgetPriority], a
.priority
	ld a, NUM_OBJECT_STRUCTS - 1
	ld [wMobileBudgetIndex], a
.object
	ld a, 1
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, wObjectPriorities
	add hl, de
	ld a, [hl]
	ld e, a
	and $f0
	ld hl, wMobileBudgetPriority
	cp [hl]
	jr nz, .next_object
	ld a, e
	and $f
	jr z, .next_object ; local player already has its own leading OAM block
	ld hl, wObjectStructs
	ld de, OBJECT_LENGTH
.pointer
	add hl, de
	dec a
	jr nz, .pointer
	ld b, h
	ld c, l
	push bc
	call MobileCanSuppress
	ld a, 0
	jr c, .protected
	inc a
.protected
	ld [wMobileBudgetOptional], a
	pop bc
	ld hl, OBJECT_FACING
	add hl, bc
	ld a, [hl]
	call MobileFacingOAMCount
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
.classify
	ld a, [wMobileBudgetCursor]
	cp OAM_SIZE
	jr nc, .invalid
	ld l, a
	ld h, 0
	srl l
	srl l
	ld bc, wMobileNPCOptional
	add hl, bc
	ld a, [wMobileBudgetOptional]
	ld [hl], a
	ld hl, wMobileBudgetCursor
	ld a, [hl]
	add OBJ_SIZE
	ld [hl], a
	ld a, e
	sub OBJ_SIZE
	ld e, a
	jr nz, .classify
.next_object
	ld hl, wMobileBudgetIndex
	dec [hl]
	ld a, [hl]
	cp $ff
	jr nz, .object
	ld hl, wMobileBudgetPriority
	ld a, [hl]
	sub $10
	ld [hl], a
	jr nz, .priority
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetCursor]
	ld hl, wMobileOAMWriteCursor
	cp [hl]
	jr nz, .invalid
	call MobileCheckNPCScanlines
	ret nc
; Hide entire optional actors, including all their quads, for this frame only.
	ld hl, wMobileNPCOptional
	ld de, wMobileOAMOutput
	ld b, OAM_COUNT
.hide_optional
	ld a, [hli]
	and a
	jr z, .keep
	xor a
	ld [de], a
.keep
	inc de
	inc de
	inc de
	inc de
	dec b
	jr nz, .hide_optional
	jmp MobileCheckNPCScanlines
.invalid
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	scf
	ret

MobileCheckNPCScanlines:
	ld hl, wMobileScanlineBudget
	ld bc, SCREEN_HEIGHT_PX + 1
	xor a
	rst ByteFill
	ld [wMobileBudgetCursor], a
.quad
	ld a, [wMobileBudgetCursor]
	ld l, a
	ld h, 0
	ld bc, wMobileOAMOutput
	add hl, bc
	ld a, [hl]
	cp OAM_Y_OFS - TILE_WIDTH + 1
	jr c, .next_quad
	cp SCREEN_HEIGHT_PX + OAM_Y_OFS
	jr nc, .next_quad
	ld e, a
	sub OAM_Y_OFS
	jr nc, .start
	xor a
.start
	ld l, a
	ld h, 0
	ld bc, wMobileScanlineBudget
	add hl, bc
	inc [hl]
	ld a, e
	sub OAM_Y_OFS - TILE_WIDTH
	cp SCREEN_HEIGHT_PX
	jr c, .end
	ld a, SCREEN_HEIGHT_PX
.end
	ld l, a
	ld h, 0
	add hl, bc
	dec [hl]
.next_quad
	ld hl, wMobileBudgetCursor
	ld a, [hl]
	add OBJ_SIZE
	ld [hl], a
	ld hl, wMobileOAMWriteCursor
	cp [hl]
	jr c, .quad
	ld hl, wMobileScanlineBudget
	ld b, 0
	ld c, SCREEN_HEIGHT_PX
.sweep
	ld a, [hli]
	add b
	ld b, a
	cp 11
	jr nc, .overflow
	dec c
	jr nz, .sweep
	and a
	ret
.overflow
	scf
	ret

MobileScanlineRange:
	ld a, [wMobileOAMReadCursor]
	ld l, a
	ld h, HIGH(wShadowOAM)
	ld a, [hl]
	sub OAM_Y_OFS
	ld e, a
	ld d, 0
	ld b, TILE_WIDTH
	ret
MobileAddScanlineOAM:
	call MobileScanlineRange
.line
	ld a, e
	cp SCREEN_HEIGHT_PX
	jr nc, .next
	ld hl, wMobileScanlineBudget
	add hl, de
	inc [hl]
.next
	inc e
	dec b
	jr nz, .line
	ret
MobileCheckScanlineOAM:
	call MobileScanlineRange
.line
	ld a, e
	cp SCREEN_HEIGHT_PX
	jr nc, .next
	ld hl, wMobileScanlineBudget
	add hl, de
	ld a, [hl]
	cp 10
	jr nc, .full
.next
	inc e
	dec b
	jr nz, .line
	and a
	ret
.full
	scf
	ret

INCLUDE "mobile_adapter/rom/replaceable.asm"

; Own small copies of the sixteen approved avatar facings. Budget checks for
; ordinary NPCs still read their actual banked tables through GetFarByte.
MobileSocialFacings:
for facing, 0, 16
	dw .Frame{d:facing}
endr
for facing, 0, 16
.Frame{d:facing}
	db 4
	REDEF mobile_tile = (facing / 4) * 4
	REDEF mobile_flip = 0
	if facing >= 12
		REDEF mobile_tile = 8
		REDEF mobile_flip = OAM_XFLIP
	elif facing % 4 == 3 && facing < 8
		REDEF mobile_flip = OAM_XFLIP
	endc
	if facing % 2
		REDEF mobile_tile = mobile_tile + $80
	endc
	for quad, 0, 4
		db (quad / 2) * 8, ((quad % 2) ^ !!mobile_flip) * 8, mobile_flip, mobile_tile + quad
	endr
endr
