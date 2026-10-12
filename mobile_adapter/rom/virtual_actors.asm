; Independent peer presentation. Native objects are read-only inputs.
MobileVirtualActorVersion:: db 1
MobileNativeActorStep::
MobilePrepareSprites::
	and a
	ret

MobileNativeWait::
; Never enter HALT after background work has already received the awaited
; VBlank. Preserve native registers and flags across this replacement wait.
	push af
	push bc
.again
	ldh a, [hVBlankOccurred]
	and a
	jr nz, .done ; the native caller owns the frame as soon as its wait is complete
	ldh a, [rLY]
	cp 136
	jr nc, .sleep ; do not even enter a background job near the awaited VBlank
	call MobileNativeIdle
	ldh a, [hVBlankOccurred]
	and a
	jr nz, .done
	ldh a, [rLY]
	cp 136
	jr nc, .sleep
	ld a, [wMobileOwner]
	and a
	jr z, .deadline
	ldh a, [rIE]
	or IE_SERIAL
	ldh [rIE], a
.deadline
	ld a, [wMobileVirtualStage]
	and a
	jr nz, .retry
	ldh a, [rLY]
	cp 116
	jr c, .retry
	ld a, [wMobileWire]
	and a
	jr z, .sleep
	cp MOBILE_WIRE_DONE
	jr nc, .sleep
.retry
	ld a, [wMobileVirtualStage]
	and a
	jr nz, .again
	ld a, [wMobileOwner]
	ld b, a
	ld a, [wMobileWanted]
	or b
	jr z, .sleep
	ldh a, [hVBlankCounter]
	ld b, a
	ld a, [wMobileLastFrame]
	cp b
	jr nz, .again ; replenish frame credit even after capture used the first slot
; Serial completions may advance bounded foreground phases during the native
; frame wait. Stop at the same scanline deadline even if the adapter is absent.
	ld a, [wMobileTransferActive]
	and a
	jr nz, .again
	ld a, [wMobileBurst]
	and a
	jr z, .sleep
	ld a, [wMobilePumpCount]
	and a
	jr nz, .again
.sleep
	halt
	nop
.done
	ld a, [wMobileOwner]
	and a
	jp z, .return
	ldh a, [rIE]
	and ~IE_SERIAL
	ldh [rIE], a
.return
	pop bc
	pop af
	ret


MobileNativeIdle::
	push af
	push bc
	push de
	push hl
	ldh a, [rWBK]
	push af
	ldh a, [rVBK]
	push af
	ldh a, [rIE]
	and ~IE_SERIAL
	ldh [rIE], a
	ldh a, [hRequested2bpp]
	ld b, a
	ldh a, [hRequested1bpp]
	or b
	jp nz, .done
	ldh a, [hDMATransfer]
	and a
	jp nz, .done
	ldh a, [rVDMA_LEN]
	bit B_VDMA_LEN_BUSY, a
	jp z, .done
	ldh a, [hVBlankCounter]
	ld b, a
	ld a, [wMobileLastFrame]
	cp b
	jr z, .sample
	call MobileTick
	jp .done
.sample
	ldh a, [rLY]
	cp 120
	jr nc, .compose
	ldh a, [hVBlankCounter]
	bit 0, a
	jr nz, .pump
	ld b, a
	ld a, [wMobileColdFrame]
	cp b
	jr z, .pump
	ld a, b
	ld [wMobileColdFrame], a
	call MobileCapture
	jp .done
.pump
	ldh a, [hVBlankCounter]
	and 3
	cp 1
	jr nz, .project_admit
	ldh a, [rLY]
	cp 120
	jr nc, .project_admit
	ldh a, [hVBlankCounter]
	ld b, a
	ld a, [wMobileVirtualUploadFrame]
	cp b
	jr z, .project_admit
	ld a, b
	ld [wMobileVirtualUploadFrame], a
	call MobileVirtualUploadJob
	jp .done
.project_admit
; Position projection and graphics are admitted independently. Every fourth
; frame gives the cold cache first choice; no job consumes a native game frame.
	ldh a, [rLY]
	cp 128
	jr nc, .compose
	ldh a, [hVBlankCounter]
	bit 0, a
	jr z, .compose ; keep pose/transport admission ahead of optional projection
	ld b, a
	ld a, [wMobileNativePrepareFrame]
	cp b
	jr z, .compose
	ld a, b
	ld [wMobileNativePrepareFrame], a
	call MobileNativePrepareSpritesWork
	jp .done

.compose
	ldh a, [hVBlankOccurred]
	and a
	jr nz, .done
	call MobileVirtualCanCompose
	jr nc, .transport
	ld a, [wMobileVirtualStage]
	and a
	jr z, .transport
	ld a, 32
	ld [wMobileVirtualAttempts], a
.compose_loop
	call MobileVirtualComposeStep
	ld a, [wMobileVirtualStage]
	and a
	jr z, .transport
	ldh a, [hVBlankOccurred]
	and a
	jr nz, .done
	call MobileVirtualCanCompose
	jr nc, .transport
	ld hl, wMobileVirtualAttempts
	dec [hl]
	jr nz, .compose_loop
.transport
	ldh a, [hVBlankOccurred]
	and a
	jr nz, .done
	ldh a, [rLY]
	cp 130
	jr nc, .done
	call MobileTick
.done
	pop af
	ldh [rVBK], a
	pop af
	ldh [rWBK], a
	pop hl
	pop de
	pop bc
	pop af
	ret

MobileVirtualCanCompose:
; C=admitted. Exact interval groups have a much smaller fixed worst case than
; arbitrary sixteen-quad shapes. Publication uses bounded four-byte copies.
	ld b, 128
	ld a, [wMobileVirtualStage]
	cp 7
	jr z, .small
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jr nz, .deadline
.small
	ld b, 133
.deadline
	ldh a, [rLY]
	cp b
	ret

MobileNativeObjectPointer:
; A slot -> BC pointer, without changing the selected WRAM bank.
	ld bc, wObjectStructs
	ld hl, OBJECT_LENGTH
.loop
	and a
	ret z
	add hl, bc
	ld b, h
	ld c, l
	ld hl, OBJECT_LENGTH
	dec a
	jr .loop

MobileNativePeerPointer:
	ld hl, wMobilePeer1
	ld bc, MOBILE_PEER_SIZE
.loop
	and a
	ret z
	add hl, bc
	dec a
	jr .loop

MobileNativePrepareSpritesWork:
	ld a, [wMobileOnline]
	and a
	ret z
	ld a, 1
	ldh [rWBK], a
	ld a, [wScriptRunning]
	and a
	ret nz
	ld a, [wPanningAroundTinyMap]
	and a
	ret nz
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
	xor a
	ld [wMobileVisibleCount], a
	ld [wMobileDeferred], a
	ld hl, wMobilePeer1
	ld c, 0
.project
	push bc
	push hl
	ld a, [wMobilePeerCount]
	cp c
	jr z, .hidden
	jr c, .hidden
	ld a, [wMobileLocalValid]
	and a
	jr z, .hidden
	call MobileProjectPeer
	jr .next
.hidden
	ld bc, MOBILE_PEER_VISIBLE
	add hl, bc
	ld [hl], 0
.next
	pop hl
	ld bc, MOBILE_PEER_SIZE
	add hl, bc
	pop bc
	inc c
	ld a, c
	cp MOBILE_PEERS
	jr c, .project
	ret

MobileVirtualUploadJob:
; One bounded pose/color job. Native menus and scripts own their graphics.
	ld a, [wMobileOnline]
	and a
	ret z
	ld a, [wMobileWanted]
	and a
	ret z
	ldh a, [hInMenu]
	and a
	ret nz
	ld a, [wMobileLocalValid]
	and a
	ret z
	ld a, 1
	ldh [rWBK], a
	ld a, [wScriptRunning]
	and a
	ret nz
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetCursor]
	ld c, a
	inc a
	cp MOBILE_PEERS
	jr c, .cursor
	xor a
.cursor
	ld [wMobileBudgetCursor], a
	ld a, c
	call MobileNativePeerPointer
	ld a, c ; pointer helper clobbers C, reload the selected index
	ld a, [wMobileBudgetCursor]
	and a
	jr nz, .previous
	ld a, MOBILE_PEERS
.previous
	dec a
	ld [wMobileBudgetIndex], a
	call MobileNativePeerPointer
	jp MobileNativeUpload

MobileNativeUpload:
; WBK=2, HL=peer. No native record or native VRAM atlas is writable.
	push hl
	ld bc, MOBILE_PEER_VISIBLE
	add hl, bc
	ld a, [hl]
	pop hl
	and a
	ret z
	push hl
	ld bc, 18
	add hl, bc
	ld a, [hli]
	ld b, a
	add a
	add b
	add a
	add [hl]
	ld c, a
	ld b, 0
	ld hl, PlayerStateSprites
	add hl, bc
	ld a, BANK(PlayerStateSprites)
	call GetFarByte
	ld b, a
	pop hl
	push hl
	ld de, 33
	add hl, de
	cp [hl]
	jr z, .sprite_ready
	ld [hli], a
	ld [hl], 0
	ld de, 2
	add hl, de
	ld bc, 16
	xor a
	rst ByteFill
	pop hl
	push hl
	ld bc, 54
	add hl, bc
	ld [hl], 0
.sprite_ready
	pop hl
	push hl
	ld bc, 34
	add hl, bc
	ld a, [hl]
	and a
	jr nz, .pose
; A build-prepared canonical asset removes the cold LZ job from the frame
; budget. Only the current four-tile pose is copied into the private cache.
	ld [hl], 1
	pop hl
	ret

.pose
	ld a, [hl]
	cp 255
	jp z, .return
	pop hl
	push hl
	ld bc, MOBILE_PEER_PALETTE
	add hl, bc
	ld c, [hl]
	ld de, 18 - MOBILE_PEER_PALETTE
	add hl, de
	ld a, [hl]
	farcall MobileNativePaletteGuard
	jp c, .return
	ld [wMobileAvatarPalette], a
	pop hl
	push hl
	ld bc, MOBILE_PEER_PALETTE
	add hl, bc
	ld [hl], a
	ld a, [wMobileBudgetIndex]
	ld b, a
	ld a, [wMobileVirtualThirdBlocked]
	and a
	jr z, .window
	ld a, b
	cp 2
	jr nz, .window
	ld b, 1 ; only the second window is shared in a large-sprite scene
.window
	ld a, b
	ld [wMobileVirtualWindow], a
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualWindowOwners
	add hl, de
	ld a, [wMobileBudgetIndex]
	inc a
	cp [hl]
	ld a, 0
	jr nz, .upload_pose
	inc a
.upload_pose
	ld [wMobileVirtualCacheHit], a
	pop hl
	push hl
	ld bc, 16
	add hl, bc
	ld a, [hl]
	ld [wMobileAvatarTile], a ; accepted facing (including mirror)
	ld b, a
	and $c
	cp $c
	jr nz, .source
	ld a, 8
.source
	bit 0, b
	jr z, .standing
	add 12
.standing
	ld [wMobileVirtualFrame], a
	pop hl
	push hl
	ld bc, MOBILE_PEER_LOADED_FRAME
	add hl, bc
	cp [hl]
	jr nz, .copy
	ld a, [wMobileVirtualCacheHit]
	and a
	jp nz, .publish_facing
.copy
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, 18
	add hl, bc
	ld a, [hli]
	ld b, a
	add a
	add b
	add a
	add [hl]
	add a
	ld e, a
	ld d, 0
	ld hl, MobileVirtualAssetPointers
	add hl, de
	ld a, BANK(MobileVirtualAssetPointers)
	call GetFarWord
	ld a, [wMobileVirtualFrame]
	ld bc, 1 tiles
	rst AddNTimes
	push hl
	ld a, [wMobileBudgetIndex]
	ld hl, wMobileAvatarCache1
	ld bc, 24 tiles
	rst AddNTimes
	ld a, [wMobileVirtualFrame]
	ld bc, 1 tiles
	rst AddNTimes
	ld d, h
	ld e, l
	pop hl
	ld bc, 4 tiles
	ld a, BANK(MobileVirtualAssetPointers)
	call FarCopyBytes
	ld h, d
	ld l, e
	ld bc, -4 tiles
	add hl, bc

	ld d, h
	ld e, l
	ld a, [wMobileVirtualWindow]
	cp 2
	ld hl, vTiles0 tile $7c
	ld a, 1
	jr z, .transfer
	ld a, [wMobileVirtualWindow]
	ld hl, vTiles0 tile $64
	and a
	jr z, .bank0
	ld hl, vTiles0 tile $68
.bank0
	xor a
.transfer
	call MobileNativeCopyFourTiles
	jp nc, .return
	ld a, [wMobileVirtualWindow]
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualWindowOwners
	add hl, de
	ld a, [wMobileBudgetIndex]
	inc a
	ld [hl], a
	pop hl
	push hl
	ld bc, MOBILE_PEER_LOADED_FRAME
	add hl, bc
	ld a, [wMobileVirtualFrame]
	ld [hl], a
.publish_facing
	pop hl
	push hl
	ld bc, 52
	add hl, bc
	ld a, [wMobileAvatarTile]
	ld [hl], a
	ld bc, 34 - 52
	add hl, bc
	ld [hl], 7
	ld bc, MOBILE_PEER_LOADED_SPRITE - 34
	add hl, bc
	ld bc, 18 - MOBILE_PEER_LOADED_SPRITE
	add hl, bc
	ld a, [hl]
	ld bc, MOBILE_PEER_LOADED_SPRITE - 18
	add hl, bc
	ld [hl], a
.return
	pop hl
	ret

MobileNativeCopyFourTiles:
; Aligned DE source, HL native destination, A=VRAM bank. Select only under DI:
; native VBlank writes VBK while drawing menus and background attributes.
; Four single-tile GDMA blocks fit four HBlanks. Never call a frame-wait helper or
; leave a DMA pending after returning. Only the short copy owns IME; VBlank has
; priority outside this bounded interval. Native DMA requests were checked above.
	di
	ldh [rVBK], a
	ld b, 4
; Reject before the first write when a whole four-HBlank job cannot fit. A
; late retry must never leave the current displayed lease partially updated.
	ldh a, [rLY]
	cp 128
	jr nc, .defer
.tile
	ld a, d
	ldh [rVDMA_SRC_HIGH], a
	ld a, e
	ldh [rVDMA_SRC_LOW], a
	ld a, h
	and $1f
	ldh [rVDMA_DEST_HIGH], a
	ld a, l
	ldh [rVDMA_DEST_LOW], a
	ldh a, [rLCDC]
	bit B_LCDC_ENABLE, a
	jr z, .copy
.leave_hblank
	ldh a, [rLY]
	cp 136
	jr nc, .defer
	ldh a, [rSTAT]
	and STAT_MODE
	jr z, .leave_hblank
.enter_hblank
	ldh a, [rLY]
	cp 136
	jr nc, .defer
	ldh a, [rSTAT]
	and STAT_MODE
	jr nz, .enter_hblank
.copy
	xor a ; one 16-byte block, general DMA
	ldh [rVDMA_LEN], a
	ld a, e
	add 16
	ld e, a
	jr nc, .source_ready
	inc d
.source_ready
	ld a, l
	add 16
	ld l, a
	jr nc, .destination_ready
	inc h
.destination_ready
	dec b
	jr nz, .tile
	scf
	ei
	ret
.defer
; A partially filled private lease is still hidden. Retry the same whole chunk.
	and a
	ei
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
	jp nz, .ineligible
	inc hl
	ld a, [wMobileLocalStaging + 1]
	cp [hl]
	jp nz, .ineligible
	inc hl
	ld a, [wMobileInstanceKey]
	cp [hl]
	jp nz, .ineligible
	inc hl
	ld a, [wMobileInstanceKey + 1]
	cp [hl]
	jp nz, .ineligible
	ld bc, 8 ; peer status at offset 17
	add hl, bc
	ld a, [hl]
	cp 1
	jp nz, .ineligible
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
	and a
	ret
.ineligible
	pop hl
	scf
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

MobileNativeResetLeases:
; Compatibility entry point for map/recovery integration. Independent actors
; clear only private presentation ownership, never ordinary NPC structures.
	push af
	push bc
	push hl
	ld hl, wMobileVirtualWindowOwners
	ld bc, NUM_OBJECT_STRUCTS
	xor a
	rst ByteFill
	pop hl
	pop bc
	pop af
	ret
