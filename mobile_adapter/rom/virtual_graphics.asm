; Commit an immutable four-tile pose while the LCD cannot fetch sprite tiles.
; The ordinary engine still owns OAM DMA, frame cadence and save structures.
SECTION "Mobile Virtual Graphics", ROMX, BANK[MOBILE_ROM_BANK]
MobileVirtualGraphicsStart::

MobileVirtualCommitPose::
	ld a, [wMobileNativeGraphicsFence]
	and a
	ret nz
	ld a, [wMobileVirtualQueuedPeer]
	and a
	ret z
	ldh a, [hRequested2bpp]
	and a
	ret nz ; native graphics transfers own the early VBlank window
	ldh a, [hRequested1bpp]
	and a
	ret nz
	push af
	push bc
	push de
	push hl
	ldh a, [rWBK]
	push af
	ldh a, [rVBK]
	push af
	ld a, [wMobileWanted]
	and a
	jp z, .discard
	ld a, [wMobileOnline]
	and a
	jp z, .discard
	ld a, [wMobileLocalValid]
	and a
	jp z, .discard
	ldh a, [hInMenu]
	and a
	jp nz, .discard
	ldh a, [hVBlank]
	and $f
	jp nz, .restore ; ordinary world VBlank owns the matching OAM publication
	ldh a, [hDMATransfer]
	and a
	jp nz, .restore ; do not overwrite a native transfer's prepared DMA registers
	ldh a, [rVDMA_LEN]
	bit B_VDMA_LEN_BUSY, a
	jp z, .restore
	ldh a, [rLCDC]
	bit B_LCDC_ENABLE, a
	jr z, .safe_lcd
	ldh a, [rLY]
	cp 144
	jp c, .restore
	cp 148
	jp nc, .restore ; preserve the ordinary OAM DMA's remaining VBlank window
.safe_lcd
	ld a, [wMobileVirtualQueuedFrame]
	cp 21
	jp nc, .discard
	and 3
	jp nz, .discard
	ld a, [wMobileVirtualQueuedPeer]
	cp MOBILE_PEERS + 1
	jp nc, .discard
	dec a
	ld b, a
	ld a, [wMobileVirtualQueuedWindow]
	cp b
	jp nz, .discard ; each peer has a distinct, fixed four-tile window
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, b
	push bc
	ld hl, wMobileAvatarCache1
	ld bc, 24 tiles
	rst AddNTimes
	ld a, [wMobileVirtualQueuedFrame]
	ld bc, 1 tiles
	rst AddNTimes
	ld a, [wMobileVirtualQueuedSource]
	cp l
	pop bc
	jp nz, .discard
	ld a, [wMobileVirtualQueuedSource + 1]
	cp h
	jp nz, .discard ; only this peer's immutable, aligned four-tile pose is valid
	ld a, b
	call MobileNativePeerPointer
	push hl
	ld bc, 18
	add hl, bc
	ld a, [wMobileVirtualQueuedAvatar]
	cp [hl]
	jp nz, .discard_avatar
	inc hl
	ld a, [wMobileVirtualQueuedMovement]
	cp [hl]
.discard_avatar
	pop hl
	jp nz, .discard
	ld bc, 33
	add hl, bc
	ld a, [wMobileVirtualQueuedSprite]
	cp [hl]
	jp nz, .discard ; a new identity/appearance invalidated this pending job
	ld a, [wMobileVirtualQueuedSource]
	ld e, a
	ld a, [wMobileVirtualQueuedSource + 1]
	ld d, a
	ld a, [wMobileVirtualQueuedWindow]
	add a
	add a
	add $64
	ld l, a
	ld h, 0
rept 4
	add hl, hl
endr
	ld bc, vTiles0
	add hl, bc
	xor a
	ldh [rVBK], a
	ld b, 3
	ld a, [wMobileVirtualQueuedWindow]
	cp 2
	jr nz, .copy
; Preserve bank-0 weather tiles. Each third-window quadrant has its own
; audited slot; the native fifteen-tile atlases leave $3f/$7f unused.
	ld b, 0
.copy
	call .dma
	ld a, [wMobileVirtualQueuedWindow]
	cp 2
	jr nz, .copied
	ld a, 1
	ldh [rVBK], a
for quadrant, 3
	DEF tile_id = ($40 << quadrant) - 1
	ld a, e
	add 1 tiles
	ld e, a
	jr nc, .next_source{d:tile_id}
	inc d
.next_source{d:tile_id}
	ld hl, vTiles0 tile tile_id
	ld b, 0
	call .dma
	PURGE tile_id
endr
.copied
	ld a, [wMobileVirtualQueuedWindow]
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualWindowOwners
	add hl, de
	ld a, [wMobileVirtualQueuedPeer]
	ld [hl], a
	dec a
	call MobileNativePeerPointer
	push hl
	ld bc, MOBILE_PEER_LOADED_SPRITE
	add hl, bc
	ld a, [wMobileVirtualQueuedAvatar]
	ld [hli], a
	ld a, [wMobileVirtualQueuedFrame]
	ld [hl], a
	ld bc, 34 - MOBILE_PEER_LOADED_FRAME
	add hl, bc
	ld [hl], 7
	ld bc, 52 - 34
	add hl, bc
	ld a, [wMobileVirtualQueuedFacing]
	ld [hl], a
	ld bc, MOBILE_PEER_LOADED_MOVEMENT - 52
	add hl, bc
	ld a, [wMobileVirtualQueuedMovement]
	ld [hl], a
	pop hl
	call .patch_pose
.discard
	xor a
	ld [wMobileVirtualQueuedPeer], a
.restore
	pop af
	ldh [rVBK], a
	pop af
	ldh [rWBK], a
	pop hl
	pop de
	pop bc
	pop af
	ret

.dma
	ld a, d
	ldh [rVDMA_SRC_HIGH], a
	ld a, e
	ldh [rVDMA_SRC_LOW], a
	ld a, h
	and $1f
	ldh [rVDMA_DEST_HIGH], a
	ld a, l
	ldh [rVDMA_DEST_LOW], a
	ld a, b
	ldh [rVDMA_LEN], a ; complete GDMA, no request survives this return
	ret

.patch_pose
; A previously published figure may still use the same window. Commit its
; quadrant bank/flip attributes together with the tiles, before native OAM DMA.
	push hl
	call MobileVirtualFrameValid
	pop hl
	ret z
	ld bc, MOBILE_PEER_OAM_START + 1
	add hl, bc
	ld a, [hl]
	cp OAM_SIZE - 16 + 1
	ret nc
	ld l, a
	ld h, HIGH(wMobileVirtualOutput)
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	push hl
	ld a, [wMobileVirtualQueuedWindow]
	add a
	add a
	add $64
	ld b, a
	inc hl
	inc hl
	ld a, [wMobileVirtualQueuedWindow]
	cp 2
	ld a, [hli]
	jr z, .third_window
	sub b
	cp 2
	jr nc, .not_present
	bit 3, [hl]
	jr nz, .not_present
	jr .pose_present
.third_window
	cp $6c
	jr nz, .third_mirror
	bit 3, [hl]
	jr nz, .not_present
	jr .pose_present
.third_mirror
	cp $3f
	jr nz, .not_present
	bit 3, [hl]
	jr z, .not_present
.pose_present
	ld a, [hl]
	and ~(OAM_XFLIP | OAM_BANK1)
	ld c, a
	ld a, [wMobileVirtualQueuedFacing]
	call MobileVirtualFacingMirror
	or c
	ld c, a
	and OAM_XFLIP
	jr z, .columns
	inc b
.columns
	pop hl
	ld d, [hl]
	inc hl
	ld e, [hl]
	dec hl
	jp MobileVirtualWritePeerQuads
.not_present
	pop hl
	ret

MobileVirtualFacingMirror:
	and $c
	cp $c
	jr z, .mirror
	cp 8
	ld a, 0
	ret nc
	ld a, [wMobileVirtualQueuedFacing]
	and 3
	cp 3
	ld a, 0
	ret nz
.mirror
	ld a, OAM_XFLIP
	ret

MobileVirtualGraphicsEnd::
ASSERT MobileVirtualGraphicsEnd - MobileVirtualGraphicsStart <= 1024
