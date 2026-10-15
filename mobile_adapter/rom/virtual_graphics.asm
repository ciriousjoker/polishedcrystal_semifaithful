; One candidate owns its exact pose batch, RGB bindings and OAM publication.
SECTION "Mobile Virtual Graphics", ROMX, BANK[MOBILE_ROM_BANK]
MobileVirtualGraphicsStart::

; Foreground only. Pending jobs own immutable caches. Existing peer writers
; revoke PreviousOAM before changing identity, scene, availability or avatar.
MobileVirtualPrevalidatePoseBatch:
 xor a
 ld [wMobileStablePoseCanceled], a
 inc a
 ld [wMobileStablePosePreparing], a
 ld a, [wMobileVirtualQueuedPeer]
 ld [wMobileVirtualPoseBatchMask], a
 call MobileVirtualPoseBatchMatches
 jr nz, .failed
 xor a
 ld [wMobileVirtualPoseBatchIndex], a
.next
 call MobileVirtualLoadPoseJob
 jr z, .validated
 call MobileVirtualValidatePoseJob
 jr c, .discard
.validated
 ld hl, wMobileVirtualPoseBatchIndex
 inc [hl]
 ld a, [hl]
 cp MOBILE_PEERS
 jr c, .next
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileVirtualPoseBatchMask]
 ld [wMobileVirtualQueuedPeer], a
 ld a, [wMobileStablePoseCanceled]
 and a
 jr nz, .discard
 ld [wMobileStablePosePreparing], a
 and a
 ret
.discard
 xor a
 ld [wMobileVirtualPoseBatchMask], a
.failed
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileVirtualPoseBatchMask]
 ld [wMobileVirtualQueuedPeer], a
 ld a, [wMobileStablePoseCanceled]
 and a
 jr z, .failure_ready
 xor a
 ld [wMobileVirtualQueuedPeer], a
.failure_ready
 xor a
 ld [wMobileStablePosePreparing], a
 scf
 ret

; The chooser has validated the complete candidate. No foreground or serial
; writer can run between this last exact batch check and its palette/OAM use.
; Cold preparation owns only inactive fixed windows. Native allocation and
; prewrite guards retain their existing authority. The physical DMA stamp,
; rather than the current candidate flag, excludes any still displayed page.
MobileVirtualPrepareColdPose::
 push bc
 push de
 push hl
 ldh a, [rWBK]
 push af
 ldh a, [rVBK]
 push af
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileStableSourceValid]
 cp 1
 jp nz, .failed
 ld a, [wMobileStableActorViewValid]
 cp 1
 jp nz, .failed
 ld a, [wMobileStablePoseCanceled]
 and a
 jp nz, .failed
 ldh a, [hRequested2bpp]
 ld b, a
 ldh a, [hRequested1bpp]
 or b
 jp nz, .failed
 ldh a, [hDMATransfer]
 and a
 jp nz, .failed
 ldh a, [rVDMA_LEN]
 bit B_VDMA_LEN_BUSY, a
 jp z, .failed
 call MobileVirtualPoseBatchMatches
 jp nz, .failed
 ld a, [wMobileVirtualQueuedPeer]
 and a
 jp z, .success
 ld a, [wMobileActualOAMPage]
 cp HIGH(wMobileVirtualOutput)
 jr nz, .inactive_page_ready
 ; The actual DMA mask binds destinations, independently of pending state.
 ld a, [wMobileStablePoseMask]
 rept 3
 rrca
 endr
 ld b, a
 ld a, [wMobileStableLastActual + 1]
 and b
 jp nz, .failed
.inactive_page_ready
 ; Finite cold discriminator: no ordinary nonlocal atlas may be allocated,
 ; including hidden owners, and no native animation owns temporary graphics.
 ; A native binding cannot change while this foreground idle call owns CPU.
 ld a, 1
 ldh [rWBK], a
 ld hl, wObject1Sprite
 ld de, OBJECT_LENGTH
 ld b, NUM_OBJECT_STRUCTS - 1
.native_owner
 ld a, [hl]
 and a
 jp nz, .failed
 add hl, de
 dec b
 jr nz, .native_owner
for actor, 1, NUM_SPRITE_ANIM_STRUCTS + 1
 ld a, [wSpriteAnim{d:actor} + SPRITEANIMSTRUCT_INDEX]
 and a
 jp nz, .failed
endr
 ld a, [wScriptRunning]
 and a
 jp nz, .failed
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
for peer, MOBILE_PEERS
 ld a, [wMobileStablePoseMask]
 bit peer, a
 jr z, .inactive_ready{d:peer}
 ld a, [wMobileVirtualWindowOwners + peer]
 and a
 jp nz, .failed
.inactive_ready{d:peer}
endr
 ld a, [wMobileNativeGraphicsFence]
 and a
 jp nz, .failed
 ; Movement may change without retiring the peer identity. Validate every
 ; pending movement before the first tile write, retaining the old GPU pose
 ; only through its existing explicit committed-pose path.
for peer, MOBILE_PEERS
 DEF peer_number = peer + 1
 ld a, [wMobileStablePoseMask]
 bit peer, a
 jr z, .movement_ready{d:peer}
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 ld a, [wMobilePeer{d:peer_number}Movement]
 ld b, a
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileStablePoseBatch + peer * 8 + 7]
 cp b
 jp nz, .discard
.movement_ready{d:peer}
 PURGE peer_number
endr
 ldh a, [rLCDC]
 bit B_LCDC_ENABLE, a
 jp z, .failed
 bit 4, a ; signed BG tile addressing keeps native background outside OBJ holes
 jp nz, .failed
 ; Close the actual remaining transfer batch and fused publication, rather
 ; than inheriting a per-job deadline. Measured one/two batch tails are
 ; 20956T/21616T from first HDMA to idle.done. Include phase and return reserve.
 ld c, 0
 ld a, [wMobileStablePoseMask]
 for peer, MOBILE_PEERS
 bit peer, a
 jr z, .count_transfer{d:peer}
 inc c
.count_transfer{d:peer}
 endr
 ld b, MOBILE_COMPACT_NATIVE_LINE - 22800 / MOBILE_COMPACT_LINE_T - 1
 dec c
 jr z, .batch_deadline
 ld b, MOBILE_COMPACT_NATIVE_LINE - 23712 / MOBILE_COMPACT_LINE_T - 1
 dec c
 jr z, .batch_deadline
 ld b, MOBILE_COMPACT_NATIVE_LINE - 30096 / MOBILE_COMPACT_LINE_T - 1
.batch_deadline
 ldh a, [rLY]
 cp b
 jp nc, .failed
.safe_lcd
 ; Only inactive tiles change here. RGB and actual OAM remain native.
for peer, MOBILE_PEERS
 DEF peer_number = peer + 1
 ld a, [wMobileStablePoseMask]
 bit peer, a
 jp z, .pose_ready{d:peer}
 ; Source/window/alignment and identity were validated before publication.
 ld a, [wMobileStablePoseBatch + peer * 8 + 1]
 ld l, a
 ld a, [wMobileStablePoseBatch + peer * 8 + 2]
 ld h, a
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 xor a
 ldh [rVBK], a
 if peer < 2
 ld de, vTiles0 tile ($64 + peer * 4)
 ld b, 4
 call MobileVirtualColdHDMA
 else
 ld de, vTiles0 tile $6c
 ld b, 1
 call MobileVirtualColdHDMA
 ld a, 1
 ldh [rVBK], a
 for quadrant, 3
  ld de, vTiles0 tile (($40 << quadrant) - 1)
  ld b, 1
  call MobileVirtualColdHDMA
 endr
 endc
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileStablePoseBatch + peer * 8 + 6]
 ld b, a
 ld a, [wMobileStablePoseBatch + peer * 8 + 3]
 ld c, a
 ld a, [wMobileStablePoseBatch + peer * 8 + 4]
 ld d, a
 ld a, [wMobileStablePoseBatch + peer * 8 + 7]
 ld e, a
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 ld a, b
 ld [wMobilePeer{d:peer_number}LoadedSprite], a
 ld a, c
 ld [wMobilePeer{d:peer_number}LoadedFrame], a
 ld a, d
 ld [wMobilePeer{d:peer_number}NativeFacing], a
 ld a, e
 ld [wMobilePeer{d:peer_number}LoadedMovement], a
 ld a, 7
 ld [wMobilePeer{d:peer_number}NativeStage], a
 ld a, peer + 1
 ld [wMobileVirtualWindowOwners + peer], a
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
.pose_ready{d:peer}
 PURGE peer_number
endr
 xor a
 ld [wMobileVirtualQueuedPeer], a
.success
 pop af
 ldh [rVBK], a
 pop af
 ldh [rWBK], a
 pop hl
 pop de
 pop bc
 and a
 ret
.discard
 xor a
 ld [wMobileVirtualQueuedPeer], a
.failed
 pop af
 ldh [rVBK], a
 pop af
 ldh [rWBK], a
 pop hl
 pop de
 pop bc
 scf
 ret

; Synchronous ownership of one HBlank transfer. No bank/source selector changes
; or foreground/native work can occur until FF55 reports complete. Interrupts
; remain enabled; the admission bound completes before the native VBlank.
MobileVirtualColdHDMA:
 ld a, h
 ldh [rVDMA_SRC_HIGH], a
 ld a, l
 ldh [rVDMA_SRC_LOW], a
 ld a, d
 and $1f
 ldh [rVDMA_DEST_HIGH], a
 ld a, e
 ldh [rVDMA_DEST_LOW], a
 ld a, b
 dec a
 or $80
 ldh [rVDMA_LEN], a
.wait
 ldh a, [rVDMA_LEN]
 bit 7, a
 jr z, .wait
 ; Source selector advances by exactly the immutable block span.
 ld a, b
 swap a
 ld e, a
 ld d, 0
 add hl, de
 ret

; Cold tiles were completed in the inactive idle lease. An unprepared batch
; cannot fall back to unsafe visible CPU stores in the IRQ.
MobileVirtualCommitPose::
 ld a, [wMobileVirtualQueuedPeer]
 and a
 jr nz, .pending
 and a
 ret
.pending
 scf
 ret

MobileVirtualLoadPoseJob:
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileVirtualPoseBatchIndex]
 ld e, a
 ld d, 0
 ld hl, MobileVirtualPaletteBits
 add hl, de
 ld a, [wMobileVirtualPoseBatchMask]
 and [hl]
 ret z
 ld a, e
 inc a
 ld [wMobileVirtualQueuedPeer], a
 ld a, e
 add a
 add a
 add a
 ld e, a
 ld hl, wMobileStablePoseBatch
 add hl, de
 ld de, wMobileVirtualQueuedWindow
 ld bc, 8
 rst CopyBytes
 ld a, 1
 and a
 ret

MobileVirtualValidatePoseJob:
	ld a, [wMobileVirtualQueuedFrame]
	cp 21
	jp nc, .invalid
	and 3
	jp nz, .invalid
	ld a, [wMobileVirtualQueuedPeer]
	cp MOBILE_PEERS + 1
	jp nc, .invalid
	dec a
	ld b, a
	ld a, [wMobileVirtualQueuedWindow]
	cp b
	jp nz, .invalid ; each peer has a distinct, fixed four-tile window
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
	jp nz, .invalid
	ld a, [wMobileVirtualQueuedSource + 1]
	cp h
	jp nz, .invalid ; only this peer's immutable, aligned four-tile pose is valid
	ld a, b
	call MobileNativePeerPointer
	push hl
	ld a, [hli]
	ld b, a
	ld a, [hli]
	ld c, a
	ld a, [hli]
	ld d, a
	ld e, [hl]
	push bc
	push de
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileVirtualQueuedWindow]
	add a
	add a
	ld e, a
	ld d, 0
	ld hl, wMobileStableRemoteBindings
	add hl, de
	pop de
	pop bc
	ld a, [hli]
	cp b
	jr nz, .identity_invalid
	ld a, [hli]
	cp c
	jr nz, .identity_invalid
	ld a, [hli]
	cp d
	jr nz, .identity_invalid
	ld a, [hl]
	cp e
.identity_invalid
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	pop hl
	jp nz, .invalid
	push hl
	ld bc, 18
	add hl, bc
	ld a, [wMobileVirtualQueuedAvatar]
	cp [hl]
	jp nz, .avatar_checked
	inc hl
	ld a, [wMobileVirtualQueuedMovement]
	cp [hl]
.avatar_checked
	pop hl
	jp nz, .invalid
	ld bc, 33
	add hl, bc
	ld a, [wMobileVirtualQueuedSprite]
	cp [hl]
	jp nz, .invalid ; a new identity/appearance invalidated this pending job
	and a
	ret
.invalid
	scf
	ret


MobileVirtualGraphicsEnd::
ASSERT MobileVirtualGraphicsEnd - MobileVirtualGraphicsStart <= 2048
