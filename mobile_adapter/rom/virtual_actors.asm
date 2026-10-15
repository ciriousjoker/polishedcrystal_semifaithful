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
	ld a, [wMobileVirtualStage]
	and a
	ld a, 1
	jr nz, .serial_slice
	ld a, $ff
.serial_slice
	ld [wMobileVirtualSerialSlice], a
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
	call MobileNativeLateIRQ
; A rejected late drain can have observed the awaited VBlank during its checks.
	ldh a, [hVBlankOccurred]
	and a
	jr nz, .done
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


MobileNativeLateIRQ::
; A completed byte may be pending while the composer owns the idle CPU. Drain
; exactly that byte before HALT when its audited ISR plus return still fits.
; The IRQ's existing LY136 guard prohibits chaining a new byte in this window.
	ldh a, [hVBlankOccurred]
	and a
	ret nz
	ld a, [wMobileOwner]
	and a
	ret z
	ld a, [wMobileTransferActive]
	and a
	ret z
	ldh a, [rIF]
	bit B_IE_SERIAL, a
	ret z
	ldh a, [rLY]
	cp 140
	ret nc
	ld a, 1
	ld [wMobileVirtualSerialSlice], a
	ldh a, [rIE]
	or IE_SERIAL
	ldh [rIE], a
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
	call MobileVirtualNativeRequestsDisjoint
	jp c, .done
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
; Replenish genuine frame credit before required presentation. The protected
; credit entry leaves FrameTick pending: the ordinary pump still owns command
; deadlines/parsing. A previous pending tick forces that pump now, bounding
; deferral even when consecutive frames cannot finish cold presentation.
.first_transport
; Preserve the carried-byte drain before taking the foreground slice.
	call MobileNativeLateIRQ
	ldh a, [rIE]
	and ~IE_SERIAL
	ldh [rIE], a
	ld a, [wMobileVirtualStage]
	cp 1
	jr nz, .ordinary_tick
	ld a, [wMobileFrameTick]
	and a
	jr nz, .ordinary_tick
	call MobileTickCredit
	jr .sample
.ordinary_tick
	call MobileTick
.sample
; The closed native source does not depend on local upload readiness. Give it
; its first actual attempt before optional initial snapshot/hash work.
	ld a, [wMobileVirtualStage]
	and a
	jp nz, .compose
	ld a, [wMobileLocalValid]
	and a
	jr nz, .required_capture_ready
	ldh a, [rLY]
	cp 94
	jr nc, .required_capture_ready
	call MobileCapture
.required_capture_ready
	ld a, [wMobileVirtualStage]
	and a
	jp nz, .compose
; A dense native frame leaves a much smaller idle window. Once a complete
; guest exists, reserve three of four late windows for presentation; the fourth
; advances optional capture/pose/projection jobs. Early scenes keep their normal
; per-frame policy. Deferred transport keeps real frame credit pending.
	ld a, [wMobileVirtualStage]
	and a
	jr z, .optional_job
	ldh a, [rLY]
	cp 116
	jr c, .optional_job
	ldh a, [hVBlankCounter]
	and 3
	jp nz, .compose
.optional_job
; At most one optional capture, graphics or projection job per frame. Select
; the oldest eligible job so a short idle window cannot permanently starve
; projection behind capture and upload work. The remaining idle calls compose
; the native scene and pump bounded transport.
	ldh a, [hVBlankCounter]
	ld b, a
	ld a, [wMobileVirtualBackgroundFrame]
	cp b
	jp z, .compose
	ldh a, [rLY]
	cp 130
	jr nc, .compose
	ld hl, wMobileColdFrame
	ld a, b
	sub [hl]
	ld c, a
	ld hl, wMobileNativePrepareFrame
	ld a, b
	sub [hl]
	ld d, a
	ld hl, wMobileVirtualUploadFrame
	ld a, b
	sub [hl]
	cp 4
	jr c, .capture_or_project
	cp c
	jr c, .capture_or_project
	cp d
	jr c, .capture_or_project
	ldh a, [rLY]
	cp 130 ; direct color selection and pose copying are separate bounded jobs
	jr nc, .capture_or_project
	ld a, b
	ld [wMobileVirtualUploadFrame], a
	ld [wMobileVirtualBackgroundFrame], a
	call MobileVirtualUploadJob
	jp .done
.capture_or_project
	ld a, c
	cp d
	jr c, .project_admit
	cp 2
	jr c, .compose
	ld a, b
	ld [wMobileColdFrame], a
	ld [wMobileVirtualBackgroundFrame], a
	call MobileCapture
	jp .done
.project_admit
	ld a, d
	cp 2
	jr c, .compose
	ld a, b
	ld [wMobileNativePrepareFrame], a
	ld [wMobileVirtualBackgroundFrame], a
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
	cp 1
	jr z, .transport ; a rejected cold attempt must yield to the real pump
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
	ld b, 110 ; sixteen irregular quads plus exact rollback need a larger window
	ld a, [wMobileVirtualStage]
	cp 9
	jr z, .native_deadline
	cp 7
	jr z, .publication
	cp 3
	jr z, .small
	cp 5
	jr z, .native_deadline ; late calls only close optional peer admission
	cp 6
	jr z, .weather_deadline
	cp 4
	jr z, .native_deadline
	cp 2
	jr nz, .start_deadline
	jp MobileVirtualNativeDeadline
.native_deadline
; Each native group applies its own shape deadline before doing expensive
; admission. Empty descriptors must not inherit the sixteen-quad deadline.
	ld b, 142 ; late calls may only drain cheap rejections and publish bookkeeping
	jr .deadline
.start_deadline
	ld a, [wMobileVirtualStage]
	cp 1
	jr nz, .shape_deadline
	ld b, 128
.shape_deadline
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jr nz, .deadline
	ld a, [wMobileVirtualStage]
	cp 3
	jr z, .small ; peer preparation has a larger bound than a raw native group
	cp 1
	jr nz, .canonical_group_deadline
	ld b, 128 ; full metadata audit and aligned proof also need caller-return time
	jr .deadline
.canonical_group_deadline
	ld b, 140
	jr .deadline
.publication
	ld b, 138 ; include foreground merged RGB and complete caller return
	ld a, [wMobileVirtualLength]
	cp OAM_SIZE
	jr nz, .deadline
	ld b, 139 ; full pages have no suffix stores
	jr .deadline
.small
 ; Known invisible peers only advance bookkeeping. A visible complete peer
 ; keeps a conservative twelve-line closure until actual entry/return proof.
 ld a, [wMobileVirtualCursor]
 ld c, a
 ld a, [wMobilePeerCount]
 cp c
 jr z, .cheap_peer
 jr c, .cheap_peer
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 ld a, c
 call MobileNativePeerPointer
 ld bc, MOBILE_PEER_VISIBLE
 add hl, bc
 ld c, [hl]
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, c
 and a
 jr z, .cheap_peer
 ld b, 132
 jr .deadline
.cheap_peer
 ld b, 140
 jr .deadline
.weather_deadline
	ld b, 140
.deadline
	ldh a, [rLY]
	cp b
	ret

MobileVirtualNativeDeadline:
; Both the idle caller and the step check the actual pending native slot.
; Leave all transaction state intact when a complete figure must wait.
	ld b, 138 ; bounded empty-slot drain plus complete idle caller return
	ld a, [wMobileVirtualCursor]
	cp NUM_OBJECT_STRUCTS
	jr nc, .deadline
	add a
	add a
	add a
	add LOW(wMobileVirtualDescriptors + 6)
	ld l, a
	ld h, HIGH(wMobileVirtualDescriptors + 6)
	ld a, [hl]
	and $1f
	jr z, .deadline
	ld b, 110 ; complete general figure, exact admission and rollback
	cp 5
	jr nc, .deadline
	ld b, 131 ; complete four-quad caller plus return and scanline margin
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
	jr z, .world_ready
	cp PLAYEREVENT_JOYCHANGEFACING
	ret nz
.world_ready
	ld a, [wPanningAroundTinyMap]
	and a
	ret nz
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileStableSourceValid]
	cp 1
	ret nz
	ld a, [wMobileStableActorViewValid]
	cp 1
	ret nz
	ld a, [wMobileStableCameraX]
	ld [wMobileCameraX], a
	ld a, [wMobileStableCameraX + 1]
	ld [wMobileCameraX + 1], a
	ld a, [wMobileStableCameraY]
	ld [wMobileCameraY], a
	ld a, [wMobileStableCameraY + 1]
	ld [wMobileCameraY + 1], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	xor a
	ld [wMobileVisibleCount], a
	ld [wMobileDeferred], a
 farcall MobileCompactProjectPeers
 ret

MobileVirtualUploadJob:
	ld a, [wMobileNativeGraphicsFence]
	and a
	jr z, .ownership_ready
	farcall MobileNativeGraphicsRelease
	and a
	ret nz
.ownership_ready
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
; A pending job owns this peer's source until the joint hardware commit.
	push hl
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [wMobileVirtualQueuedPeer]
	and [hl]
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	pop hl
	ret nz
; WBK=2, HL=peer. No native record or native VRAM atlas is writable.
	push hl
	ld bc, MOBILE_PEER_VISIBLE
	add hl, bc
	ld a, [hl]
	pop hl
	and a
	ret z
 ; Cold jobs use one bounded constructor. Keep the general committed-pose
 ; updater for warm motion until a separate active-window lease is established.
 push hl
 ld bc, MOBILE_PEER_NATIVE_STAGE
 add hl, bc
 ld a, [hl]
 cp 7
 pop hl
 jr z, .committed_update
 push hl
 farcall MobileCompactColdUpload
 pop hl
 ret
.committed_update
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	xor a
	ld [wMobileStablePoseCanceled], a
	inc a
	ld [wMobileStablePosePreparing], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
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
	ld hl, MobileVirtualAssetSprites
	add hl, bc
	ld a, [hl]
	ld b, a
	pop hl
	push hl
	ld de, 33
	add hl, de
	cp [hl]
	jr z, .sprite_ready
	ld [hli], a
	ld a, [hl]
	cp 7
	jr nz, .invalidate_pose
; A movement-mode change has the same color and a complete old GPU pose.
; Keep that presentation coherent until the new immutable job commits.
	push hl
	ld bc, MOBILE_PEER_LOADED_SPRITE - 34
	add hl, bc
	ld a, [hl]
	ld bc, 18 - MOBILE_PEER_LOADED_SPRITE
	add hl, bc
	cp [hl]
	pop hl
	jr z, .sprite_ready
.invalidate_pose
	ld [hl], 0
	inc hl
	ld [hl], 0 ; cancel an obsolete split color/pose job
	inc hl
	ld bc, 16
	xor a
	rst ByteFill
	pop hl
	push hl
	ld bc, MOBILE_PEER_LOADED_FRAME
	add hl, bc
	ld [hl], 255 ; a changed sprite cannot reuse the old window's equal pose index
	ld bc, 54 - MOBILE_PEER_LOADED_FRAME
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

.pose
	ld a, [hl]
	cp 255
	jp z, .return
	inc hl
	ld a, [hl]
	and a
	jr nz, .resume_pose
	ld bc, 54 - MOBILE_PEER_COLOR_READY
	add hl, bc
	ld a, [hl]
	cp 1
	jr nz, .check_color
; The private constructor already caches identity and fade dirtiness. Its
; immutable color remains usable without an eight-slot native lookup per pose.
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [wMobilePrivatePalDirty]
	and [hl]
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	jr z, .cached_pose
.check_color
	pop hl
	push hl
	ld bc, MOBILE_PEER_PALETTE
	add hl, bc
	ld c, [hl]
	ld de, 18 - MOBILE_PEER_PALETTE
	add hl, de
	ld a, [hl]
.construct_color
	call MobileNativePaletteGuard
	jp c, .return
.color_ready
	ld [wMobileAvatarPalette], a
	pop hl
	push hl
	ld bc, MOBILE_PEER_PALETTE
	add hl, bc
	ld [hl], a
	ldh a, [rLY]
	cp 116
	jr c, .window_ready
; Palette construction and a banked 64-byte pose copy have separate budgets.
; Dense native scenes still have room for either job, but not both together.
	pop hl
	push hl
	ld bc, MOBILE_PEER_COLOR_READY
	add hl, bc
	ld [hl], 1 ; keep a previously committed pose ready throughout this job
	jp .return
.resume_pose
	ld [hl], 0
.cached_pose
	pop hl
	push hl
	ld bc, MOBILE_PEER_PALETTE
	add hl, bc
	ld a, [hl]
	ld [wMobileAvatarPalette], a
.window_ready
	ld a, [wMobileBudgetIndex]
	ld b, a
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
	jr z, .copy
	ld bc, MOBILE_PEER_LOADED_MOVEMENT - MOBILE_PEER_LOADED_FRAME
	add hl, bc
	ld a, [hl]
	ld bc, 19 - MOBILE_PEER_LOADED_MOVEMENT
	add hl, bc
	cp [hl]
	jp z, .publish_facing
.copy
 ; The generated assets and this fixed-size copier share the same upper bank.
 ; One immutable canonical pose copy replaces the generic far-copy constructor.
 farcall MobileVirtualCopyCanonicalPose

	ld d, h
	ld e, l
	ld a, e
	ld [wMobileVirtualQueuedSource], a
	ld a, d
	ld [wMobileVirtualQueuedSource + 1], a
	ld a, [wMobileVirtualWindow]
	ld [wMobileVirtualQueuedWindow], a
	ld a, [wMobileVirtualFrame]
	ld [wMobileVirtualQueuedFrame], a
	ld a, [wMobileAvatarTile]
	ld [wMobileVirtualQueuedFacing], a
	pop hl
	push hl
	ld bc, 33
	add hl, bc
	ld a, [hl]
	ld [wMobileVirtualQueuedSprite], a
	ld bc, 18 - 33
	add hl, bc
	ld a, [hl]
	ld [wMobileVirtualQueuedAvatar], a
	inc hl
	ld a, [hl]
	ld [wMobileVirtualQueuedMovement], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
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
	ld a, [wMobileBudgetIndex]
	add a
	add a
	ld e, a
	ld d, 0
	ld hl, wMobileStableRemoteBindings
	add hl, de
	pop de
	pop bc
	ld a, b
	ld [hli], a
	ld a, c
	ld [hli], a
	ld a, d
	ld [hli], a
	ld [hl], e
	ld a, [wMobileBudgetIndex]
	add a
	add a
	add a
	ld e, a
	ld d, 0
	ld hl, wMobileStablePoseBatch
	add hl, de
	ld d, h
	ld e, l
	ld hl, wMobileVirtualQueuedWindow
	ld bc, 8
	rst CopyBytes
	ld hl, wMobileStablePoseGeneration
	inc [hl]
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [wMobileVirtualQueuedPeer]
	or [hl]
	ld [wMobileVirtualQueuedPeer], a ; publish the complete peer job last
	ld a, [wMobileStablePoseCanceled]
	and a
	jr z, .return
	xor a
	ld [wMobileVirtualQueuedPeer], a
	jr .return
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
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileStablePoseCanceled]
	and a
	jr z, .preparation_done
	xor a
	ld [wMobileVirtualQueuedPeer], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, MOBILE_PEER_NATIVE_STAGE
	add hl, bc
	ld [hl], 0
.preparation_done
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	xor a
	ld [wMobileStablePosePreparing], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	pop hl
	ret

; Palette sharing is decided by eight retained RGB bytes, never an ID.
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
	push de
	push hl
	ld hl, wMobileVirtualWindowOwners
	ld bc, NUM_OBJECT_STRUCTS
	xor a
	rst ByteFill
	ld [wMobileVirtualQueuedPeer], a
	ld [wMobileVirtualForceFull], a
	ldh a, [rWBK]
	push af
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
 xor a
 ld [wMobileNativeRGBLeaseValid], a
 ; This entry is called only by actual map replacement, not ordinary refresh.
 ; Retire native bindings before the map setup script clears associations.
 xor a
.retire_native
 call MobileRetireNativeSlot
 inc a
 cp NUM_OBJECT_STRUCTS
 jr c, .retire_native
 call MobileRejectNormalSource
 pop af
 ldh [rWBK], a
	ld a, $80
	ld [wMobileVirtualNeedColorAudit], a
	pop hl
	pop de
	pop bc
	pop af
	ret

PUSHS
SECTION "Mobile Canonical Pose Copy", ROMX, BANK[MOBILE_ROM_BANK + 1]
MobileVirtualCopyCanonicalPose:
 ; WBK=2. The validated peer supplies avatar and movement exactly once.
 ld a, [wMobileBudgetIndex]
 add a
 ld e, a
 ld d, 0
 ld hl, .peer_appearance
 add hl, de
 ld a, [hli]
 ld h, [hl]
 ld l, a
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
 ld a, [hli]
 ld d, [hl]
 ld e, a
 ld a, [wMobileVirtualFrame]
 ld l, a
 ld h, 0
 rept 4
 add hl, hl
 endr
 push hl
 add hl, de
 push hl
 ld a, [wMobileBudgetIndex]
 add a
 ld e, a
 ld d, 0
 ld hl, .cache_bases
 add hl, de
 ld a, [hli]
 ld d, [hl]
 ld e, a
 pop bc ; exact generated ROM source
 pop hl ; exact pose byte offset
 add hl, de
 ld d, h
 ld e, l
 push hl ; immutable destination start, including a possible page-end copy
 ld h, b
 ld l, c
 rept 64
 ld a, [hli]
 ld [de], a
 inc e
 endr
 pop hl
 ret
.peer_appearance
 for peer, 1, MOBILE_PEERS + 1
 dw wMobilePeer{d:peer} + 18
 endr
.cache_bases
 for peer, 1, MOBILE_PEERS + 1
 dw wMobileAvatarCache{d:peer}
 endr
ASSERT BANK(MobileVirtualCopyCanonicalPose) == BANK(MobileVirtualAssetPointers)
POPS

PUSHS
SECTION "Mobile Compact Peer Projection", ROMX, BANK[MOBILE_ROM_BANK + 5]
MobileCompactProjectPeers:
 xor a
 for peer, 1, MOBILE_PEERS + 1
 ld [wMobilePeer{d:peer}Visible], a
 endr
 ; Combine the old projection and drawing scope checks once per snapshot.
 ld a, [wMobileLocalStaging]
 ld b, a
 ld a, [wMobileVirtualMapGroup]
 cp b
 ret nz
 ld a, [wMobileLocalStaging + 1]
 ld b, a
 ld a, [wMobileVirtualMapNumber]
 cp b
 ret nz
 ld a, [wMobileLocalValid]
 and a
 ret z

 ; Slot 1: exact accepted scope and one immutable current-camera projection.
 ld a, [wMobilePeerCount]
 cp 1
 jp c, .hidden1
 ld a, [wMobilePeer1 + 6]
 ld b, a
 ld a, [wMobileLocalStaging]
 cp b
 jp nz, .hidden1
 ld a, [wMobilePeer1 + 7]
 ld b, a
 ld a, [wMobileLocalStaging + 1]
 cp b
 jp nz, .hidden1
 ld a, [wMobilePeer1 + 8]
 ld b, a
 ld a, [wMobileInstanceKey]
 cp b
 jp nz, .hidden1
 ld a, [wMobilePeer1 + 9]
 ld b, a
 ld a, [wMobileInstanceKey + 1]
 cp b
 jp nz, .hidden1
 ld a, [wMobilePeer1 + 17]
 cp 1
 jp nz, .hidden1
 ld a, [wMobilePeer1 + MOBILE_PEER_DRAW_VALID]
 and a
 jr z, .snap1
 ld a, [wMobilePeer1 + 12]
 ld b, a
 ld a, [wMobilePeer1 + MOBILE_PEER_DRAW_X + 1]
 cp b
 jr nz, .smooth1
 ld a, [wMobilePeer1 + 13]
 ld b, a
 ld a, [wMobilePeer1 + MOBILE_PEER_DRAW_X]
 cp b
 jr nz, .smooth1
 ld a, [wMobilePeer1 + 14]
 ld b, a
 ld a, [wMobilePeer1 + MOBILE_PEER_DRAW_Y + 1]
 cp b
 jr nz, .smooth1
 ld a, [wMobilePeer1 + 15]
 ld b, a
 ld a, [wMobilePeer1 + MOBILE_PEER_DRAW_Y]
 cp b
 jr nz, .smooth1
 jr .project1
.smooth1
 ld hl, wMobilePeer1 + 12
 ld de, wMobilePeer1 + MOBILE_PEER_DRAW_X
 call MobileCompactApproachPixel
 ld hl, wMobilePeer1 + 14
 ld de, wMobilePeer1 + MOBILE_PEER_DRAW_Y
 call MobileCompactApproachPixel
 jr .project1
.snap1
 ld a, 1
 ld [wMobilePeer1 + MOBILE_PEER_DRAW_VALID], a
 ld a, [wMobilePeer1 + 12]
 ld [wMobilePeer1 + MOBILE_PEER_DRAW_X + 1], a
 ld a, [wMobilePeer1 + 13]
 ld [wMobilePeer1 + MOBILE_PEER_DRAW_X], a
 ld a, [wMobilePeer1 + 14]
 ld [wMobilePeer1 + MOBILE_PEER_DRAW_Y + 1], a
 ld a, [wMobilePeer1 + 15]
 ld [wMobilePeer1 + MOBILE_PEER_DRAW_Y], a
.project1
 ld a, [wMobileCameraX]
 ld c, a
 ld a, [wMobilePeer1 + MOBILE_PEER_DRAW_X]
 sub c
 ld e, a
 ld a, [wMobileCameraX + 1]
 ld c, a
 ld a, [wMobilePeer1 + MOBILE_PEER_DRAW_X + 1]
 sbc c
 ld d, a
 ld a, e
 add 8
 ld e, a
 ld a, d
 adc 0
 and a
 jr nz, .hidden1
 ld a, e
 cp 168
 jr nc, .hidden1
 ld [wMobilePeer1 + MOBILE_PEER_SCREEN_X], a
 ld a, [wMobileCameraY]
 ld c, a
 ld a, [wMobilePeer1 + MOBILE_PEER_DRAW_Y]
 sub c
 ld e, a
 ld a, [wMobileCameraY + 1]
 ld c, a
 ld a, [wMobilePeer1 + MOBILE_PEER_DRAW_Y + 1]
 sbc c
 ld d, a
 ld a, e
 add 12
 ld e, a
 ld a, d
 adc 0
 and a
 jr nz, .hidden1
 ld a, e
 cp 160
 jr nc, .hidden1
 ld [wMobilePeer1 + MOBILE_PEER_SCREEN_Y], a
 ld a, 1
 ld [wMobilePeer1 + MOBILE_PEER_VISIBLE], a
 ld hl, wMobileVisibleCount
 inc [hl]
.hidden1

 ; Slot 2: exact accepted scope and one immutable current-camera projection.
 ld a, [wMobilePeerCount]
 cp 2
 jp c, .hidden2
 ld a, [wMobilePeer2 + 6]
 ld b, a
 ld a, [wMobileLocalStaging]
 cp b
 jp nz, .hidden2
 ld a, [wMobilePeer2 + 7]
 ld b, a
 ld a, [wMobileLocalStaging + 1]
 cp b
 jp nz, .hidden2
 ld a, [wMobilePeer2 + 8]
 ld b, a
 ld a, [wMobileInstanceKey]
 cp b
 jp nz, .hidden2
 ld a, [wMobilePeer2 + 9]
 ld b, a
 ld a, [wMobileInstanceKey + 1]
 cp b
 jp nz, .hidden2
 ld a, [wMobilePeer2 + 17]
 cp 1
 jp nz, .hidden2
 ld a, [wMobilePeer2 + MOBILE_PEER_DRAW_VALID]
 and a
 jr z, .snap2
 ld a, [wMobilePeer2 + 12]
 ld b, a
 ld a, [wMobilePeer2 + MOBILE_PEER_DRAW_X + 1]
 cp b
 jr nz, .smooth2
 ld a, [wMobilePeer2 + 13]
 ld b, a
 ld a, [wMobilePeer2 + MOBILE_PEER_DRAW_X]
 cp b
 jr nz, .smooth2
 ld a, [wMobilePeer2 + 14]
 ld b, a
 ld a, [wMobilePeer2 + MOBILE_PEER_DRAW_Y + 1]
 cp b
 jr nz, .smooth2
 ld a, [wMobilePeer2 + 15]
 ld b, a
 ld a, [wMobilePeer2 + MOBILE_PEER_DRAW_Y]
 cp b
 jr nz, .smooth2
 jr .project2
.smooth2
 ld hl, wMobilePeer2 + 12
 ld de, wMobilePeer2 + MOBILE_PEER_DRAW_X
 call MobileCompactApproachPixel
 ld hl, wMobilePeer2 + 14
 ld de, wMobilePeer2 + MOBILE_PEER_DRAW_Y
 call MobileCompactApproachPixel
 jr .project2
.snap2
 ld a, 1
 ld [wMobilePeer2 + MOBILE_PEER_DRAW_VALID], a
 ld a, [wMobilePeer2 + 12]
 ld [wMobilePeer2 + MOBILE_PEER_DRAW_X + 1], a
 ld a, [wMobilePeer2 + 13]
 ld [wMobilePeer2 + MOBILE_PEER_DRAW_X], a
 ld a, [wMobilePeer2 + 14]
 ld [wMobilePeer2 + MOBILE_PEER_DRAW_Y + 1], a
 ld a, [wMobilePeer2 + 15]
 ld [wMobilePeer2 + MOBILE_PEER_DRAW_Y], a
.project2
 ld a, [wMobileCameraX]
 ld c, a
 ld a, [wMobilePeer2 + MOBILE_PEER_DRAW_X]
 sub c
 ld e, a
 ld a, [wMobileCameraX + 1]
 ld c, a
 ld a, [wMobilePeer2 + MOBILE_PEER_DRAW_X + 1]
 sbc c
 ld d, a
 ld a, e
 add 8
 ld e, a
 ld a, d
 adc 0
 and a
 jr nz, .hidden2
 ld a, e
 cp 168
 jr nc, .hidden2
 ld [wMobilePeer2 + MOBILE_PEER_SCREEN_X], a
 ld a, [wMobileCameraY]
 ld c, a
 ld a, [wMobilePeer2 + MOBILE_PEER_DRAW_Y]
 sub c
 ld e, a
 ld a, [wMobileCameraY + 1]
 ld c, a
 ld a, [wMobilePeer2 + MOBILE_PEER_DRAW_Y + 1]
 sbc c
 ld d, a
 ld a, e
 add 12
 ld e, a
 ld a, d
 adc 0
 and a
 jr nz, .hidden2
 ld a, e
 cp 160
 jr nc, .hidden2
 ld [wMobilePeer2 + MOBILE_PEER_SCREEN_Y], a
 ld a, 1
 ld [wMobilePeer2 + MOBILE_PEER_VISIBLE], a
 ld hl, wMobileVisibleCount
 inc [hl]
.hidden2

 ; Slot 3: exact accepted scope and one immutable current-camera projection.
 ld a, [wMobilePeerCount]
 cp 3
 jp c, .hidden3
 ld a, [wMobilePeer3 + 6]
 ld b, a
 ld a, [wMobileLocalStaging]
 cp b
 jp nz, .hidden3
 ld a, [wMobilePeer3 + 7]
 ld b, a
 ld a, [wMobileLocalStaging + 1]
 cp b
 jp nz, .hidden3
 ld a, [wMobilePeer3 + 8]
 ld b, a
 ld a, [wMobileInstanceKey]
 cp b
 jp nz, .hidden3
 ld a, [wMobilePeer3 + 9]
 ld b, a
 ld a, [wMobileInstanceKey + 1]
 cp b
 jp nz, .hidden3
 ld a, [wMobilePeer3 + 17]
 cp 1
 jp nz, .hidden3
 ld a, [wMobilePeer3 + MOBILE_PEER_DRAW_VALID]
 and a
 jr z, .snap3
 ld a, [wMobilePeer3 + 12]
 ld b, a
 ld a, [wMobilePeer3 + MOBILE_PEER_DRAW_X + 1]
 cp b
 jr nz, .smooth3
 ld a, [wMobilePeer3 + 13]
 ld b, a
 ld a, [wMobilePeer3 + MOBILE_PEER_DRAW_X]
 cp b
 jr nz, .smooth3
 ld a, [wMobilePeer3 + 14]
 ld b, a
 ld a, [wMobilePeer3 + MOBILE_PEER_DRAW_Y + 1]
 cp b
 jr nz, .smooth3
 ld a, [wMobilePeer3 + 15]
 ld b, a
 ld a, [wMobilePeer3 + MOBILE_PEER_DRAW_Y]
 cp b
 jr nz, .smooth3
 jr .project3
.smooth3
 ld hl, wMobilePeer3 + 12
 ld de, wMobilePeer3 + MOBILE_PEER_DRAW_X
 call MobileCompactApproachPixel
 ld hl, wMobilePeer3 + 14
 ld de, wMobilePeer3 + MOBILE_PEER_DRAW_Y
 call MobileCompactApproachPixel
 jr .project3
.snap3
 ld a, 1
 ld [wMobilePeer3 + MOBILE_PEER_DRAW_VALID], a
 ld a, [wMobilePeer3 + 12]
 ld [wMobilePeer3 + MOBILE_PEER_DRAW_X + 1], a
 ld a, [wMobilePeer3 + 13]
 ld [wMobilePeer3 + MOBILE_PEER_DRAW_X], a
 ld a, [wMobilePeer3 + 14]
 ld [wMobilePeer3 + MOBILE_PEER_DRAW_Y + 1], a
 ld a, [wMobilePeer3 + 15]
 ld [wMobilePeer3 + MOBILE_PEER_DRAW_Y], a
.project3
 ld a, [wMobileCameraX]
 ld c, a
 ld a, [wMobilePeer3 + MOBILE_PEER_DRAW_X]
 sub c
 ld e, a
 ld a, [wMobileCameraX + 1]
 ld c, a
 ld a, [wMobilePeer3 + MOBILE_PEER_DRAW_X + 1]
 sbc c
 ld d, a
 ld a, e
 add 8
 ld e, a
 ld a, d
 adc 0
 and a
 jr nz, .hidden3
 ld a, e
 cp 168
 jr nc, .hidden3
 ld [wMobilePeer3 + MOBILE_PEER_SCREEN_X], a
 ld a, [wMobileCameraY]
 ld c, a
 ld a, [wMobilePeer3 + MOBILE_PEER_DRAW_Y]
 sub c
 ld e, a
 ld a, [wMobileCameraY + 1]
 ld c, a
 ld a, [wMobilePeer3 + MOBILE_PEER_DRAW_Y + 1]
 sbc c
 ld d, a
 ld a, e
 add 12
 ld e, a
 ld a, d
 adc 0
 and a
 jr nz, .hidden3
 ld a, e
 cp 160
 jr nc, .hidden3
 ld [wMobilePeer3 + MOBILE_PEER_SCREEN_Y], a
 ld a, 1
 ld [wMobilePeer3 + MOBILE_PEER_VISIBLE], a
 ld hl, wMobileVisibleCount
 inc [hl]
.hidden3
 ret

MobileCompactApproachPixel:
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


POPS

PUSHS
SECTION "Mobile Compact Cold Job", ROMX, BANK[MOBILE_ROM_BANK + 1]
MobileCompactPrepareColdCohort:
 ; One admitted foreground batch owns CPU through exact prevalidation. Each
 ; constructor consumes a generic slot; no generic upload dispatch is entered.
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 xor a
 ld [wMobileStablePoseCanceled], a
 inc a
 ld [wMobileStablePosePreparing], a
 ld [wMobileBudgetOptional], a ; one construction lifetime for the whole cohort
for peer, 1, MOBILE_PEERS + 1
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 ld a, [wMobilePeer{d:peer}Visible]
 and a
 jr z, .next{d:peer}
 ld a, [wMobilePeer{d:peer}NativeStage]
 cp 7
 jr z, .next{d:peer}
 ld a, [wMobileVirtualQueuedPeer]
 bit peer - 1, a
 jr nz, .next{d:peer}
 ld a, peer - 1
 ld [wMobileBudgetIndex], a
 call MobileCompactColdUpload
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileStablePoseCanceled]
 and a
 jr nz, .finish
.next{d:peer}
endr
.finish
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 xor a
 ld [wMobileBudgetOptional], a
 ld [wMobileStablePosePreparing], a
 ret

MobileCompactColdUpload:
 ld a, [wMobileBudgetIndex]
 ld b, a
 ld a, [wMobilePeerCount]
 cp b
 ret z
 ret c
 ld a, [wMobileWanted]
 and a
 ret z
 ld a, [wMobileOnline]
 and a
 ret z
 ld a, [wMobileBudgetIndex]
 ld e, a
 ld d, 0
 ld hl, wMobileVirtualWindowOwners
 add hl, de
 ld a, [hl]
 and a
 ret nz ; this constructor never overwrites an existing private tile lease
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileStableSourceValid]
 cp 1
 jp nz, .early_return
 ld a, [wMobileStableActorViewValid]
 cp 1
 jp nz, .early_return
 ld a, [wMobileBudgetOptional]
 and a
 jr nz, .construction_lifetime_ready
 xor a
 ld [wMobileStablePoseCanceled], a
 inc a
 ld [wMobileStablePosePreparing], a
.construction_lifetime_ready
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 call .peer
 ld bc, 18
 add hl, bc
 ld a, [hli]
 ld [wMobileVirtualQueuedAvatar], a
 ld b, a
 add a
 add b
 add a
 add [hl]
 ld e, a
 ld d, 0
 ld a, [hl]
 ld [wMobileVirtualQueuedMovement], a
 ld hl, MobileVirtualAssetSprites
 add hl, de
 ld a, [hl] ; immutable assets share this upper bank
 ld [wMobileVirtualQueuedSprite], a
 push af
 call .peer
 ld bc, 33
 add hl, bc
 pop af
 ld [hli], a
 ld [hl], 1 ; immutable job pending, not a resident GPU pose
 call MobileCompactColdPalette
 jp c, .failed
 call .peer
 ld bc, 16
 add hl, bc
 ld a, [hl]
 ld [wMobileAvatarTile], a
 ld [wMobileVirtualQueuedFacing], a
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
 ld [wMobileVirtualQueuedFrame], a
 ld a, [wMobileBudgetIndex]
 ld [wMobileVirtualQueuedWindow], a
 call MobileVirtualCopyCanonicalPose
 ld a, l
 ld [wMobileVirtualQueuedSource], a
 ld a, h
 ld [wMobileVirtualQueuedSource + 1], a
 ; Bind the exact full identity, separately from facing/avatar/movement.
 call .peer
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
 ld a, [wMobileBudgetIndex]
 add a
 add a
 ld e, a
 ld d, 0
 ld hl, wMobileStableRemoteBindings
 add hl, de
 pop de
 pop bc
 ld a, b
 ld [hli], a
 ld a, c
 ld [hli], a
 ld a, d
 ld [hli], a
 ld [hl], e
 ld a, [wMobileBudgetIndex]
 add a
 add a
 add a
 ld e, a
 ld d, 0
 ld hl, wMobileStablePoseBatch
 add hl, de
 ld d, h
 ld e, l
 ld hl, wMobileVirtualQueuedWindow
 ld bc, 8
 rst CopyBytes
 ld hl, wMobileStablePoseGeneration
 inc [hl]
 ld a, [wMobileBudgetIndex]
 ld e, a
 ld d, 0
 ld hl, .bits
 add hl, de
 ld a, [wMobileVirtualQueuedPeer]
 or [hl]
 ld [wMobileVirtualQueuedPeer], a
 ld a, [wMobileStablePoseCanceled]
 and a
 jr nz, .failed
 ld a, [wMobileBudgetOptional]
 and a
 jr nz, .early_return
 xor a
 ld [wMobileStablePosePreparing], a
.early_return
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 ret
.failed
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, 1
 ld [wMobileStablePoseCanceled], a
 xor a
 ld [wMobileVirtualQueuedPeer], a
 ld [wMobileStablePosePreparing], a
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 call .peer
 ld bc, MOBILE_PEER_NATIVE_STAGE
 add hl, bc
 ld [hl], 0
 ret
.peer
 ld a, [wMobileBudgetIndex]
 add a
 ld e, a
 ld d, 0
 ld hl, .peers
 add hl, de
 ld a, [hli]
 ld h, [hl]
 ld l, a
 ret
.peers
 for peer, 1, MOBILE_PEERS + 1
 dw wMobilePeer{d:peer}
 endr
.bits
 db 1, 2, 4
ASSERT BANK(MobileCompactColdUpload) == BANK(MobileVirtualAssetPointers)

; Cold canonical RGB uses the native read-only selector directly. It neither
; allocates a native palette nor writes either native palette buffer. Only
; wPalState and wNeededObjPalGlow are selector inputs temporarily replaced.
MobileCompactColdPalette:
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wPalFadeDelayFrames]
 and a
 jr nz, .failed
 ld a, 1
 ldh [rWBK], a
 ld a, [wPalState]
 push af
 ld a, [wNeededObjPalGlow]
 push af
 ld a, CURR_PALSTATE
 ld [wPalState], a
 xor a
 ld [wNeededObjPalGlow], a
 ld a, [wMobileVirtualQueuedAvatar]
 farcall LookupOBPalette
 pop af
 ld [wNeededObjPalGlow], a
 pop af
 ld [wPalState], a
 push hl
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileBudgetIndex]
 add a
 add a
 add a
 add LOW(wMobilePrivatePalColors)
 ld e, a
 adc HIGH(wMobilePrivatePalColors)
 sub e
 ld d, a
 pop hl
 ld bc, 8
 ld a, BANK(LookupOBPalette)
 call FarCopyBytes
 ld a, [wMobileBudgetIndex]
 ld e, a
 ld d, 0
 ld hl, wMobilePrivatePalIdentities
 add hl, de
 ld a, [wMobileVirtualQueuedAvatar]
 ld [hl], a
 ld hl, .bits
 add hl, de
 ld a, [hl]
 cpl
 ld hl, wMobilePrivatePalDirty
 and [hl]
 ld [hl], a
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 call MobileCompactColdUpload.peer
 ld bc, 54
 add hl, bc
 ld [hl], 1
 and a
 ret
.failed
 scf
 ret
.bits
 db 1,2,4
ASSERT BANK(MobileCompactColdPalette) == BANK(MobileVirtualAssetPointers)
POPS

PUSHS
SECTION "Mobile Fresh ROM Resources", ROMX, BANK[MOBILE_ROM_BANK + 1]
; This entry creates all records from live authenticated peers and consumes
; them synchronously. Serial is masked by NativeIdle; native prewrite guards
; keep cancellation sticky throughout selector, construction and transfer.
MobileCompactFreshResources::
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 xor a
 ld [wMobileVirtualQueuedPeer], a ; no old queued record inherits this proof
 ld [wMobileStablePoseCanceled], a
 inc a
 ld [wMobileStablePosePreparing], a
 ld a, [wPalFadeDelayFrames]
 and a
 jp nz, .failed
 ld a, 1
 ldh [rWBK], a
 ld a, [wPalState]
 push af
 ld a, [wNeededObjPalGlow]
 push af
 ld a, CURR_PALSTATE
 ld [wPalState], a
 xor a
 ld [wNeededObjPalGlow], a
 farcall LookupOBPalette
 pop af
 ld [wNeededObjPalGlow], a
 pop af
 ld [wPalState], a
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, l
 ld [wMobileStableFreshPaletteBase], a
 ld a, h
 ld [wMobileStableFreshPaletteBase + 1], a
 ; LookupOBPalette's time-dependent domain is contiguous in every endpoint.
 ASSERT NUM_PLAYER_GENDERS <= NUM_OW_TIME_OF_DAY_PALS
 ASSERT NUM_PLAYER_STATES == 6
 ASSERT NUM_PLAYER_GENDERS * NUM_PLAYER_STATES * 16 <= $200
for peer, 1, MOBILE_PEERS + 1
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 ld a, [wMobilePeer{d:peer}Visible]
 and a
 jp z, .next{d:peer}
 ld a, [wMobilePeer{d:peer}NativeStage]
 cp 7
 jp z, .next{d:peer}
 ld a, [wMobilePeerCount]
 cp peer
 jp c, .failed
 ld a, [wMobilePeer{d:peer}Avatar]
 cp NUM_PLAYER_GENDERS
 jp nc, .failed
 ld b, a
 ld a, [wMobilePeer{d:peer}Movement]
 cp NUM_PLAYER_STATES
 jp nc, .failed
 ld c, a
 ld a, b
 add a
 add b
 add a
 add c
 ld l, a
 ld h, 0
 rept 4
 add hl, hl
 endr
 ld de, MobileVirtualPoseDescriptors
 add hl, de
 ld a, [hli]
 ld [wMobilePeer{d:peer}NativeSprite], a
 inc hl
 push hl
 ld a, [wMobilePeer{d:peer}Facing]
 ld b, a
 and $c
 cp $c
 jr nz, .facing{d:peer}
 ld a, 8
.facing{d:peer}
 bit 0, b
 jr z, .standing{d:peer}
 add 12
.standing{d:peer}
 ld c, a
 ; frame is exactly one of 0,4,8,12,16,20. Descriptor pointers are two bytes.
 rrca
 ld e, a
 ld d, 0
 add hl, de
 ld a, [hli]
 ld e, a
 ld d, [hl]
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld hl, wMobileStablePoseBatch + (peer - 1) * 8
 ld a, peer - 1
 ld [hli], a
 ld a, e
 ld [hli], a
 ld a, d
 ld [hli], a
 ld a, c
 ld [hli], a
 ld a, b
 ld [hli], a
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 ld a, [wMobilePeer{d:peer}Avatar]
 ld b, a
 ld a, [wMobilePeer{d:peer}Movement]
 ld c, a
 pop hl
 dec hl
 dec hl
 ld d, [hl]
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, b
 ld [wMobileStablePoseBatch + (peer - 1) * 8 + 6], a
 ld a, d
 ld [wMobileStablePoseBatch + (peer - 1) * 8 + 5], a
 ld a, c
 ld [wMobileStablePoseBatch + (peer - 1) * 8 + 7], a
 ; Full player and generation identity, copied while no peer writer can run.
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 ld hl, wMobilePeer{d:peer}
 ld a, [hli]
 ld b, a
 ld a, [hli]
 ld c, a
 ld a, [hli]
 ld d, a
 ld e, [hl]
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld hl, wMobileStableRemoteBindings + (peer - 1) * 4
 ld a, b
 ld [hli], a
 ld a, c
 ld [hli], a
 ld a, d
 ld [hli], a
 ld [hl], e
 ; Exact receiving-lighting RGB, one native lookup for the common domain.
 ld a, [wMobileStablePoseBatch + (peer - 1) * 8 + 6]
 ld [wMobilePrivatePalIdentities + peer - 1], a
 add a
 add a
 add a
 ld e, a
 ld d, 0
 ld a, [wMobileStableFreshPaletteBase]
 ld l, a
 ld a, [wMobileStableFreshPaletteBase + 1]
 ld h, a
 add hl, de
 ld de, wMobilePrivatePalColors + (peer - 1) * 8
 ld bc, 8
 ld a, BANK(LookupOBPalette)
 call FarCopyBytes
 ld a, [wMobilePrivatePalDirty]
 res peer - 1, a
 ld [wMobilePrivatePalDirty], a
 ld a, [wMobileVirtualQueuedPeer]
 set peer - 1, a
 ld [wMobileVirtualQueuedPeer], a
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 ld a, 1
 ld [wMobilePeer{d:peer}PrivatePalValid], a
.next{d:peer}
endr
MobileCompactFreshResources.constructed::
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld hl, wMobileStablePoseGeneration
 inc [hl]
 ld a, [hl]
 ld [wMobileStablePoseToken], a
 ld a, [wMobileVirtualQueuedPeer]
 ld [wMobileStablePoseMask], a
 call MobileCompactFreshPrepare
 jr c, .failed
 xor a
 ld [wMobileStablePosePreparing], a
 and a
 ret
.failed
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 xor a
 ld [wMobileVirtualQueuedPeer], a
 ld [wMobileStablePosePreparing], a
 scf
 ret
ASSERT BANK(MobileCompactFreshResources) == BANK(MobileVirtualPoseDescriptors)
POPS
