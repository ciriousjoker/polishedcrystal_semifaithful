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
; Every frame must advance the bounded transport state, including response
; parsing and command deadlines. A scene that cannot finish must not repeatedly
; reserve its first idle slice and starve a live connection. Rendering uses the
; remaining slices under its unchanged admission deadlines.
.first_transport
	call MobileTick
	jp .done
.sample
; A dense native frame leaves a much smaller idle window. Once a complete
; guest exists, reserve three of four late windows for presentation; the fourth
; advances optional capture/pose/projection jobs. Early scenes keep their normal
; per-frame policy. Transport still receives its first poll on every frame.
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
	ld a, [wMobileVirtualDescriptors + 6]
	and $1f
	cp 5
	jr nc, .deadline
	ld b, 139 ; bounded four-quad local figure, including exact row rollback
	jr .deadline
.native_deadline
; Each native group applies its own shape deadline before doing expensive
; admission. Empty descriptors must not inherit the sixteen-quad deadline.
	ld b, 142 ; late calls may only drain cheap rejections and publish bookkeeping
	jr .deadline
.start_deadline
	ld a, [wMobileVirtualStage]
	cp 1
	jr nz, .shape_deadline
	ld b, 133
.shape_deadline
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jr nz, .deadline
	ld a, [wMobileVirtualStage]
	cp 3
	jr z, .small ; peer preparation has a larger bound than a raw native group
	cp 1
	jr nz, .canonical_group_deadline
	ld b, 133 ; full metadata audit and aligned proof also need caller-return time
	jr .deadline
.canonical_group_deadline
	ld b, 140
	jr .deadline
.publication
	ld b, 141 ; bounded suffix stores, bindings, commit and complete caller return
	jr .deadline
.small
	ld b, 138 ; one peer has a measured bound below six double-speed scanlines
	jr .deadline
.weather_deadline
	ld b, 140
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
	ld a, [wMobileVirtualQueuedPeer]
	and a
	ret nz ; source and identity remain immutable until the hardware commit
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
	pop hl
	ret

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
	call MobileVirtualExactPalette
	jr c, .construct_color
	push af
	ld bc, 54 - 18
	add hl, bc
	ld [hl], 2 ; native palette uploads already keep this exact source current
	pop af
	jr .color_ready
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
	inc a
	ld [wMobileVirtualQueuedPeer], a ; publish the complete transaction last
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
	pop hl
	ret

MobileVirtualExactPalette:
; A native base color, C last hardware slot. Revalidate an exact ordinary
; source without rebuilding its private copy. Native palette metadata, fades and
; glow state stay authoritative. A changed or unavailable source takes the full
; isolated constructor path. Preserve the peer pointer and selected WRAM bank.
	push af
	push hl
	push de
	ld b, a
	ldh a, [rWBK]
	push af
	ld a, 1
	ldh [rWBK], a
	ld a, c
	cp 8
	jr nc, .scan
	call .check_slot
	jr nc, .found
.scan
; Native palette slots can move as ordinary NPCs load. Find an existing exact
; color before invoking the isolated constructor and copying a private cache.
	ld c, 0
.slot
	call .check_slot
	jr nc, .found
	inc c
	ld a, c
	cp 8
	jr c, .slot
	jr .missing
.found
	pop af
	ldh [rWBK], a
	pop de
	pop hl
	pop af
	ld a, c
	and a
	ret
.check_slot
	ld a, c
	cp PAL_OW_WEATHER
	jr nz, .ordinary_color
	ld a, [wCurWeather]
	ld d, a
	ld a, [wOverworldWeatherCooldown]
	or d
	jr nz, .slot_missing
.ordinary_color
	ld d, 0
	ld hl, wLoadedObjPal0
	ld e, c
	add hl, de
	ld a, b
	cp [hl]
	jr nz, .slot_missing
	ld hl, wLoadedObjPalGlows
	add hl, de
	ld a, [hl]
	ld hl, wLoadedObjPalPrevGlows
	add hl, de
	or [hl]
	jr nz, .slot_missing
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [wLoadedObjPalType]
	and [hl]
	jr nz, .slot_missing
	and a
	ret
.slot_missing
	scf
	ret
.missing
	pop af
	ldh [rWBK], a
	pop de
	pop hl
	pop af
	scf
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
	ld [wMobileVirtualQueuedPeer], a
	ld [wMobileVirtualForceFull], a
	ld [wMobileVirtualBaseReady], a
	ld a, $80
	ld [wMobileVirtualNeedColorAudit], a
	pop hl
	pop bc
	pop af
	ret
