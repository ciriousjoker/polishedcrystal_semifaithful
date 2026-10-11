; Native remote actors with protected-default NPC leases and bounded idle jobs.
MobileNativeActorStep::
; Ordinary map actors take the short path. Only temporary actors need a lease scan.
	ld hl, OBJECT_MAP_OBJECT_INDEX
	add hl, bc
	ld a, [hl]
	cp TEMP_OBJECT
	jr z, .temporary
	and a
	ret
.temporary
	push de
	push bc
	ldh a, [hMapObjectIndexBuffer]
	ld b, a
	ld hl, wMobileSavedFacings
	ld c, MOBILE_PEERS
.scan
	ld a, [hli]
	and a
	jr z, .next
	cp b
	jr z, .owned
.next
	dec c
	jr nz, .scan
	pop bc
	pop de
	and a
	ret
.owned
	pop bc
	pop de
	ld hl, OBJECT_FLAGS2
	add hl, bc
	ld [hl], HIGH_PRIORITY | FROZEN
	scf
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
	ldh a, [rLY]
	cp 116
	jr c, .retry
	ld a, [wMobileWire]
	and a
	jr z, .sleep
	cp MOBILE_WIRE_DONE
	jr nc, .sleep
.retry
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
	jr z, .return
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
; Transport interrupts run in the enclosing wait, outside every background job.
	ld a, [wMobileOwner]
	and a
	jr z, .clock
	ldh a, [rIE]
	and ~IE_SERIAL
	ldh [rIE], a
.clock
	ldh a, [rLY]
	ld [wMobileClippedWeather], a
; Native graphics requests own their copy registers and their frame wait.
; hNbTilesToCopy aliases the persistent BG-map row count, so it cannot identify
; an active graphics request. Only the actual pending copy counts may gate us.
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
.admit
	ldh a, [rLY]
	cp 120
	jr c, .capture
	cp 136
	jp nc, .done
; Late idle time permits only small transport transitions. CRC, application
; validation and request preparation retain the earlier admission boundary.
	ld a, [wMobileWire]
	and a
	jp z, .done
	cp MOBILE_WIRE_DONE
	jp nc, .done
	jr .pump
.capture
	ldh a, [rLY]
	cp 116
	jr nc, .pump
	ld a, [wMobileOwner]
	and a
	jr z, .pump ; do not schedule an empty capture ahead of the initial join
; Read a coherent native state only while the game is waiting, once per frame.
; A capture and an expensive packet phase never share one admission window.
	ldh a, [hVBlankCounter]
; Alternate capture frames with native actor work so a dense map cannot let
; continuous transport sampling consume every available position-job window.
	bit 0, a
	jr nz, .pump
	ld b, a
	ld a, [wMobileColdFrame]
	cp b
	jr z, .pump
	ld a, b
	ld [wMobileColdFrame], a
	ldh a, [rIE]
	and ~IE_SERIAL
	ldh [rIE], a
	call MobileCapture
	jp .done
.pump
; Prepare one bounded transport phase with serial dispatch suspended. The
; enclosing wait resumes byte IRQs after this job has returned.
	call MobileTick
	ldh a, [rIE]
	and ~IE_SERIAL
	ldh [rIE], a
	ldh a, [hVBlankOccurred]
	and a
	jp nz, .done
	ldh a, [rLY]
	cp 116
	jp nc, .done
	ldh a, [hVBlankCounter]
	ld b, a
	ld a, [wMobileSavedFacings + NUM_OBJECT_STRUCTS - 1]
	cp b
	jp z, .prepare_admit
	ld a, b
	ld [wMobileSavedFacings + NUM_OBJECT_STRUCTS - 1], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld hl, wMobilePeer1
	ld de, wMobileAvatarCache1
	ld b, MOBILE_PEERS
	ld c, 0
.loop
	ldh a, [hVBlankOccurred]
	and a
	jr nz, .done
	ldh a, [rLY]
	cp 116
	jr nc, .done
; Completed leases have no upload work. Avoid three full call paths before
; admitting the position job when a dense native map leaves a short idle tail.
	push hl
	push bc
	ld bc, MOBILE_NATIVE_ROLE
	add hl, bc
	ld a, [hld]
	cp 2
	jr z, .upload_needed
	ld a, [hl]
	cp 7
	jr nc, .upload_complete
.upload_needed
	pop bc
	pop hl
	push bc
	push hl
	push de
	call MobileNativeUpload
	pop de
	pop hl
	pop bc
	jr c, .done
	jr .upload_next
.upload_complete
	pop bc
	pop hl
.upload_next
	push bc
	ld bc, MOBILE_PEER_SIZE
	add hl, bc
	push hl
	ld hl, 24 tiles
	add hl, de
	ld d, h
	ld e, l
	pop hl
	pop bc
	inc c
	dec b
	jr nz, .loop
.prepare_admit
	ldh a, [hVBlankOccurred]
	and a
	jr nz, .done
	ldh a, [rLY]
	cp 115
	jr nc, .done
	ldh a, [hVBlankCounter]
	ld b, a
	ld a, [wMobileNativePrepareFrame]
	cp b
	jr z, .done
	ld a, b
	ld [wMobileNativePrepareFrame], a
	call MobileNativePrepareSpritesWork
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

MobilePrepareSprites::
; Native menu actions mark temporary objects STANDING. Keep their last accepted
; facing, just as ordinary actors remain frozen, without processing new positions.
	ld a, [wMobileOnline]
	and a
	ret z
	ldh a, [rWBK]
	push af
	ld a, 1
	ldh [rWBK], a
	ld a, [wScriptRunning]
	and a
	jr z, .done
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld hl, wMobilePeer1NativeFacing
	ld de, wMobileSavedFacings
	ld b, MOBILE_PEERS
.loop
	ld a, [de]
	and a
	jr z, .next
	push bc
	push hl
	push de
	ld c, [hl]
	ld a, c
	cp NUM_FACINGS
	jr nc, .skip
	ld bc, 34 - 52
	add hl, bc
	ld c, a
	inc hl
	ld a, [hl] ; native role
	cp 2
	jr z, .skip
	ld a, [de]
	push bc
	call MobileNativeOwnSlot
	pop bc
	jr nc, .skip
	push bc
	call MobileNativeObjectPointer
	ld hl, OBJECT_FLAGS1
	add hl, bc
	ld a, 1
	ldh [rWBK], a
	bit INVISIBLE_F, [hl]
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	jr z, .menu_visible
	pop bc
	jr .skip
.menu_visible
	ld hl, OBJECT_FACING
	add hl, bc
	pop bc
	ld a, 1
	ldh [rWBK], a
	ld [hl], c
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
.skip
	pop de
	pop hl
	pop bc
.next
	inc de
	push bc
	ld bc, MOBILE_PEER_SIZE
	add hl, bc
	pop bc
	dec b
	jr nz, .loop
.done
	pop af
	ldh [rWBK], a
	ret

MobileNativePrepareSpritesWork:
	ldh a, [rWBK]
	push af
; Menus freeze the native world. Retain the complete last accepted visual state.
; Offline cleanup still runs here so emergency exit cannot depend on a menu.
	ld a, [wMobileOnline]
	and a
	jr z, .active
	ld a, 1
	ldh [rWBK], a
	ld a, [wPanningAroundTinyMap]
	and a
	jr nz, .active
	ld a, [wScriptRunning]
	and a
	jp nz, .done
.active
	ld a, [wMobileBudgetCursor]
	and a
	jr nz, .keep_counts
	ld [wMobileVisibleCount], a
	ld [wMobileOAMDemand], a
	ld [wMobileDeferred], a
.keep_counts
	ld a, 1
	ldh [rWBK], a
	ld a, [wPanningAroundTinyMap]
	and a
	jr z, .ordinary_scene
	ld a, 3
	ld [wMobileDeferred], a
.ordinary_scene
	ld a, [wScriptRunning]
	ld [wMobileBudgetOptional], a
	ld a, [wPlayerBGMapOffsetX]
	ld [wMobileOAMReadCursor], a
	ld a, [wPlayerBGMapOffsetY]
	ld [wMobileOAMWriteCursor], a
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
	ld a, [wMobileBudgetCursor]
	ld c, a
; Alternate urgent and ordinary jobs. Even a continuously moving fast peer
; cannot starve camera updates or cleanup for the other two native actors.
	ldh a, [hVBlankCounter]
	bit 0, a
	jr nz, .regular
	ld a, [wMobileBudgetPriority]
	and a
	jr z, .regular
	dec a
	ld c, a
	xor a
	ld [wMobileBudgetPriority], a
.regular
	ld a, c
	push af
	call MobileNativePeerPointer
	pop af
	ld c, a
	call MobileNativePrepare
	ld a, [wMobileBudgetCursor]
	inc a
	cp MOBILE_PEERS
	jr c, .next_peer
	xor a
.next_peer
	ld [wMobileBudgetCursor], a
; Count current owned native objects, independently of urgent job order.
	ld hl, wMobileSavedFacings
	ld d, MOBILE_PEERS
	ld e, 0
.count
	ld a, [hli]
	and a
	jr z, .count_next
	push hl
	push de
	call MobileNativeObjectPointer
	ld a, 1
	ldh [rWBK], a
	inc bc
	ld a, [bc]
	cp TEMP_OBJECT
	jr nz, .not_visible
	ld hl, OBJECT_FACING - 1
	add hl, bc
	ld a, [hl]
	cp STANDING
	jr z, .not_visible
	pop de
	inc e
	jr .count_bank
.not_visible
	pop de
.count_bank
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	pop hl
.count_next
	dec d
	jr nz, .count
	ld a, e
	ld [wMobileRenderedCount], a
.done
	pop af
	ldh [rWBK], a
	ret

MobileNativePrepare:
; Bank 2, HL peer, C pool index. Resident slots keep native AI out of leases.
	ld a, c
	ld [wMobileBudgetIndex], a
	push hl
	ld bc, MOBILE_NATIVE_ROLE
	add hl, bc
	ld a, [hl]
	pop hl
	cp 2
	jp z, .release
	ld a, [wMobileOnline]
	and a
	jp z, .release
	ld a, [wMobileLocalValid]
	and a
	jp z, .release
	ld a, [wMobileBudgetIndex]
	ld c, a
	ld a, [wMobilePeerCount]
	cp c
	jp z, .release
	jp c, .release
	ld a, [wMobileBudgetOptional]
	and a
	ret nz
	call MobileProjectPeer
	jp c, .release
	push hl
	ld bc, MOBILE_PEER_VISIBLE
	add hl, bc
	ld a, [hl]
	pop hl
	and a
	jr nz, .visible
	push hl
	call .slot_pointer
	ld a, [hl]
	call MobileNativeOwnSlot
	jp nc, .offscreen_done
	call MobileNativeObjectPointer
	ld d, b
	ld e, c
	push de
	jp .hide_waiting
.offscreen_done
	pop hl
	ret
.visible
	push hl
	call .slot_pointer
	ld a, [hl]
	and a
	jr nz, .existing
	ld a, 1
	ldh [rWBK], a
	call MobileNativeLeaseSlot
	jr nc, .found
	pop hl
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, 1
	ld [wMobileDeferred], a
	ret

.found
	push de
	push bc
	ld h, b
	ld l, c
	ld bc, OBJECT_LENGTH
	xor a
	rst ByteFill
	pop bc
	ld a, SPRITE_CHRIS
	ld [bc], a
	ld hl, OBJECT_MAP_OBJECT_INDEX
	add hl, bc
	ld [hl], TEMP_OBJECT
	ld hl, OBJECT_FLAGS1
	add hl, bc
	ld [hl], INVISIBLE | WONT_DELETE | EMOTE_OBJECT
	inc hl
	ld [hl], HIGH_PRIORITY | FROZEN
	pop de
	ld a, e
	push af
	call .slot_pointer
	pop af
	ld [hl], a
.existing
	call MobileNativeOwnSlot
	jr c, .owned
	pop hl
	jmp MobileNativeReleaseLease
.owned
	push af
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, MOBILE_NATIVE_ROLE
	add hl, bc
	ld a, [hl]
	and a
	jr nz, .room
	ldh a, [hUsedOAMIndex]
	cp OAM_SIZE - 4 * OBJ_SIZE
	jr c, .room
	ld a, 1
	ldh [rWBK], a
	call MobileNativeLeaseSlot.demand
	cp OAM_SIZE + 1
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	jr c, .room
; Native actors can enter after joining. A free lease can migrate to an audited
; NPC slot when the OAM budget becomes full, without changing any story state.
	pop af
	pop hl
	jmp MobileNativeReleaseLease
.room
	pop af
	call MobileNativeObjectPointer
	ld d, b
	ld e, c
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	pop hl
	push hl
	push de
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
	pop de
	pop hl
	push hl
	push de
	ld bc, 34
	add hl, bc
	ld c, a
	ld a, [hl]
	cp 7
	jr z, .choose_sprite
	inc hl
	inc hl
	ld a, [hl]
	and a
	dec hl
	dec hl
	jr nz, .keep_target
.choose_sprite
	ld a, c
	dec hl
	cp [hl]
	jr z, .same_sprite
	ld [hl], a
	inc hl
	ld [hl], 0 ; upload stage
	inc hl
	inc hl
	ld [hl], 0 ; reset the private decoder on a sprite change
	dec hl
	dec hl
	dec hl
.same_sprite
	inc hl
.keep_target
	ld a, [hl]
	cp 7
	jr z, .ready
	ld bc, 18 - 34
	add hl, bc
	ld b, [hl]
	ld de, MOBILE_PEER_LOADED_SPRITE - 18
	add hl, de
	ld a, [hl]
	cp b
	pop de
	push de
	jr z, .same_figure
	ld a, 1
	ldh [rWBK], a
	ld h, d
	ld l, e
	ld bc, OBJECT_FLAGS1
	add hl, bc
	set INVISIBLE_F, [hl]
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	jp .waiting
.same_figure
	ld a, 1
	ldh [rWBK], a
	ld h, d
	ld l, e
	ld bc, OBJECT_FLAGS1
	add hl, bc
	bit INVISIBLE_F, [hl]
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	jp nz, .waiting
.ready
	pop de
	pop hl
	push hl
	push de
	ld bc, MOBILE_PEER_PALETTE
	add hl, bc
	ld c, [hl]
	ld de, 18 - MOBILE_PEER_PALETTE
	add hl, de
	ld a, [hl]
	farcall MobileNativePaletteGuard
	jp c, .waiting
	ld [wMobileAvatarPalette], a
	pop de
	pop hl
	push hl
	ld bc, MOBILE_PEER_PALETTE
	add hl, bc
	ld [hl], a
	pop hl
	push hl
	ld bc, MOBILE_PEER_SCREEN_X
	add hl, bc
	ld a, [hli]
	sub OAM_X_OFS
	ld b, a
	ld a, [wMobileOAMReadCursor]
	ld c, a
	ld a, b
	sub c
	ld [wMobileAvatarTile], a ; private temporary screen coordinate
	ld a, [hl]
	sub OAM_Y_OFS - 4
	ld b, a
	ld a, [wMobileOAMWriteCursor]
	ld c, a
	ld a, b
	sub c
	ld c, a
	pop hl
	push hl
	ld bc, 34
	add hl, bc
	ld a, [hl]
	cp 7
	jr nz, .old_facing
	ld bc, 16 - 34
	add hl, bc
	ld a, [hl]
	ld b, a
	and $c
	cp $c
	jr nz, .source_direction
	ld a, 8
.source_direction
	bit 0, b
	jr z, .source_standing
	add 12
.source_standing
	push bc
	ld bc, MOBILE_PEER_LOADED_FRAME - 16
	add hl, bc
	ld [hl], a
	pop bc
	push bc
	ld bc, 52 - MOBILE_PEER_LOADED_FRAME
	add hl, bc
	pop bc
	ld [hl], b
	jr .position
.old_facing
	ld bc, 52 - 34
	add hl, bc
	ld b, [hl]
.position
	ld a, 1
	ldh [rWBK], a
	ld h, d
	ld l, e
	ld a, [wMobileAvatarTile]
	push bc
	ld bc, OBJECT_SPRITE_X
	add hl, bc
	ld [hl], a
	inc hl
	; Recompute Y from the private peer while preserving the native pointer.
	push hl
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, MOBILE_PEER_SCREEN_Y
	add hl, bc
	ld a, [hl]
	sub OAM_Y_OFS - 4
	ld b, a
	ld a, [wMobileOAMWriteCursor]
	ld c, a
	ld a, b
	sub c
	ld b, a
	ld a, 1
	ldh [rWBK], a
	pop hl
	ld [hl], b
	inc hl
	xor a
	ld [hli], a
	ld [hl], a
	pop bc
	ld h, d
	ld l, e
	push bc
	ld bc, OBJECT_FACING
	add hl, bc
	pop bc
	ld [hl], b
	ld h, d
	ld l, e
	ld bc, OBJECT_PALETTE
	add hl, bc
	ld a, [wMobileAvatarPalette]
	ld [hl], a
	push de
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, 18
	add hl, bc
	ld b, [hl]
	ld a, 1
	ldh [rWBK], a
	pop de
	ld h, d
	ld l, e
	push bc
	ld bc, OBJECT_PAL_INDEX
	add hl, bc
	pop bc
	ld [hl], b
	ld h, d
	ld l, e
	ld bc, OBJECT_FLAGS1
	add hl, bc
	ld [hl], WONT_DELETE | NOCLIP_TILES | NOCLIP_OBJS | EMOTE_OBJECT
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	pop hl
	push hl
	ld bc, 34
	add hl, bc
	ld a, [hl]
	cp 7
	jr nz, .presented
	dec hl
	ld a, [hl]
	ld b, a
	ld a, 1
	ldh [rWBK], a
	ld a, b
	ld [de], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld bc, 18 - 33
	add hl, bc
	ld a, [hl]
	ld bc, MOBILE_PEER_LOADED_SPRITE - 18
	add hl, bc
	ld [hl], a
.presented
	pop hl
	ret
.waiting
.hide_waiting
	pop de
	pop hl
	ld a, 1
	ldh [rWBK], a
	ld h, d
	ld l, e
; STANDING alone hides one OAM build. Turn/menu preservation must also know
; this lease is hidden, so it cannot revive the last accepted native facing.
	ld bc, OBJECT_FLAGS1
	add hl, bc
	set INVISIBLE_F, [hl]
	ld bc, OBJECT_FACING - OBJECT_FLAGS1
	add hl, bc
	ld [hl], STANDING
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ret
.release
	jmp MobileNativeReleaseLease

.slot_pointer
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, wMobileSavedFacings
	add hl, de
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
; A pool index -> HL private peer.
	ld hl, wMobilePeer1
	ld bc, MOBILE_PEER_SIZE
.loop
	and a
	ret z
	add hl, bc
	dec a
	jr .loop

MobileNativeUpload:
; Bank 2, HL peer, DE cache, C pool. At most one eight-byte decode or four-tile copy per frame.
	ld a, c
	ld [wMobileBudgetIndex], a
	push hl
	ld bc, MOBILE_NATIVE_ROLE
	add hl, bc
	ld a, [hl]
	cp 2
	jp z, .restore
	cp 1
	jr nz, .avatar
	pop hl
	push hl
	push de
	farcall MobileNativeBackupTiles
	pop de
	jr nc, .avatar
	pop hl
	scf
	ret
.avatar
	pop hl
	push hl
	ld bc, 33
	add hl, bc
	ld a, [hli]
	and a
	jp z, .nothing
	ld b, a
	ld a, [hl]
	cp 7
	jp nc, .nothing
	and a
	jr nz, .chunk
; Stage zero owns one resumable decoder record per peer. Copy the small context
; to private scratch; neither native decompression state nor VBlank is borrowed.
	push hl ; stage
	push de ; cache
	push bc ; sprite
	ld bc, 2 ; stage 34 -> decoder state 36
	add hl, bc
	ld de, wMobileLZScratch
	ld bc, 16
	rst CopyBytes
	pop bc
	pop hl ; cache
	ld a, [wMobileLZBank]
	and a
	jr nz, .decode
	push hl
	ld a, b
	farcall GetSprite
	pop hl
	call MobileNativeLZStart
.decode
	call MobileNativeLZPump
	push af
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, 36
	add hl, bc
	ld d, h
	ld e, l
	ld hl, wMobileLZScratch
	ld bc, 16
	rst CopyBytes
	pop af
	pop hl ; stage
	jr c, .decoded
	jr nz, .decode_pending
	ld [hl], 255 ; invalid local asset remains hidden
	jr .decode_pending
.decoded
	ld [hl], 1
.decode_pending
	pop hl
	scf
	ret
.chunk
	ld b, a
	push hl
	ld bc, MOBILE_PEER_LOADED_FRAME - 34
	add hl, bc
	ld a, [hl]
	rrca
	rrca
	and 7
	pop hl
	ld b, a
	ld a, [hl]
	add b
	cp 6
	jr c, .chunk_index
	sub 6
.chunk_index
	ld [wMobileAvatarTile], a
	push hl
	push de
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, wMobileSavedFacings
	add hl, de
	ld a, [hl]
	call MobileNativeOwnSlot
	jp nc, .cancel
	push af
	call MobileNativeObjectPointer
	ld a, 1
	ldh [rWBK], a
	ld h, b
	ld l, c
	ld [hl], SPRITE_CHRIS ; reserve the slot; the exact ID is set after upload
	inc hl
	ld [hl], TEMP_OBJECT
	inc hl
	pop af
	ld b, 0
	cp FIRST_VRAM1_OBJECT_STRUCT
	jr c, .tile
	inc b
	sub FIRST_VRAM1_OBJECT_STRUCT
.tile
	add a
	add a
	ld c, a
	add c
	add c
	ld c, a
	ld a, b
	and a
	ld a, c
	jr nz, .tile_ready
	or $80
.tile_ready
	ld [hl], a
	ld a, b
	ld [wMobileAvatarBank], a
	ld a, c
	ld l, a
	ld h, 0
rept 4
	add hl, hl
endr
	ld bc, vTiles0
	add hl, bc
	ld a, [wMobileAvatarTile]
	cp 3
	jr c, .standing
	ld a, [wMobileAvatarBank]
	and a
	ld bc, $800
	jr z, .walk_offset
	ld bc, $400
.walk_offset
	add hl, bc
	ld a, [wMobileAvatarTile]
	sub 3
.standing
	push af
	ld bc, 4 tiles
.offset
	and a
	jr z, .offset_done
	add hl, bc
	dec a
	jr .offset
.offset_done
	pop af
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	pop de
	ld a, [wMobileAvatarTile]
	push hl
	ld h, d
	ld l, e
	ld bc, 4 tiles
.source
	and a
	jr z, .source_done
	add hl, bc
	dec a
	jr .source
.source_done
	ld d, h
	ld e, l
	pop hl
	ld a, [wMobileAvatarBank]
	call MobileNativeCopyFourTiles
	push af
	xor a
	ldh [rVBK], a
	pop af
	pop hl
	jr nc, .copy_deferred
	inc [hl]
.copy_deferred
	pop hl
	scf
	ret
.cancel
	pop de
	pop hl
.nothing
	pop hl
	and a
	ret
.restore
	push hl
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, wMobileSavedFacings
	add hl, de
	ld a, [hl]
	call MobileNativeOwnSlot
	pop hl
	jp nc, .nothing
	dec hl ; stage
	ld a, [hl]
	cp 7
	jp nc, .nothing
	and a
	jr nz, .restore_chunk
	dec hl
	ld a, [hli]
	cp SPRITE_SAILBOAT
	jr nz, .restore_private
	ld [hl], 7 ; shared original atlas was never overwritten
	jp .nothing
.restore_private
	inc [hl]
	ld a, 1
.restore_chunk
	dec a
	push hl
	farcall MobileNativeRawChunk
	ld a, b
	call MobileNativeCopyFourTiles
	push af
	xor a
	ldh [rVBK], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	pop af
	pop hl
	jr nc, .restore_pending
	inc [hl]
.restore_pending
	pop hl
	scf
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

MobileRenderSprites::
; Native InitSprites already submitted leases with normal OAM limits.
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

INCLUDE "mobile_adapter/rom/native_leases.asm"
INCLUDE "mobile_adapter/rom/native_lz.asm"

INCLUDE "mobile_adapter/rom/native_lease_policy.asm"
