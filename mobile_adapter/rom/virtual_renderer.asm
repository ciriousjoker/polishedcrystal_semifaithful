; Complete-figure OAM admission runs only in native idle time. Ordinary actors
; are immutable descriptors, never leased, deleted, frozen or reclassified.
SECTION "Mobile Virtual Renderer", ROMX, BANK[MOBILE_ROM_BANK]
MobileVirtualRendererStart::

MobileVirtualBeginSprites::
	push af
	push bc
	push de
	push hl
	ldh a, [rWBK]
	push af
	xor a
	ld [wMobileVirtualStage], a
	ld [wMobileVirtualPublished], a
	ld [wMobileVirtualThirdBlocked], a
	ld [wMobileVirtualPreviousOAM], a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld hl, wMobilePrivatePalBindings
	ld bc, 8
	xor a
	rst ByteFill
	ldh a, [hInMenu]
	and a
	jr nz, .done
	ld a, [wMobileLocalValid]
	and a
	jr z, .done
	ld a, [wMobileWanted]
	and a
	jr z, .done
	ld a, [wMobileOnline]
	and a
	jr z, .done
	ld a, TRUE
	ldh [hCGBPalUpdate], a
	ld a, [wMobileVisibleCount]
	and a
	jr z, .done
	ld hl, wMobileVirtualDescriptors + 6
	ld de, 8
	ld b, NUM_OBJECT_STRUCTS
.clear_descriptors
	ld [hl], 0
	add hl, de
	dec b
	jr nz, .clear_descriptors
	ld a, 1
	ld [wMobileVirtualStage], a
	call MobileVirtualAuditTileWindow
.done
	pop af
	ldh [rWBK], a
	pop hl
	pop de
	pop bc
	pop af
	ret

MobileVirtualAuditTileWindow:
; Audit loaded graphics, including retained offscreen actors. Tile availability
; is a sprite-format property, not a map/NPC permission list.
	ld a, 1
	ldh [rWBK], a
	ld hl, wObjectStructs
	ld b, NUM_OBJECT_STRUCTS
.loop
	inc hl
	inc hl
	ld a, [hld]
	dec hl
	cp (NUM_OBJECT_STRUCTS - FIRST_VRAM1_OBJECT_STRUCT - 1) * 12
	jr nz, .next
	ld a, [hl]
	and a
	jr z, .next
	cp SPRITE_VARS
	jr nc, .blocked
	push hl
	push bc
	dec a
	ld hl, SpriteHeaders + SPRITEDATA_TYPE_PAL
	ld bc, SPRITEDATA_LENGTH
	rst AddNTimes
	ld a, BANK(SpriteHeaders)
	call GetFarByte
	and SPRITEDATA_TYPE_MASK
	cp (BIG_GYARADOS_SPRITE - 1) << 6
	pop bc
	pop hl
	jr nc, .blocked
.next
	ld de, OBJECT_LENGTH
	add hl, de
	dec b
	jr nz, .loop
	jr .done
.blocked
	ld a, 1
	ld [wMobileVirtualThirdBlocked], a
	xor a
	ld [wMobileVirtualWindowOwners + 2], a
.done
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ret

MobileVirtualComposeStep::
	ld a, [wMobileVirtualStage]
	and a
	ret z
	ld a, [wMobileOnline]
	and a
	jr z, .abort
	ld a, [wMobileWanted]
	and a
	jr z, .abort
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileVirtualStage]
	cp 1
	jr z, .start
	cp 2
	jp z, .local
	cp 3
	jp z, .urgent
	cp 4
	jp z, .native
	cp 6
	jp z, .weather
	cp 7
	jp z, .publish
.abort
	xor a
	ld [wMobileVirtualStage], a
	ret
.start
	call MobileVirtualFastCase
; Only row counters and ownership need zeroing. Admitted groups overwrite
; their output bytes; the unused OAM suffix is hidden once, at publication.
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jp z, .rows_ready
	ld hl, wMobileVirtualLines
	xor a
rept SCREEN_HEIGHT_PX
	ld [hli], a
endr
.rows_ready
; Share complete remote figures only under actual total-OAM pressure. A crowded
; native viewport keeps its ordinary figures, with one rotating remote guest.
	ld a, [hUsedOAMIndex]
	cp OAM_SIZE - MOBILE_PEERS * 4 * OBJ_SIZE + 1
	ld a, MOBILE_PEERS
	jr c, .remote_limit
	ld a, 1
.remote_limit
	ld [wMobileVirtualRemoteLimit], a
	xor a
	ld hl, wMobileVirtualBindings
rept 8
	ld [hli], a
endr
	ld [wMobileVirtualLength], a
	ld [wMobileVirtualPaletteUsed], a
	ld [wMobileVirtualCursor], a
	ld [wMobileVirtualRectangleCount], a
	ld [wMobileRenderedCount], a
	ld a, [wMobileVirtualPhase]
	cp MOBILE_PEERS
	jr c, .phase_ready
	xor a
.phase_ready
	ld [wMobileVirtualPhase], a
	ldh a, [hVBlankCounter]
	and $f
	cp NUM_OBJECT_STRUCTS - 1
	jr c, .rotation_ready
	sub NUM_OBJECT_STRUCTS - 1
.rotation_ready
	ld [wMobileVirtualNativeRotation], a
	ld a, 2
	ld [wMobileVirtualStage], a
	ret
.local
	xor a
	call MobileVirtualNativeGroup
	ld a, 3
	ld [wMobileVirtualStage], a
	ret
.urgent
; Independent avatars precede ordinary figures only in presentation admission.
; Native NPC state/simulation remains untouched when total OAM capacity is full.
	ld a, [wMobileVirtualCursor]
	ld b, a
	ld a, [wMobileVirtualRemoteLimit]
	cp b
	jr z, .urgent_done
	ld a, b
	ld b, a
	inc a
	ld [wMobileVirtualCursor], a
	ld a, [wMobileVirtualPhase]
	add b
	cp MOBILE_PEERS
	jr c, .urgent_index
	sub MOBILE_PEERS
.urgent_index
	call MobileVirtualPeerGroup
	ret
.urgent_done
	xor a
	ld [wMobileVirtualCursor], a
	ld a, 4
	ld [wMobileVirtualStage], a
	ret

.native
	ld a, [wMobileVirtualCursor]
	inc a
	ld [wMobileVirtualCursor], a
	cp NUM_OBJECT_STRUCTS
	jr c, .native_group
	xor a
	ld [wMobileVirtualCursor], a
	ld a, 6
	ld [wMobileVirtualStage], a
	ret
  .native_group
	ld b, a
	ld a, [wMobileVirtualNativeRotation]
	add b
.native_modulo
	cp NUM_OBJECT_STRUCTS - 1
	jr c, .native_index
	sub NUM_OBJECT_STRUCTS - 1
	jr .native_modulo
.native_index
	inc a
	ld c, a
	add a
	add a
	add a
	add LOW(wMobileVirtualDescriptors + 6)
	ld l, a
	adc HIGH(wMobileVirtualDescriptors + 6)
	sub l
	ld h, a
	ld a, [hl]
	and $1f
	jr z, .native
	ld a, c
	call MobileVirtualNativeGroup
	ret
.weather
	ld hl, wMobileVirtualGroup
	call MobileVirtualScratchGroup
	ld a, [wMobileVirtualCursor]
	ld c, a
	ldh a, [hUsedWeatherSpriteIndex]
	add OBJ_SIZE
	cp c
	jr c, .weather_done
	jr z, .weather_done
	ldh a, [hUsedOAMIndex]
	ld b, a
	ld a, OAM_SIZE
	sub b
	cp c
	jr c, .weather_done
	jr z, .weather_done
	ld a, c
	add OBJ_SIZE
	ld [wMobileVirtualCursor], a
	ld h, HIGH(wShadowOAM)
	ld l, c
	ld a, [hl]
	and a
	ret z
	cp 160
	ret nc
	ld de, wMobileVirtualGroup
	ld bc, OBJ_SIZE
	rst CopyBytes
	ld a, OBJ_SIZE
	ld [wMobileVirtualGroupLength], a
	call MobileVirtualAdmitGroup
	ret
.weather_done
	ld a, 7
	ld [wMobileVirtualStage], a
	ret
.publish
	call MobileVirtualMergePalettes
; Whole output and matching hardware palette bindings commit in one early idle
; window. Native DMA never sees an in-progress admission list.
	ld hl, wMobileVirtualOutput
	ld de, wShadowOAM
	ld a, [wMobileVirtualLength]
	rrca
	rrca
	ld b, a
	and a
	jr z, .copy_done
.copy_quads
rept OBJ_SIZE
	ld a, [hli]
	ld [de], a
	inc e
endr
	dec b
	jr nz, .copy_quads
.copy_done
	call MobileVirtualHideShadowSuffix
	ld hl, wMobileVirtualBindings
	ld de, wMobilePrivatePalBindings
	ld bc, 8
	rst CopyBytes
	xor a
	ld [wPlayerCurrentOAMSlot], a
	ld [wMobileVirtualStage], a
	ld a, 1
	ld [wMobileVirtualPublished], a
	ld a, [wMobileVirtualPhase]
	inc a
	cp MOBILE_PEERS
	jr c, .next_phase
	xor a
.next_phase
	ld [wMobileVirtualPhase], a
	ld a, TRUE
	ldh [hCGBPalUpdate], a
	ret

MobileVirtualHideShadowSuffix:
	ld a, [wMobileVirtualLength]
	ld l, a
	ld h, HIGH(wShadowOAM)
.loop
	ld a, l
	cp OAM_SIZE
	ret nc
	ld [hl], 160
	add OBJ_SIZE
	ld l, a
	jr .loop

MobileVirtualFastCase:
; Geometry is checked while the native facing ROM is mapped. Rectangular
; figures use exact interval admission; special shapes keep per-row counts.
	xor a
	ld [wMobileVirtualFastAdmission], a
	ldh a, [hUsedWeatherSpriteIndex]
	and a
	ret nz
	ld hl, wMobileVirtualDescriptors + 6
	ld b, NUM_OBJECT_STRUCTS
.loop
	ld a, [hl]
	and $1f
	jr z, .next
	bit 6, [hl]
	ret z
.next
	ld de, 8
	add hl, de
	dec b
	jr nz, .loop
	ld a, 2
	ld [wMobileVirtualFastAdmission], a
	ret

MobileVirtualNativeGroup:
; A native slot -> scratch hardware group. Every template is bounded to 16 quads.
	ld [wMobileVirtualIndex], a
	add a
	add a
	add a
	add LOW(wMobileVirtualDescriptors)
	ld l, a
	adc HIGH(wMobileVirtualDescriptors)
	sub l
	ld h, a
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jr nz, .materialize
	push hl
	ld de, 6
	add hl, de
	ld a, [hli]
	bit 7, a
	jr z, .not_raw_rectangle
	and $1f
	cp 4
	jr nz, .not_raw_rectangle
	ld a, [hl]
	ld [wMobileVirtualGroupPointer], a
	ld a, HIGH(wShadowOAM)
	ld [wMobileVirtualGroupPointer + 1], a
	ld a, 16
	ld [wMobileVirtualGroupLength], a
	pop hl
	jp .prepared_group
.not_raw_rectangle
	pop hl
.materialize
	push hl
	ld hl, wMobileVirtualGroup
	call MobileVirtualScratchGroup
	pop hl
	ld de, wMobileVirtualTemplate
rept 8
	ld a, [hli]
	ld [de], a
	inc de
endr
	ld a, [wMobileVirtualTemplate + 6]
	and $1f
	ret z
	cp 17
	ret nc
	add a
	add a
	ld [wMobileVirtualGroupLength], a
	ld a, [wMobileVirtualTemplate + 6]
	bit 7, a
	jr z, .template
	ld a, [wMobileVirtualTemplate + 7]
	ld l, a
	ld h, HIGH(wShadowOAM)
	ld de, wMobileVirtualGroup
	ld a, [wMobileVirtualGroupLength]
	cp 16
	jr nz, .raw_copy
rept 16
	ld a, [hli]
	ld [de], a
	inc de
endr
	jp .prepared_group
.raw_copy
	ld c, a
	ld b, 0
	rst CopyBytes
	jp .prepared_group
.template
	ld hl, wMobileVirtualTemplate + 4
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld de, wMobileVirtualTemplate + 8
	ld a, [wMobileVirtualGroupLength]
	ld c, a
	ld b, 0
	ld a, BANK(Facings)
	call FarCopyBytes
	ld hl, wMobileVirtualTemplate + 8
	ld de, wMobileVirtualGroup
.loop
	ld a, [hli]
	ld b, a
	ld a, [wMobileVirtualTemplate + 1]
	add b
	ld [de], a
	inc de
	ld a, [hli]
	ld b, a
	ld a, [wMobileVirtualTemplate]
	add b
	ld [de], a
	inc de
	ld a, [hli]
	ld c, a
	ld a, [hli]
	ld b, a
	xor a
	bit ABSOLUTE_TILE_ID_F, c
	jr nz, .absolute
	ld a, [wMobileVirtualTemplate + 2]
.absolute
	add b
	ld b, a
	ld a, [wMobileVirtualTemplate + 3]
	and OAM_BANK1
	ld a, b
	jr z, .tile
	cp $80
	jr c, .tile
	sub $40
.tile
	ld [de], a
	inc de
	xor a
	bit RELATIVE_ATTRIBUTES_F, c
	jr z, .flags
	ld a, [wMobileVirtualTemplate + 7]
.flags
	or c
	and OAM_XFLIP | OAM_YFLIP | OAM_PRIO
	ld b, a
	ld a, [wMobileVirtualTemplate + 3]
	or b
	bit NEXT_PALETTE_F, c
	jr z, .palette
	inc a
.palette
	ld [de], a
	inc de
	ld a, e
	sub LOW(wMobileVirtualGroup)
	ld b, a
	ld a, [wMobileVirtualGroupLength]
	cp b
	jr nz, .loop
.prepared_group
	call MobileVirtualGroupVisible
	ret z
; Native palette ownership blocks private colors for exactly these presentations.
	ld a, [wMobileVirtualGroupPointer]
	ld l, a
	ld a, [wMobileVirtualGroupPointer + 1]
	ld h, a
	ld de, 3
	add hl, de
	ld a, [wMobileVirtualGroupLength]
	cp 16
	jr nz, .palette_general
; Emitted native four-quad facings have a uniform palette, checked by the
; assembly-data contract. Independent avatars also share one complete color.
	ld b, 1
	jr .palette_ready
.palette_general
	rrca
	rrca
	ld b, a
.palette_ready
.check_palette
	ld a, [hl]
	and OAM_PALETTE
	push hl
	push bc
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualBindings
	add hl, de
	ld a, [hl]
	pop bc
	pop hl
	and a
	ret nz
	ld de, OBJ_SIZE
	add hl, de
	dec b
	jr nz, .check_palette
	jp MobileVirtualAdmitGroup

MobileVirtualPeerGroup:
; A pool index, cache and colors validated in bounded background work.
	ld [wMobileBudgetIndex], a
	ld hl, wMobileVirtualGroup
	call MobileVirtualScratchGroup
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, MOBILE_PEER_VISIBLE
	add hl, bc
	ld a, [hl]
	and a
	ret z
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
; Revalidate map/status at drawing time. A turn or script can postpone the
; projection job while a new network record already describes another map.
	push hl
	ld a, 1
	ldh [rWBK], a
	ld a, [wMapGroup]
	ld b, a
	ld a, [wMapNumber]
	ld c, a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld de, 6
	add hl, de
	ld a, [hli]
	cp b
	jr nz, .ineligible
	ld a, [hli]
	cp c
	jr nz, .ineligible
	ld a, [wMobileInstanceKey]
	cp [hl]
	jr nz, .ineligible
	inc hl
	ld a, [wMobileInstanceKey + 1]
	cp [hl]
	jr nz, .ineligible
	ld de, 8
	add hl, de
	ld a, [hl]
	cp 1
	jr nz, .ineligible
	inc hl
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
	ld a, [hl]
	cp b
	jr nz, .ineligible
	pop hl
	jr .eligible
.ineligible
	pop hl
	ret
.eligible
	ld bc, 34
	add hl, bc
	ld a, [hl]
	cp 7
	ret nz
	ld a, [wMobileBudgetIndex]
	inc a
	ld b, a
	ld hl, wMobileVirtualWindowOwners
	ld c, 0
.window
	ld a, [hli]
	cp b
	jr z, .owned
	inc c
	ld a, c
	cp MOBILE_PEERS
	jr c, .window
	ret
.owned
	ld a, c
	ld [wMobileVirtualWindow], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, 54
	add hl, bc
	ld a, [hl]
	and a
	ret z
	ld [wMobileVirtualCacheHit], a ; 2: exact current native color
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, MOBILE_PEER_PALETTE
	add hl, bc
	ld a, [hl]
	ld [wMobileAvatarPalette], a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileVirtualCacheHit]
	cp 2
	jr nz, .private_request
	ld a, [wMobileAvatarPalette]
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualBindings
	add hl, de
	ld a, [hl]
	and a
	ret nz
	jr .color_ready
.private_request
	ld a, [wPalFadeDelayFrames]
	and a
	ret nz
; Private colors occupy only a currently unused palette. Local player was
; admitted first, so its hardware color cannot be borrowed.
	ldh a, [hVBlankCounter]
	and 7
	cp PAL_OW_WEATHER
	jr c, .color_start
	sub PAL_OW_WEATHER
.color_start
	ld c, a
	ld a, PAL_OW_WEATHER
	ld [wMobileVirtualGroupPalette], a
.find_color
	ld b, c
	inc b
	ld a, 1
.bit
	dec b
	jr z, .mask
	add a
	jr .bit
.mask
	ld b, a
	ld a, [wMobileVirtualPaletteUsed]
	and b
	jr z, .private_color
	inc c
	ld a, c
	cp PAL_OW_WEATHER
	jr c, .color_next
	ld c, 0
.color_next
	ld a, [wMobileVirtualGroupPalette]
	dec a
	ld [wMobileVirtualGroupPalette], a
	jr nz, .find_color
	ret
.private_color
	ld a, c
	ld [wMobileAvatarPalette], a
.color_ready
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, MOBILE_PEER_PALETTE
	add hl, bc
	ld a, [wMobileAvatarPalette]
	ld [hl], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, MOBILE_PEER_SCREEN_X
	add hl, bc
	ld a, [hli]
	ld [wMobileOAMWriteCursor], a
	ld a, [hl]
	ld [wMobileOAMDemand], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, 52
	add hl, bc
	ld a, [hl]
	ld [wMobileVirtualFrame], a
	and $c
	cp $c
	jr z, .mirror
	cp 8
	ld a, 0
	jr nc, .mirror_ready
	ld a, [wMobileVirtualFrame]
	and 3
	cp 3
	ld a, 0
	jr nz, .mirror_ready
.mirror
	ld a, OAM_XFLIP
.mirror_ready
	ld [wMobileVirtualMirror], a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileOAMWriteCursor]
	ld [wMobileVirtualTemplate], a
	ld a, [wMobileOAMDemand]
	ld [wMobileVirtualTemplate + 1], a
; Prepare the four canonical quads directly. Compute tile bank, mirror and
; hardware color once for the complete figure, rather than for each quadrant.
	ld a, [wMobileVirtualWindow]
	cp 2
	ld b, $7c
	ld c, OAM_BANK1
	jr z, .base_ready
	add a
	add a
	add $64
	ld b, a
	ld c, 0
.base_ready
	ld a, [wMobileVirtualMirror]
	or c
	ld c, a
	ld a, [wMobileAvatarPalette]
	or c
	ld c, a
	ld a, [wMobileVirtualMirror]
	and a
	jr z, .columns_ready
	inc b
.columns_ready
	ld a, [wMobileVirtualTemplate]
	ld e, a
	ld a, [wMobileVirtualTemplate + 1]
	ld d, a
	ld a, 16
	ld [wMobileVirtualGroupLength], a
	ld hl, wMobileVirtualGroup
for quad, 4
	ld a, d
if quad >= 2
	add 8
endc
	ld [hli], a
	ld a, e
if quad & 1
	add 8
endc
	ld [hli], a
	ld a, b
if quad & 1
	xor 1
endc
if quad >= 2
	add 2
endc
	ld [hli], a
	ld [hl], c
	inc hl
endr

	call MobileVirtualAdmitGroup
	ret c
	ld a, [wMobileVirtualLength]
	sub 16
	ld [wMobileOAMReadCursor], a
	ld a, [wMobileVirtualCacheHit]
	cp 2
	jr z, .bound
	ld a, [wMobileAvatarPalette]
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualBindings
	add hl, de
	ld a, [wMobileBudgetIndex]
	inc a
	ld [hl], a
.bound
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, MOBILE_PEER_OAM_START
	add hl, bc
	ld a, [wMobileOAMReadCursor]
	ld [hl], a
	ld hl, wMobileRenderedCount
	inc [hl]
	ret

MobileVirtualGroupVisible:
; Fully offscreen quads consume no hardware scanline. Do not waste finite idle
; time admitting a retained actor that contributes no pixel to this viewport.
	ld a, [wMobileVirtualGroupLength]
	rrca
	rrca
	ld b, a
	ld a, [wMobileVirtualGroupPointer]
	ld l, a
	ld a, [wMobileVirtualGroupPointer + 1]
	ld h, a
.loop
	ld a, [hl]
	cp 9
	jr c, .next
	cp 160
	jr c, .visible
.next
	ld de, OBJ_SIZE
	add hl, de
	dec b
	jr nz, .loop
	xor a
	ret
.visible
	or 1
	ret

MobileVirtualAdmitGroup:
; Bank 5 scratch. Count every real hardware scanline and roll back a rejected
; complete figure. All loops have fixed hardware bounds and only private writes.
	ld a, [wMobileVirtualLength]
	ld b, a
	ld a, [wMobileVirtualGroupLength]
	add b
	jr c, .full
	cp OAM_SIZE + 1
	jr nc, .full
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jr nz, .exact_rows
	call MobileVirtualAdmitRectangle
	ret c
	jr .admitted
.exact_rows
	xor a
	ld [wMobileVirtualRejected], a
	ld hl, wMobileVirtualGroup
	call .count
	ld a, [wMobileVirtualRejected]
	and a
	jr z, .admitted
	ld hl, wMobileVirtualGroup
	call .undo
.full
	scf
	ret
.admitted
	ld a, [wMobileVirtualLength]
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualOutput
	add hl, de
	ld d, h
	ld e, l
	ld a, [wMobileVirtualGroupPointer]
	ld l, a
	ld a, [wMobileVirtualGroupPointer + 1]
	ld h, a
	ld a, [wMobileVirtualGroupLength]
	cp 16
	jr nz, .copy_general
rept 16
	ld a, [hli]
	ld [de], a
	inc de
endr
	jr .copied
.copy_general
	ld c, a
	ld b, 0
	rst CopyBytes
.copied
	ld a, [wMobileVirtualLength]
	ld b, a
	ld a, [wMobileVirtualGroupLength]
	add b
	ld [wMobileVirtualLength], a
	ld a, [wMobileVirtualGroupPointer]
	ld l, a
	ld a, [wMobileVirtualGroupPointer + 1]
	ld h, a
	ld de, 3
	add hl, de
	ld a, [wMobileVirtualGroupLength]
	cp 16
	jr nz, .palette_general
; Emitted native four-quad facings have a uniform palette, checked by the
; assembly-data contract. Independent avatars also share one complete color.
	ld b, 1
	jr .palette_ready
.palette_general
	rrca
	rrca
	ld b, a
.palette_ready
.palette_used
	ld a, [hl]
	and OAM_PALETTE
	push hl
	ld e, a
	ld d, 0
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [wMobileVirtualPaletteUsed]
	or [hl]
	ld [wMobileVirtualPaletteUsed], a
	pop hl
	ld de, OBJ_SIZE
	add hl, de
	dec b
	jr nz, .palette_used
	and a
	ret

.count
	ld c, 1
	jr .rectangle
.undo
	ld c, -1
.rectangle
; A complete ordinary 16x16 figure contributes two objects on sixteen rows.
; Unroll this common case rather than visiting each of its four quads separately.
	ld a, [wMobileVirtualGroupLength]
	cp 16
	jp nz, .lines
	ld a, [wMobileVirtualGroup]
	cp 16
	jp c, .lines
	cp 145
	jp nc, .lines
	ld b, a
	ld a, [wMobileVirtualGroup + 4]
	cp b
	jp nz, .lines
	ld a, [wMobileVirtualGroup + 8]
	ld d, a
	ld a, [wMobileVirtualGroup + 12]
	cp d
	jp nz, .lines
	ld a, d
	sub b
	cp 8
	jr z, .forward_rectangle
	cp -8
	jp nz, .lines
	ld b, d
.forward_rectangle
	ld a, c
	add c
	ld c, a
	ld a, b
	sub OAM_Y_OFS
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualLines
	add hl, de
rept 16
	ld a, [hl]
	add c
	ld [hli], a
	cp 11
	jr c, :+
	ld [wMobileVirtualRejected], a
:
endr
	ret
.lines
	ld a, [wMobileVirtualGroupLength]
	rrca
	rrca
	ld b, a
.quad_lines
	push bc
	push hl
	ld a, [hl]
	sub OAM_Y_OFS
	ld e, a
	ld d, 8
.line
	ld a, e
	cp SCREEN_HEIGHT_PX
	jr nc, .next_line
	push de
	ld d, 0
	ld hl, wMobileVirtualLines
	add hl, de
	ld a, [hl]
	add c
	ld [hl], a
	cp 11
	jr c, .within
	ld a, 1
	ld [wMobileVirtualRejected], a
.within
	pop de
.next_line
	inc e
	dec d
	jr nz, .line
	pop hl
	ld de, OBJ_SIZE
	add hl, de
	pop bc
	dec b
	jr nz, .quad_lines
	ret

MobileVirtualAdmitRectangle:
; Equal-height intervals overlap exactly when six sorted top coordinates span
; fewer than sixteen pixels. Each canonical figure contributes two objects.
; This proves the ten-object bound without updating 144 separate row counters.
	ld a, [wMobileVirtualGroupPointer]
	ld l, a
	ld a, [wMobileVirtualGroupPointer + 1]
	ld h, a
	ld b, [hl]
	ld de, 8
	add hl, de
	ld a, [hl]
	cp b
	jr c, .top
	ld a, b
.top
	ld [wMobileVirtualRectangleTop], a
	ld d, a
	ld a, [wMobileVirtualRectangleCount]
	ld b, a
	ld c, 0
	ld hl, wMobileVirtualLines
.find
	ld a, c
	cp b
	jr z, .insert
	ld a, [hli]
	cp d
	jr nc, .insert
	inc c
	jr .find
.insert
	ld a, c
	ld [wMobileVirtualRejected], a
	ld a, b
	sub c
	ld c, a
	ld a, b
	ld hl, wMobileVirtualLines
	ld e, a
	ld d, 0
	add hl, de
	ld d, h
	ld e, l
	dec hl
	ld a, c
	and a
	jr z, .write
.shift
	ld a, [hld]
	ld [de], a
	dec de
	dec c
	jr nz, .shift
.write
	ld a, [wMobileVirtualRectangleTop]
	ld [de], a
	ld hl, wMobileVirtualRectangleCount
	inc [hl]
	ld a, [hl]
	sub 5
	jr c, .accepted
	jr z, .accepted
	ld b, a
	ld hl, wMobileVirtualLines
	ld de, wMobileVirtualLines + 5
.check
	ld a, [de]
	sub [hl]
	cp 16
	jr c, .remove
	inc hl
	inc de
	dec b
	jr nz, .check
.accepted
	and a
	ret
.remove
	ld a, [wMobileVirtualRejected]
	ld c, a
	ld a, [wMobileVirtualRectangleCount]
	dec a
	ld [wMobileVirtualRectangleCount], a
	sub c
	ld b, a
	ld a, c
	ld c, b
	ld b, 0
	ld hl, wMobileVirtualLines
	ld e, a
	ld d, 0
	add hl, de
	ld d, h
	ld e, l
	inc hl
	ld a, c
	and a
	jr z, .rejected
	rst CopyBytes
.rejected
	scf
	ret

MobileVirtualMergePalettes:
; Build all 64 bytes before publishing OAM. VBlank then uses the same unrolled
; upload as vanilla, without eight variable-cost palette-source calls.
	ld hl, wOBPals2
	ld de, wMobileVirtualMergedPalette
	ld bc, 8 palettes
	rst CopyBytes
	ld c, 0
.loop
	push bc
	ld e, c
	ld d, 0
	ld hl, wMobileVirtualBindings
	add hl, de
	ld a, [hl]
	and a
	jr z, .next
	cp MOBILE_PEERS + 1
	jr nc, .next
	dec a
	ld hl, wMobilePrivatePalColors
	ld bc, 1 palettes
	rst AddNTimes
	push hl
	ld hl, sp + 2
	ld a, [hl] ; saved C is above the pushed source pointer
	ld hl, wMobileVirtualMergedPalette
	ld bc, 1 palettes
	rst AddNTimes
	ld d, h
	ld e, l
	pop hl
	ld bc, 1 palettes
	rst CopyBytes
.next
	pop bc
	inc c
	ld a, c
	cp 8
	jr c, .loop
	ret

MobileVirtualPaletteSource:
; Bank 5, HL ordinary active color, D hardware slot. Return a private pointer
; only for the completed current OAM publication. Never edit native metadata.
	push af
	push bc
	push de
	ld a, [wMobileOnline]
	and a
	jr z, .done
	ld a, [wMobileWanted]
	and a
	jr z, .done
	push hl
	ld e, d
	ld d, 0
	ld hl, wMobilePrivatePalBindings
	add hl, de
	ld a, [hl]
	and a
	jr z, .ordinary
	cp MOBILE_PEERS + 1
	jr nc, .ordinary
	dec a
	ld hl, wMobilePrivatePalColors
	ld bc, 1 palettes
	rst AddNTimes
	pop de ; discard ordinary source
	jr .done
.ordinary
	pop hl
.done
	pop de
	pop bc
	pop af
	ret

MobileRenderSprites::
	ret
MobileVirtualScratchGroup:
	ld a, l
	ld [wMobileVirtualGroupPointer], a
	ld a, h
	ld [wMobileVirtualGroupPointer + 1], a
	ret

MobileVirtualPaletteBits: db 1, 2, 4, 8, 16, 32, 64, 128
MobileVirtualRendererEnd::
ASSERT MobileVirtualRendererEnd - MobileVirtualRendererStart <= 4096
