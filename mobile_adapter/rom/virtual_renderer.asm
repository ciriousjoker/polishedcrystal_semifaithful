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
	ld a, [wMapGroup]
	ld [wMobileVirtualMapGroup], a
	ld a, [wMapNumber]
	ld [wMobileVirtualMapNumber], a
	ld a, [wMobileVirtualHasPrivateColors]
	and a
	jr z, .colors_restored
	ld a, TRUE
	ldh [hCGBPalUpdate], a ; restore native colors if no private frame completes
.colors_restored
	xor a
	ld [wMobileVirtualHasPrivateColors], a
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
	ld a, 2
	ld [wMobileVirtualFastAdmission], a
.done
	pop af
	ldh [rWBK], a
	pop hl
	pop de
	pop bc
	pop af
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
	cp 8
	jp z, .collect
.abort
	xor a
	ld [wMobileVirtualStage], a
	ret
.start
	call MobileVirtualFastCase
	ld a, [wMobileVirtualNeedColorAudit]
	bit 7, a
	jr nz, .refresh_colors
	ldh a, [hCGBPalUpdate]
	and a
	jr z, .colors_current
.refresh_colors
	call MobileVirtualCurrentColors
.colors_current
	xor a
	ld [wMobileVirtualBaseReady], a
.reset
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
	ld hl, wMobileVirtualLines + 16
	xor a
rept 11
	ld [hli], a
endr
	ld hl, wMobileVirtualKeepSlots
	xor a
rept 2 * OAM_COUNT / 4
	ld [hli], a
endr
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
	ld [wMobileVirtualPlannedPrivateColors], a
	ld [wMobileVirtualCoarseTrial], a
	ld a, [wMobileVirtualPhase]
	cp MOBILE_PEERS
	jr c, .phase_ready
	xor a
.phase_ready
	ld [wMobileVirtualPhase], a
	ld [wMobileVirtualNextPhase], a
	ldh a, [hVBlankCounter]
	and $f
	cp NUM_OBJECT_STRUCTS - 1
	jr c, .rotation_ready
	sub NUM_OBJECT_STRUCTS - 1
.rotation_ready
	ld [wMobileVirtualNativeRotation], a
	ld a, [wMobileVirtualBaseReady]
	cp 255
	jr z, .full_base
	ld a, [wMobileVirtualForceFull]
	and a
	jr nz, .full_base
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jr nz, .full_base
	ld a, 2
	ld [wMobileVirtualBaseReady], a
	call MobileVirtualFastScene
	jr nc, .fast_base
	ld a, 255
	ld [wMobileVirtualBaseReady], a
	jp .reset
.fast_base
	ld a, [wMobileVirtualBaseReady]
	cp 1
	ld a, 3
	jr z, .base_stage
	ld a, 8
.base_stage
	ld [wMobileVirtualStage], a
	ret
.collect
	call MobileVirtualFastSceneContinue
	jr nc, .fast_base
	ld a, 255
	ld [wMobileVirtualBaseReady], a
	jp .reset
.full_base
	xor a
	ld [wMobileVirtualBaseReady], a
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
	ld a, [wMobileVirtualStage]
	cp 3
	ret nz
	ld a, [wMobileVirtualCursor]
	ld b, a
	ld a, [wMobileVirtualRemoteLimit]
	cp b
	jr z, .urgent_done ; a completed last guest needs no second large-job admission
	ret
.urgent_done
	ld a, [wMobileVirtualBaseReady]
	and a
	jr z, .full_native
	ld a, 7
	ld [wMobileVirtualStage], a
	ret
.full_native
	xor a
	ld [wMobileVirtualCursor], a
	ld a, 4
	ld [wMobileVirtualStage], a
	ret

.native
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jr z, .native_raw
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
.native_raw
; Native rectangles already fill aligned OAM groups. Walk those current groups
; directly instead of resolving thirteen object descriptors and ROM templates.
; An actor vanilla could not fit is not made visible by the multiplayer layer.
	ld a, 255
	ld [wMobileVirtualGroupOwner], a
	ld a, [wMobileVirtualCursor]
	ld c, a
	inc a
	ld [wMobileVirtualCursor], a
	cp OAM_COUNT / 4 + 1
	jr nc, .native_raw_done
	ld a, [wMobileVirtualNativeRotation]
	add c
.raw_modulo
	cp OAM_COUNT / 4
	jr c, .raw_slot
	sub OAM_COUNT / 4
	jr .raw_modulo
.raw_slot
	swap a
	ld c, a
	ld a, [wPlayerCurrentOAMSlot]
	cp c
	jr z, .next_raw
	ld a, c
	ld [wMobileVirtualGroupPointer], a
	ld a, HIGH(wShadowOAM)
	ld [wMobileVirtualGroupPointer + 1], a
	ld a, 16
	ld [wMobileVirtualGroupLength], a
	call MobileVirtualNativeRectangle
.next_raw
	ld a, [wMobileVirtualCursor]
	cp OAM_COUNT / 4
	jr nc, .native_raw_done
	ldh a, [rLY]
	cp 137
	ret nc
	jr .native_raw
.native_raw_done
	xor a
	ld [wMobileVirtualCursor], a
	ld a, 7 ; this path has no weather groups
	ld [wMobileVirtualStage], a
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
	ld a, [wMobileRenderedCount]
	swap a
	ld b, a
	ld a, [wMobileVirtualLength]
	sub b
	ld [wMobileVirtualPreviousOAM], a ; before restoring an unused native hole
	ld a, [wMobileVirtualBaseReady]
	and a
	jr z, .native_ready
	call MobileVirtualRestoreUnusedHole
.native_ready
	ld hl, wMobileVirtualBindings
	xor a
rept 8
	or [hl]
	inc hl
endr
	ld [wMobileVirtualHasPrivateColors], a
	and a
	jr z, .colors_ready
	call MobileVirtualMergePalettes
	ld a, TRUE
	ldh [hCGBPalUpdate], a
.colors_ready
; Whole output and matching hardware palette bindings commit in one early idle
; window. Native DMA never sees an in-progress admission list.
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jr nz, .packed_output
	call MobileVirtualPublishScatter
	jr .bindings
.packed_output
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
	xor a
	ld [wPlayerCurrentOAMSlot], a
.bindings
	ld hl, wMobileVirtualBindings
	ld de, wMobilePrivatePalBindings
	ld bc, 8
	rst CopyBytes
	xor a
	ld [wMobileVirtualStage], a
	ld a, 1
	ld [wMobileVirtualPublished], a
	ld a, [wMobileVirtualNextPhase]
	ld b, a
	ld a, [wMobileRenderedCount]
	and a
	ld a, b
	jr nz, .next_phase
	inc a ; unready peers cannot keep the ready peer outside the cyclic cohort
	cp MOBILE_PEERS
	jr c, .next_phase
	xor a
.next_phase
	ld [wMobileVirtualPhase], a
	ret

MobileVirtualPublishScatter:
; All native figures in this path occupy aligned four-quad groups. Preserve
; their current engine-generated bytes in place, hide only rejected draw groups,
; and insert generated figures into the resulting holes. Ordinary NPC records
; and positions are never changed. This avoids copying every NPC twice.
	ld a, [wMobileVirtualBaseReady]
	cp 1
	jp z, MobileVirtualPublishFastGuests
	ld hl, wMobileVirtualKeepSlots
	ld de, wShadowOAM
	ld b, OAM_COUNT / 4
.hide
	ld a, [hli]
	and a
	jr nz, .keep
	ld a, 160
rept 4
	ld [de], a
	inc e
	inc e
	inc e
	inc e
endr
	jr .next
.keep
	ld a, e
	add 16
	ld e, a
.next
	dec b
	jr nz, .hide
	ld c, 0
.extra
	ld e, c
	ld d, 0
	ld hl, wMobileVirtualExtraOwners
	add hl, de
	ld a, [hl]
	and a
	jp z, .next_extra
	ld [wMobileVirtualGroupOwner], a
	push bc
	ld hl, wMobileVirtualKeepSlots
	ld b, 0
.find
	ld a, [hli]
	and a
	jr z, .found
	inc b
	ld a, b
	cp OAM_COUNT / 4
	jr c, .find
	pop bc
	ret ; bounds fault stays confined to omitted presentation
.found
	dec hl
	ld [hl], 1
	ld a, b
	swap a
	ld [wMobileOAMReadCursor], a
	ld e, a
	ld d, HIGH(wShadowOAM)
	ld a, c
	swap a
	add LOW(wMobileVirtualOutput)
	ld l, a
	adc HIGH(wMobileVirtualOutput)
	sub l
	ld h, a
rept 16
	ld a, [hli]
	ld [de], a
	inc e
endr
	ld a, [wMobileVirtualGroupOwner]
	cp MOBILE_PEERS + 2
	jr nz, .peer
	ld a, [wMobileOAMReadCursor]
	ld [wPlayerCurrentOAMSlot], a
	jr .owner_done
.peer
	cp MOBILE_PEERS + 1
	jr nc, .owner_done
	dec a
	ld b, a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, b
	call MobileNativePeerPointer
	ld bc, MOBILE_PEER_OAM_START
	add hl, bc
	ld a, [wMobileOAMReadCursor]
	ld [hl], a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
.owner_done
	pop bc
.next_extra
	inc c
	ld a, c
	cp OAM_COUNT / 4
	jp c, .extra
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

MobileVirtualCurrentColors:
; Audit native hardware color metadata once for this composition. Peers reuse
; current exact sources, including a slot that moved since the last upload job.
; Private construction remains isolated from native save and palette metadata.
	ld a, 255
for avatar, NUM_PLAYER_GENDERS / 2
	ld [wMobileVirtualNativeColors + avatar], a
endr
	ld a, 1
	ldh [rWBK], a
	ld b, 1
	ld c, 0
.slot
	ld a, [wLoadedObjPalType]
	and b
	jr nz, .next
	ld e, c
	ld d, 0
	ld hl, wLoadedObjPalGlows
	add hl, de
	ld a, [hl]
	ld hl, wLoadedObjPalPrevGlows
	add hl, de
	or [hl]
	jr nz, .next
	ld hl, wLoadedObjPal0
	add hl, de
	ld a, [hl]
	cp NUM_PLAYER_GENDERS
	jr nc, .next
	bit 0, a
	jr nz, .odd
	srl a
	ld e, a
	ld hl, wMobileVirtualNativeColors
	add hl, de
	ld a, [hl]
	and $f0
	or c
	ld [hl], a
	jr .next
.odd
	srl a
	ld e, a
	ld hl, wMobileVirtualNativeColors
	add hl, de
	ld a, c
	swap a
	ld e, a
	ld a, [hl]
	and $f
	or e
	ld [hl], a
.next
	sla b
	inc c
	ld a, c
	cp PAL_OW_WEATHER
	jr c, .slot
	xor a
	ld [wMobileVirtualNeedColorAudit], a
	ld a, [wMobileVirtualNativeColors]
	ld b, a
	ld a, [wMobileVirtualNativeColors + 1]
	or b
	and $88 ; any missing ($f) nibble
	jr nz, .needs_audit
	jr .done
.needs_audit
	ld a, 1
	ld [wMobileVirtualNeedColorAudit], a
.done
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ret

MobileVirtualFastCase:
; Geometry is checked while the native facing ROM is mapped. Rectangular
; figures use exact interval admission; special shapes keep per-row counts.
	ldh a, [hUsedWeatherSpriteIndex]
	and a
	jr nz, .no_fast_player
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	ret nz
	ld a, [wMobileVirtualDescriptors + 6]
	bit 7, a
	jr z, .no_fast_player
	ld a, [wMobileVirtualDescriptors + 7]
	and $f
	ret z
.no_fast_player
	xor a
	ld [wMobileVirtualFastAdmission], a
	ret

MobileVirtualNativeGroup:
; A native slot -> scratch hardware group. Every template is bounded to 16 quads.
	ld [wMobileVirtualIndex], a
	and a
	ld a, 255
	jr nz, .owner
	ld a, MOBILE_PEERS + 2
.owner
	ld [wMobileVirtualGroupOwner], a
	ld a, [wMobileVirtualIndex]
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
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jp z, MobileVirtualNativeRectangle
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

MobileVirtualNativeRectangle:
; The native facing-data contract proves all four-quad groups are rectangles
; with a uniform hardware color. Read the completed current native OAM directly.
; Do not repeat general quad visibility/palette scans for this common case.
	ld a, [wMobileVirtualGroupPointer]
	ld l, a
	ld a, [wMobileVirtualGroupPointer + 1]
	cp HIGH(wShadowOAM)
	jp z, MobileVirtualNativeRawRectangle
	ld h, a
	ld b, [hl]
	ld de, 8
	add hl, de
	ld a, [hl]
	cp b
	jr c, .top
	ld a, b
.top
	cp 160
	ret nc
	and a
	ret z
	ld de, -5
	add hl, de
	ld a, [hl]
	and OAM_PALETTE
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualBindings
	add hl, de
	ld a, [hl]
	and a
	ret nz
	jp MobileVirtualAdmitGroup

MobileVirtualNativeRawRectangle:
; Admit a completed vanilla rectangle without materializing or copying it.
; Its actual bytes remain in ShadowOAM until publication's omission pass.
	ld a, [wMobileVirtualLength]
	cp OAM_SIZE - 16 + 1
	ret nc
	ld a, [wMobileVirtualGroupPointer]
	ld l, a
	ld h, HIGH(wShadowOAM)
	ld b, [hl]
	ld de, 8
	add hl, de
	ld a, [hl]
	cp b
	jr c, .top
	ld a, b
.top
	cp 160
	ret nc
	and a
	ret z
	ld [wMobileVirtualRectangleTop], a
	ld d, a
	ld a, [wMobileVirtualPlannedPrivateColors]
	and a
	jr nz, .color
	ld a, [wMobileVirtualGroupOwner]
	cp MOBILE_PEERS + 2
	jr z, .color ; local colors must remain available to subsequent peer requests
	call MobileVirtualAdmitRectangleTop
	ret c
	jr .keep
.color
	ld de, -5
	add hl, de
	ld a, [hl]
	and OAM_PALETTE
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualBindings
	add hl, de
	ld a, [hl]
	and a
	ret nz
	push de
	ld a, [wMobileVirtualRectangleTop]
	ld d, a
	call MobileVirtualAdmitRectangleTop
	pop de
	ret c
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [wMobileVirtualPaletteUsed]
	or [hl]
	ld [wMobileVirtualPaletteUsed], a
.keep
	ld a, [wMobileVirtualGroupPointer]
	swap a
	and $f
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualKeepSlots
	add hl, de
	ld [hl], 1
	ld hl, wMobileVirtualLength
	ld a, [hl]
	add 16
	ld [hl], a
	ret

MobileVirtualPeerGroup:
; A pool index, cache and colors validated in bounded background work.
	ld [wMobileBudgetIndex], a
	inc a
	ld [wMobileVirtualGroupOwner], a
	ld hl, wMobileVirtualGroup
	call MobileVirtualScratchGroup
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	push hl
	ld bc, MOBILE_PEER_VISIBLE
	add hl, bc
	ld a, [hl]
	and a
	jr z, .initial_invalid
	ld bc, MOBILE_PEER_DRAW_VALID - MOBILE_PEER_VISIBLE
	add hl, bc
	ld a, [hl]
	and a
	jr z, .initial_invalid
	pop hl
	jr .initial_valid
.initial_invalid
	pop hl
	ret
.initial_valid
; Revalidate map/status at drawing time. A turn or script can postpone the
; projection job while a new network record already describes another map.
	push hl
	ld a, [wMobileVirtualMapGroup]
	ld b, a
	ld a, [wMobileVirtualMapNumber]
	ld c, a
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
	ld hl, MobileVirtualAssetSprites
	add hl, bc
	ld a, [hl]
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
	ld e, a
	inc a
	ld b, a
	ld d, 0
	ld hl, wMobileVirtualWindowOwners
	add hl, de
	ld a, [hl]
	cp b
	ret nz
	ld a, e
	ld [wMobileVirtualWindow], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, 54
	add hl, bc
	ld a, [hl]
	and a
	ret z
	ld [wMobileVirtualCacheHit], a ; 2: exact current native color
	ld bc, MOBILE_PEER_PALETTE - 54
	add hl, bc
	ld a, [hl]
	ld [wMobileAvatarPalette], a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, 18
	add hl, bc
	ld a, [hl]
	ld b, a
	rrca
	and 1
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualNativeColors
	add hl, de
	ld a, [hl]
	bit 0, b
	jr z, .source_nibble
	swap a
.source_nibble
	and $f
	ld [wMobileAvatarPalette], a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileAvatarPalette]
	cp PAL_OW_WEATHER
	jr c, .exact_color
	ld a, [wMobileVirtualCacheHit]
	cp 1
	ret nz ; no current exact source and no constructed private color yet
	jr .private_request
.exact_color
	ld a, 2
	ld [wMobileVirtualCacheHit], a
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
.private_color
	ld a, [wPalFadeDelayFrames]
	and a
	ret nz
; Private colors occupy only a currently unused palette. Local player was
; admitted first, so its hardware color cannot be borrowed.
	ld a, [wMobileVirtualPaletteUsed]
	and (1 << PAL_OW_WEATHER) - 1
	ld e, a
	ld d, 0
	ld hl, MobileVirtualFreeColor
	add hl, de
	ld a, [hl]
	cp PAL_OW_WEATHER
	jr nc, .color_full
	ld [wMobileAvatarPalette], a
	jr .color_ready
.color_full
	ld a, [wMobileVirtualBaseReady]
	and a
	ret z
; Only genuine palette pressure needs the fair full admission path. Native
; sources were audited before taking an unused color in the common fast path.
	ld a, 1
	ld [wMobileVirtualForceFull], a
	xor a
	ld [wMobileVirtualBaseReady], a
	ld a, 1
	ld [wMobileVirtualStage], a
	scf
	ret
.color_ready
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, MOBILE_PEER_PALETTE
	add hl, bc
	ld a, [wMobileAvatarPalette]
	ld [hl], a
	ld bc, MOBILE_PEER_SCREEN_X - MOBILE_PEER_PALETTE
	add hl, bc
	ld a, [hli]
	ld [wMobileOAMWriteCursor], a
	ld a, [hl]
	ld [wMobileOAMDemand], a
	ld bc, 52 - MOBILE_PEER_SCREEN_Y
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
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jr nz, .scratch_peer
	ld a, [wMobileVirtualLength]
	cp OAM_SIZE - 16 + 1
	ret nc
	push bc
	push de
	ld a, d
	ld [wMobileVirtualRectangleTop], a
	call MobileVirtualAdmitRectangleTop
	pop de
	pop bc
	jr nc, .peer_rectangle_ready
	ld a, [wMobileVirtualBaseReady]
	and a
	ret z
	ld a, 1
	ld [wMobileVirtualForceFull], a
	xor a
	ld [wMobileVirtualBaseReady], a
	ld a, 1
	ld [wMobileVirtualStage], a
	scf
	ret
.peer_rectangle_ready
	ld a, [wMobileVirtualLength]
	ld [wMobileOAMReadCursor], a
	add LOW(wMobileVirtualOutput)
	ld l, a
	adc HIGH(wMobileVirtualOutput)
	sub l
	ld h, a
	call MobileVirtualWritePeerQuads
	ld a, [wMobileOAMReadCursor]
	swap a
	and $f
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualExtraOwners
	add hl, de
	ld a, [wMobileBudgetIndex]
	inc a
	ld [hl], a
	ld a, [wMobileAvatarPalette]
	ld e, a
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [wMobileVirtualPaletteUsed]
	or [hl]
	ld [wMobileVirtualPaletteUsed], a
	ld a, [wMobileVirtualLength]
	add 16
	ld [wMobileVirtualLength], a
	jr .peer_admitted
.scratch_peer
	ld a, 16
	ld [wMobileVirtualGroupLength], a
	ld hl, wMobileVirtualGroup
	call MobileVirtualWritePeerQuads

	call MobileVirtualAdmitGroup
	ret c
	ld a, [wMobileVirtualLength]
	sub 16
	ld [wMobileOAMReadCursor], a
.peer_admitted
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
	ld a, 1
	ld [wMobileVirtualPlannedPrivateColors], a
.bound
	ld a, [wMobileBudgetIndex]
	inc a
	cp MOBILE_PEERS
	jr c, .next_phase
	xor a
.next_phase
	ld [wMobileVirtualNextPhase], a
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

MobileVirtualWritePeerQuads:
; DE screen origin, B mirrored tile base, C complete palette/bank/flip flags.
; A rectangle's output can be written once after admission, with no scratch copy.
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
if quad >= 2
; The third window has three free bank-0 tiles and one free bank-1 tile.
; Mirroring exchanges the two bottom source quadrants, including their bank.
	cp $6f
	jr nz, .ordinary_tile{d:quad}
	ld a, $3f
	ld [hli], a
	ld a, c
	or OAM_BANK1
	ld [hli], a
	jr .quad_done{d:quad}
.ordinary_tile{d:quad}
endc
	ld [hli], a
	ld [hl], c
	inc hl
if quad >= 2
.quad_done{d:quad}
endc
endr
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
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jr nz, .copy_output
	ld a, [wMobileVirtualGroupPointer + 1]
	cp HIGH(wShadowOAM)
	jr nz, .extra_owner
	ld a, [wMobileVirtualGroupPointer]
	swap a
	and $f
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualKeepSlots
	add hl, de
	ld [hl], 1
	jp .copied
.extra_owner
	ld a, [wMobileVirtualLength]
	swap a
	and $f
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualExtraOwners
	add hl, de
	ld a, [wMobileVirtualGroupOwner]
	ld [hl], a
.copy_output
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
	ld a, b ; mirrored/reversed templates can put the second row above the first
	cp OAM_Y_OFS
	jp c, .lines
	cp SCREEN_HEIGHT_PX + 1
	jp nc, .lines
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
	cp 9
	jp c, .next_quad
	cp 160
	jp nc, .next_quad
	cp OAM_Y_OFS
	jr c, .top_clip
	sub OAM_Y_OFS
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualLines
	add hl, de
	cp SCREEN_HEIGHT_PX - 8 + 1
	jr nc, .bottom_clip
; Ordinary visible quads need eight consecutive private counter updates.
; No per-row pointer construction, stack operations or LCD reads are needed.
rept 8
	ld a, [hl]
	add c
	ld [hli], a
	cp 11
	jr c, :+
	ld [wMobileVirtualRejected], a
:
endr
	jr .next_quad
.top_clip
	sub 8
	ld d, a
	ld hl, wMobileVirtualLines
	jr .clipped
.bottom_clip
	ld a, SCREEN_HEIGHT_PX
	sub e
	ld d, a
.clipped
	ld a, [hl]
	add c
	ld [hli], a
	cp 11
	jr c, .clip_within
	ld [wMobileVirtualRejected], a
.clip_within
	dec d
	jr nz, .clipped
.next_quad
	pop hl
	ld de, OBJ_SIZE
	add hl, de
	pop bc
	dec b
	jp nz, .quad_lines
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
MobileVirtualAdmitRectangleTop:
; A coarse sixteen-pixel bucket bounds ordinary rectangle overlap cheaply.
; A bucket overflow is not a rejection: exact relative start/end events handle
; that candidate, including clipped screen edges. Keep at most ten admitted tops
; in an unordered list; ordinary open scenes need no insertion sort per actor.
	ld a, [wMobileVirtualRectangleCount]
	cp OAM_COUNT / 4
	jp nc, .rejected
	ld a, [wMobileVirtualRectangleTop]
	swap a
	and $f
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualLines + 16
	add hl, de
	ld a, [hl]
	cp 5
	jr nc, .exact
	ld a, [wMobileVirtualRectangleTop]
	and $f
	jr z, .accepted
	inc hl
	ld a, [hl]
	cp 5
	jr nc, .exact
.accepted
	ld a, [wMobileVirtualRectangleTop]
	ld c, a
	swap a
	and $f
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualLines + 16
	add hl, de
	inc [hl]
	ld a, c
	and $f
	jr z, .record
	inc hl
	inc [hl]
.record
	ld a, [wMobileVirtualRectangleCount]
	ld e, a
	ld hl, wMobileVirtualLines
	add hl, de
	ld [hl], c
	ld hl, wMobileVirtualRectangleCount
	inc [hl]
	and a
	ret
.exact
	ld a, [wMobileVirtualCoarseTrial]
	and a
	jp nz, .rejected
; Count only overlap with this candidate's sixteen rows. Every previous
; interval contributes a start or an end event, rather than sixteen row writes.
	ld hl, wMobileVirtualLines + 32
	xor a
rept 16
	ld [hli], a
endr
	ld a, [wMobileVirtualRectangleCount]
	ld b, a
	ld a, [wMobileVirtualRectangleTop]
	ld d, a
	ld hl, wMobileVirtualLines
.event
	ld a, [hli]
	sub d
	cp 16
	jr c, .start_event
	cp -15
	jr c, .next_event
	add 16
	ld e, a
	push hl
	push de
	ld d, 0
	ld hl, wMobileVirtualLines + 32
	inc [hl]
	add hl, de
	dec [hl]
	pop de
	pop hl
	jr .next_event
.start_event
	ld e, a
	push hl
	push de
	ld d, 0
	ld hl, wMobileVirtualLines + 32
	add hl, de
	inc [hl]
	pop de
	pop hl
.next_event
	dec b
	jr nz, .event
	ld hl, wMobileVirtualLines + 32
	ld b, 16
	ld c, 0
.row
	ld a, [hli]
	add c
	ld c, a
	ld a, d
	inc d
	cp OAM_Y_OFS
	jr c, .next_row
	cp SCREEN_HEIGHT_PX + OAM_Y_OFS
	jr nc, .next_row
	ld a, c
	cp 5 ; adding two objects must leave no more than ten on any visible row
	jr nc, .rejected
.next_row
	dec b
	jr nz, .row
	jp .accepted
.rejected
	scf
	ret

MobileVirtualMergePalettes:
; Build all 64 bytes before publishing OAM. VBlank then uses the same unrolled
; upload as vanilla, without eight variable-cost palette-source calls.
	ld hl, wOBPals2
	ld de, wMobileVirtualMergedPalette
	ld bc, wMobileVirtualBindings
.loop
	push hl
	ld a, [bc]
	inc bc
	and a
	jr z, .copy
	cp MOBILE_PEERS + 1
	jr nc, .copy
	dec a
	add a
	add a
	add a
	add LOW(wMobilePrivatePalColors)
	ld l, a
	adc HIGH(wMobilePrivatePalColors)
	sub l
	ld h, a
.copy
rept 1 palettes
	ld a, [hli]
	ld [de], a
	inc de
endr
	pop hl
rept 1 palettes
	inc hl
endr
	ld a, c
	cp LOW(wMobileVirtualBindings + 8)
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
MobileVirtualFreeColor:
	DEF free_color = 255
for palette_mask, 1 << PAL_OW_WEATHER
	REDEF free_color = 255
	for virtual_color, PAL_OW_WEATHER
		if !(palette_mask & (1 << virtual_color)) && free_color == 255
			REDEF free_color = virtual_color
		endc
	endr
	db free_color
endr
	PURGE free_color
MobileVirtualRendererEnd::
ASSERT MobileVirtualRendererEnd - MobileVirtualRendererStart <= 4608

INCLUDE "mobile_adapter/rom/virtual_fast_scene.asm"
INCLUDE "mobile_adapter/rom/virtual_graphics.asm"
