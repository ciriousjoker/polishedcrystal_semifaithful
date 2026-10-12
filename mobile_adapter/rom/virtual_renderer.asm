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
	ldh a, [hCGBPalUpdate]
	and a
	jr z, .native_colors_current
	ld a, [wMobileVirtualNeedColorAudit]
	or $80
	ld [wMobileVirtualNeedColorAudit], a
.native_colors_current
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
; Cold peers cannot be drawn yet. Leave the idle window to transport and pose
; preparation instead of repeatedly rebuilding a scene with no usable guest.
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
for peer, 1, MOBILE_PEERS + 1
	ld a, 255
	ld [wMobilePeer{d:peer}OAMStart], a
	ld [wMobilePeer{d:peer}OAMStart + 1], a
endr
	ld a, [wMobilePeer1NativeStage]
	cp 7
	jr z, .ready_peer
	ld a, [wMobilePeer2NativeStage]
	cp 7
	jr z, .ready_peer
	ld a, [wMobilePeer3NativeStage]
	cp 7
	jr nz, .done
.ready_peer
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
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
	jp z, .abort
.abort
	xor a
	ld [wMobileVirtualStage], a
	ret
.start
	call MobileVirtualFastCase
	ldh a, [hUsedOAMIndex]
	cp OAM_SIZE - MOBILE_PEERS * 4 * OBJ_SIZE + 1
	ld a, MOBILE_PEERS
	jr c, .remote_limit
	ld a, 1
.remote_limit
	ld [wMobileVirtualRemoteLimit], a
	call MobileVirtualAlignedScene
.audit_colors
	ld a, [wMobileVirtualNeedColorAudit]
	bit 7, a
	jr z, .colors_current
	call MobileVirtualCurrentColors
.colors_current
	xor a
	ld [wMobileVirtualBaseReady], a
.reset
; Only row counters and ownership need zeroing. Admitted groups overwrite
; their output bytes; the unused OAM suffix is hidden once, at publication.
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jr nz, .ordinary_rows
	ld a, [wMobileVirtualForceFull]
	and a
	jp nz, .rows_ready
	ld hl, wMobileVirtualLines + 64
	xor a
rept 63
	ld [hli], a
endr
	jp .rows_ready
.ordinary_rows
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
.ordinary_guests
; Independent avatars precede ordinary figures only in presentation admission.
; Native NPC state/simulation remains untouched when total OAM capacity is full.
	ld a, [wMobileVirtualCursor]
	cp MOBILE_PEERS
	jr nc, .urgent_done
	ld b, a
	ld a, [wMobileRenderedCount]
	ld c, a
	ld a, [wMobileVirtualRemoteLimit]
	cp c
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
	cp MOBILE_PEERS
	jr nc, .urgent_done
	ld a, [wMobileRenderedCount]
	ld c, a
	ld a, [wMobileVirtualRemoteLimit]
	cp c
	jr z, .urgent_done ; a completed last guest needs no second large-job admission
	ret
.urgent_done
	xor a
	ld [wMobileVirtualCursor], a
	ld a, 4
	ld [wMobileVirtualStage], a
	ret

.native
.native_capacity
	ld a, [wMobileVirtualLength]
	cp OAM_SIZE
	jp nc, .native_raw_done
	ld a, [wMobileVirtualCursor]
	cp NUM_OBJECT_STRUCTS - 1
	jr c, .native_candidate
	xor a
	ld [wMobileVirtualCursor], a
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	ld a, 6
	jr nz, .native_done_stage
	ld a, 7
.native_done_stage
	ld [wMobileVirtualStage], a
	ret
.native_candidate
	inc a

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
	ld b, a
.ordinary_source
	and $1f
	jr nz, .native_present
.omitted_native_source
	ld hl, wMobileVirtualCursor
	inc [hl]
	jr .native
.native_present
	ld b, 110 ; retain the worst-case sixteen-quad construction/rollback bound
	cp 5
	jr nc, .native_shape_deadline
	ld b, 131
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jr nz, .native_shape_deadline
	ld b, 139 ; bounded canonical template and caller-return window
	bit 7, [hl]
	jr z, .native_shape_deadline
	ld b, 140 ; copying one already complete native rectangle is bounded
.native_shape_deadline
	ldh a, [rLY]
	cp b
	ret nc ; retain the candidate for the next admitted idle slice
	ld hl, wMobileVirtualCursor
	inc [hl]
	ld a, c
	call MobileVirtualNativeGroup
	ld a, [wMobileVirtualForceFull]
	and a
	jp nz, .native
	ret
.native_raw_done
	xor a
	ld [wMobileVirtualCursor], a
	ld a, 7 ; this path has no weather groups
	ld [wMobileVirtualStage], a
	ret
.weather
	ldh a, [rLY]
	cp 126
	jr nc, .weather_done
; Effects are optional presentation, unlike the native game update. Reserve the
; palette merge and complete OAM publication window before accepting more of
; their quads. A dense weather prefix must not discard every completed avatar.
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
	cp 9
	jr c, .weather ; skip native quads with no visible hardware row
	cp 160
	jr nc, .weather
	ld a, [wMobileVirtualLength]
	cp OAM_SIZE - OBJ_SIZE + 1
	jr nc, .weather_done ; the completed buffer cannot admit another effect
	call MobileVirtualAdmitWeatherQuad
	ret
.weather_done
	ld a, 7
	ld [wMobileVirtualStage], a
	ret
.publish
; Publish the already complete private DMA page, never another full OAM copy.
; The one-shot frame token keeps menus, stale states and new scenes native.
	ld hl, wMobileVirtualBindings
	xor a
rept 8
	or [hl]
	inc hl
endr
	ld [wMobileVirtualHasPrivateColors], a
	and a
	jr z, .colors_ready
	ld a, TRUE
	ldh [hCGBPalUpdate], a
.colors_ready
	ld a, [wMobileVirtualLength]
	ld l, a
	ld h, HIGH(wMobileVirtualOutput)
	ld b, OAM_SIZE
	ld a, l
	cp b
	jr nc, .suffix_done
	ld a, 160
.hide_suffix
	ld [hli], a
	inc l
	inc l
	inc l
	ld c, a
	ld a, l
	cp b
	ld a, c
	jr c, .hide_suffix
.suffix_done
	ld hl, wMobileVirtualBindings
	ld de, wMobilePrivatePalBindings
	ld bc, 8
	rst CopyBytes
	xor a
	ld [wMobileVirtualStage], a
	ldh a, [hVBlankCounter]
	ld [wMobileVirtualHeaderOAM], a
	ld a, 1
	ld [wMobileVirtualPreviousOAM], a
	ld [wMobileVirtualPublished], a
	ld a, [wMobileVirtualPhase]
	inc a
	cp MOBILE_PEERS
	jr c, .next_phase
	xor a
.next_phase
	ld [wMobileVirtualPhase], a
	ld a, [wMobileVirtualNativeRotation]
	inc a
	cp NUM_OBJECT_STRUCTS - 1
	jr c, .next_native_rotation
	xor a
.next_native_rotation
	ld [wMobileVirtualNativeRotation], a
	ret

MobileVirtualFrameValid:
; A private frame is valid only for the immediately following world VBlank.
	ld a, [wMobileVirtualPreviousOAM]
	and a
	ret z
	ld a, [wMobileWanted]
	and a
	ret z
	ld a, [wMobileOnline]
	and a
	ret z
	ld a, [wMobileLocalValid]
	and a
	ret z
	ldh a, [hInMenu]
	and a
	jr nz, .invalid
	ldh a, [hVBlank]
	and $f
	jr nz, .invalid
	ld a, [wMobileVirtualHeaderOAM]
	ld b, a
	ldh a, [hVBlankCounter]
	cp b
	jr nz, .invalid
	ld a, 1
	and a
	ret
.invalid
	xor a
	ret

MobileVirtualPushOAM::
; Native HRAM DMA code and its timing remain intact. Only its source page changes.
; Restore WBK and all registers preserved by the original ForcePushOAM routine.
	push hl
	push de
	ldh a, [rWBK]
	push af
	call MobileVirtualFrameValid
	push af
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
for peer, 1, MOBILE_PEERS + 1
	ld a, [wMobilePeer{d:peer}OAMStart + 1]
	ld [wMobilePeer{d:peer}OAMStart], a
endr
	xor a
	ld [wMobileVirtualPreviousOAM], a
	pop af
	jr z, .ordinary
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, HIGH(wMobileVirtualOutput)
	jr .dma
.ordinary
	ld a, HIGH(wShadowOAM)
.dma
	lb bc, 40 + 1, LOW(rDMA)
	call hPushOAM
	pop af
	ldh [rWBK], a
	pop de
	pop hl
	ret

MobileVirtualCopyRawRectangle:
; Source native ShadowOAM is read only. All chosen figures share one DMA page.
	ld a, [wMobileVirtualLength]
	ld e, a
	ld d, HIGH(wMobileVirtualOutput)
	ld a, [wMobileVirtualGroupPointer]
	ld l, a
	ld h, HIGH(wShadowOAM)
rept 16
	ld a, [hli]
	ld [de], a
	inc e
endr
	ld hl, wMobileVirtualLength
	ld a, [hl]
	add 16
	ld [hl], a
	ret

MobileVirtualAdmitWeatherQuad:
; HL is one current native effect quad with visible Y. Admit directly from OAM
; instead of materializing a general actor and copying its group twice. Read all
; affected row counters before incrementing, so rejection needs no rollback.
	ld a, [hl]
	cp 9
	ret c
	cp SCREEN_HEIGHT_PX + OAM_Y_OFS
	ret nc
	ld a, [wMobileVirtualLength]
	cp OAM_SIZE - OBJ_SIZE + 1
	ret nc
	and OBJ_SIZE - 1
	ret nz
	push hl
	ld de, 3
	add hl, de
	ld a, [hl]
	and OAM_PALETTE
	ld c, a
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualBindings
	add hl, de
	ld a, [hl]
	and a
	jp nz, .rejected ; a private hardware color cannot recolor a native effect
	pop hl
	push hl
	ld a, [hl]
	cp OAM_Y_OFS
	jr c, .top_clip
	sub OAM_Y_OFS
	ld e, a
	ld d, 0
	ld b, 8
	cp SCREEN_HEIGHT_PX - 8 + 1
	jr c, .rows_ready
	ld a, SCREEN_HEIGHT_PX
	sub e
	ld b, a
	jr .rows_ready
.top_clip
	sub OAM_Y_OFS - 8
	ld b, a ; visible Y=9..15 contributes one through seven rows
	ld de, 0
.rows_ready
	ld hl, wMobileVirtualLines
	add hl, de
	push hl
	push bc
.check
	ld a, [hli]
	cp 10
	jr nc, .rows_full
	dec b
	jr nz, .check
	pop bc
	pop hl
.increment
	inc [hl]
	inc hl
	dec b
	jr nz, .increment
	ld e, c
	ld d, 0
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [wMobileVirtualPaletteUsed]
	or [hl]
	ld [wMobileVirtualPaletteUsed], a
	ld a, [wMobileVirtualLength]
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualOutput
	add hl, de
	ld d, h
	ld e, l
	pop hl
rept OBJ_SIZE
	ld a, [hli]
	ld [de], a
	inc de
endr
	ld a, [wMobileVirtualLength]
	add OBJ_SIZE
	ld [wMobileVirtualLength], a
	ret
.rows_full
	pop bc
	pop hl
.rejected
	pop hl
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
	xor a
	ld [wMobileVirtualNativeAvatarColors], a
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
	ld a, c
	cp PAL_OW_WEATHER
	jr nz, .ordinary_color
	ld a, [wCurWeather]
	ld e, a
	ld a, [wOverworldWeatherCooldown]
	or e
	jr z, .ordinary_color
; The native weather manager reserves this fixed hardware slot until all
; particles are gone. It cannot be an exact avatar source or a private loan.
	ld a, [wMobileVirtualNativeAvatarColors]
	or 1 << PAL_OW_WEATHER
	ld [wMobileVirtualNativeAvatarColors], a
	jr .next
.ordinary_color
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
; Reserve exact avatar sources before any private guest borrows a palette.
; Otherwise the first guest can occupy a later guest's native color even when
; an unused alternative exists, creating artificial temporal palette pressure.
	push af
	ld hl, MobileVirtualPaletteBits
	ld e, c
	ld d, 0
	add hl, de
	ld a, [wMobileVirtualNativeAvatarColors]
	or [hl]
	ld [wMobileVirtualNativeAvatarColors], a
	pop af
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
	cp 8
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
	ld b, a
	ld a, [wMobileVirtualForceFull]
	and a
	ld a, b
	jp nz, MobileVirtualAlignedNativeGroup
	push af
	xor a
	ld [wMobileVirtualCoarseTrial], a
	pop af
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
; Reject a proven total-OAM conflict before copying a facing template. Nothing
; is omitted merely because background CPU time is short.
	push hl
	ld de, 6
	add hl, de
	ld a, [hl]
	and $1f
	add a
	add a
	ld b, a
	ld a, [wMobileVirtualLength]
	add b
	jr c, .unavailable_template
	cp OAM_SIZE + 1
	jr nc, .unavailable_template

.template_resources_ready
	pop hl
	jr .materialize_ready
.unavailable_template
	pop hl
	ret
.materialize_ready
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
; Read the audited immutable upper-bank mirror directly. Admission precedes
; materialization, so rejected figures require no quad copy or transform.
	ld hl, wMobileVirtualTemplate + 4
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jr nz, .general_template
	ld a, [wMobileVirtualGroupLength]
	cp 16
	jr nz, .general_template
	push hl
	inc hl
	inc hl
	ld a, [hl]
	ld [wMobileVirtualTemplate + 10], a
	call MobileVirtualTemplateRectangle
	pop hl
	ret c
	jr .emit_template
.general_template
	ld de, wMobileVirtualGroup
.emit_template
	ld a, [wMobileVirtualCoarseTrial]
	and a
	jr z, .ordinary_transform
	scf
	call MobileNativeRectangleTrampoline
	jr .transform_ready
.ordinary_transform
	call MobileVirtualWriteNativeQuads
.transform_ready
	jp .prepared_group

.prepared_group
	ld a, [wMobileVirtualCoarseTrial]
	and a
	jp nz, MobileVirtualTemplateRectangleDone
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

MobileVirtualWriteNativeQuads:
; HL immutable facing mirror, DE private output, template native origin/flags.

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
	ld a, [wMobileVirtualGroupPointer]
	ld c, a
	ld a, e
	sub c
	ld b, a
	ld a, [wMobileVirtualGroupLength]
	cp b
	jr nz, .loop
	ret
MobileVirtualTemplateRectangle:
; A canonical actor beyond vanilla OAM capacity is emitted directly into the
; private admitted output. Avoid constructing and then copying it a second time.
	ld a, [wMobileVirtualLength]
	cp OAM_SIZE - 16 + 1
	jp nc, .rejected
	ld a, [wMobileVirtualTemplate + 10]
	and NEXT_PALETTE
	ld b, 0
	jr z, .native_color
	inc b
.native_color
	ld a, [wMobileVirtualTemplate + 3]
	add b
	and OAM_PALETTE
	ld [wMobileVirtualGroupPalette], a
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualBindings
	add hl, de
	ld a, [hl]
	and a
	jp nz, .rejected
	ld a, [wMobileVirtualTemplate + 1]

.top
	and a
	jr z, .rejected
	cp 160
	jr nc, .rejected
	ld [wMobileVirtualRectangleTop], a
	call MobileVirtualAdmitRectangleTop
	ret c
	ld a, 1
	ld [wMobileVirtualCoarseTrial], a
	ld a, [wMobileVirtualLength]
	add LOW(wMobileVirtualOutput)
	ld e, a
	adc HIGH(wMobileVirtualOutput)
	sub e
	ld d, a
	ld a, e
	ld [wMobileVirtualGroupPointer], a
	ld a, d
	ld [wMobileVirtualGroupPointer + 1], a
	and a
	ret
.rejected
	scf
	ret

MobileVirtualTemplateRectangleDone:
	ld a, [wMobileVirtualLength]
	swap a
	and $f
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualExtraOwners
	add hl, de
	ld a, [wMobileVirtualGroupOwner]
	ld [hl], a
	ld a, [wMobileVirtualGroupPalette]
	ld e, a
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [wMobileVirtualPaletteUsed]
	or [hl]
	ld [wMobileVirtualPaletteUsed], a
	ld hl, wMobileVirtualLength
	ld a, [hl]
	add 16
	ld [hl], a
	ret

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
	call MobileVirtualAdmitRectangleTop
	pop de
	ret c
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [wMobileVirtualPaletteUsed]
	or [hl]
	ld [wMobileVirtualPaletteUsed], a
.keep
	jp MobileVirtualCopyRawRectangle

MobileVirtualPeerGroup:
; A pool index, cache and colors validated in bounded background work.
	ld [wMobileBudgetIndex], a
	inc a
	ld [wMobileVirtualGroupOwner], a
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
	ld a, [hl]
	ld b, a
	ld [wMobileAvatarTile], a
	ld de, MOBILE_PEER_LOADED_SPRITE - 18
	add hl, de
	ld a, [hl]
	cp b
	jr nz, .ineligible
; The committed pose may retain its previous movement mode while new graphics
; prepare. Its own uploaded tiles, facing and color remain authoritative.
	pop hl
	jr .eligible
.ineligible
	pop hl
	ret
.eligible
; Collect the committed pose in one bank-2 view. Subsequent admission only
; touches private bank-5 presentation data; no repeated peer lookup is needed.
	ld a, 1
	ldh [rWBK], a
	ld a, [wScriptRunning]
	ld c, a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, c
	cp $ff
	jr nz, .outside_menu
	push hl
	ld bc, MOBILE_PEER_SCREEN_X
	add hl, bc
	ld a, [hl]
	pop hl
	cp 9 * TILE_WIDTH
	ret nc
.outside_menu
	ld bc, MOBILE_PEER_SCREEN_X
	add hl, bc
	ld a, [hli]
	ld [wMobileOAMWriteCursor], a
	ld a, [hl]
	ld [wMobileOAMDemand], a
	ld bc, MOBILE_PEER_NATIVE_STAGE - MOBILE_PEER_SCREEN_Y
	add hl, bc
	ld a, [hl]
	cp 7
	ret nz
	ld bc, MOBILE_PEER_NATIVE_FACING - MOBILE_PEER_NATIVE_STAGE
	add hl, bc
	ld a, [hli]
	ld [wMobileVirtualFrame], a
	inc hl
	ld a, [hl]
	and a
	ret z
	ld [wMobileVirtualCacheHit], a
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
	ld a, [wMobileAvatarTile]
	ld b, a
	rrca
	and 1
	ld e, a
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
	cp 8
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
; Reserve the local color and every exact avatar source before borrowing a
; currently unused hardware slot. Rotate loans across all eight native slots.
	ld a, [wMobileVirtualPaletteUsed]
	ld b, a
	ld a, [wMobileVirtualNativeAvatarColors]
	or b
	ld b, a
	ld a, [wMobileVirtualBaseReady]
	and a
	ld a, b
	jr nz, .free_color_table
	cp 255
	jr z, .color_full
	ld a, [wMobileVirtualNativeRotation]
	and 7
	ld c, a
.color_probe
	ld e, c
	ld d, 0
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [hl]
	and b
	ld a, c
	jr z, .free_color_found
	inc a
	and 7
	ld c, a
	jr .color_probe
.free_color_table
	ld e, a
	ld d, 0
	ld hl, MobileVirtualFreeColor
	add hl, de
	ld a, [hl]
	cp 8
	jr nc, .color_full
.free_color_found
	ld [wMobileAvatarPalette], a
	jr .color_ready
.color_full
	scf
	ret

.color_ready
	ld a, [wMobileVirtualFrame]
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
	ld hl, wMobileVirtualGroup
	call MobileVirtualScratchGroup
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
	ld a, [wMobileBudgetIndex]
	add a
	add a
	add a
	ld e, a
	ld d, 0
	ld hl, wMobilePrivatePalColors
	add hl, de
	push hl
	ld hl, wMobileVirtualFrameColors
	add hl, de
	ld d, h
	ld e, l
	pop hl
rept 1 palettes
	ld a, [hli]
	ld [de], a
	inc de
endr
.bound
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, MOBILE_PEER_OAM_START + 1
	add hl, bc
	ld a, [wMobileOAMReadCursor]
	ld [hl], a
	ld bc, MOBILE_PEER_PALETTE - MOBILE_PEER_OAM_START - 1
	add hl, bc
	ld a, [wMobileAvatarPalette]
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
	ld a, [wMobileVirtualForceFull]
	and a
	jp nz, MobileVirtualAdmitAlignedRectangle
MobileVirtualAdmitExactRectangle:

; Three bit planes count admitted rectangles on every actual visible row.
; Each candidate touches at most three eight-row chunks. Check all chunks
; before updating any plane, so rejection is atomic and needs no rollback.
; This is exact, including clipped edges, with no coarse bucket fallback.
	ld a, [wMobileVirtualRectangleCount]
	cp OAM_COUNT / 4
	jp nc, .rejected
	ld a, [wMobileVirtualRectangleTop]
	ld e, a
	ld d, 0
	ld hl, MobileVirtualRectangleMasks
	add hl, de
	add hl, de
	add hl, de
	add hl, de
	ld a, [hli]
	ld [wMobileVirtualLines + 128], a
for chunk, 3
	ld a, [hli]
	ld [wMobileVirtualLines + 129 + chunk], a
endr
	ld a, [wMobileVirtualLines + 128]
	ld e, a
	ld hl, wMobileVirtualLines + 64
	add hl, de
for chunk, 3
	ld a, [wMobileVirtualLines + 129 + chunk]
	ld d, a
	ld a, [hli]
	ld b, a
	ld a, [hli]
	or b
	ld b, a
	ld a, [hli]
	and b
	and d
	jp nz, .rejected
endr
	ld a, [wMobileVirtualLines + 128]
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualLines + 64
	add hl, de
for chunk, 3
	ld a, [wMobileVirtualLines + 129 + chunk]
	ld d, a
	ld a, [hl]
	ld b, a
	xor d
	ld [hli], a
	ld a, b
	and d
	ld e, a
	ld a, [hl]
	ld b, a
	xor e
	ld [hli], a
	ld a, b
	and e
	ld e, a
	ld a, [hl]
	xor e
	ld [hli], a
endr
	ld a, [wMobileVirtualRectangleCount]
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualLines
	add hl, de
	ld a, [wMobileVirtualRectangleTop]
	ld [hl], a
	ld hl, wMobileVirtualRectangleCount
	inc [hl]
	and a
	ret
.rejected
	scf
	ret

MobileVirtualRectangleMasks:
; Hardware Y is screen Y plus sixteen. Counter chunks represent only rows
; 16..159, so offscreen clipping cannot cause an artificial resource conflict.
for top, 256
	DEF clipped_top = top
	if top < OAM_Y_OFS
		REDEF clipped_top = OAM_Y_OFS
	endc
	DEF clipped_end = top + 16
	if clipped_end > SCREEN_HEIGHT_PX + OAM_Y_OFS
		REDEF clipped_end = SCREEN_HEIGHT_PX + OAM_Y_OFS
	endc
	DEF chunk_top = clipped_top
	if chunk_top >= SCREEN_HEIGHT_PX + OAM_Y_OFS
		REDEF chunk_top = SCREEN_HEIGHT_PX + OAM_Y_OFS - 1
	endc
	DEF row_chunk = (chunk_top - OAM_Y_OFS) / 8
	db row_chunk * 3
	for chunk, 3
		DEF row_mask = 0
		for bit_index, 8
			DEF hardware_row = OAM_Y_OFS + (row_chunk + chunk) * 8 + bit_index
			if hardware_row >= clipped_top && hardware_row < clipped_end
				REDEF row_mask = row_mask | (1 << bit_index)
			endc
			PURGE hardware_row
		endr
		db row_mask
		PURGE row_mask
	endr
	PURGE clipped_top, clipped_end, chunk_top, row_chunk
endr

MobileVirtualMergePalettes:
; Copy directly from each immutable source. Fixed hardware slots avoid saving
; and advancing a native pointer through colors that use a private source.
for hardware_color, 8
	ld hl, wOBPals2 + hardware_color palettes
	ld de, wMobileVirtualMergedPalette + hardware_color palettes
	ld a, [wMobileVirtualBindings + hardware_color]
	and a
	jr z, .copy{d:hardware_color}
	cp MOBILE_PEERS + 1
	jr nc, .copy{d:hardware_color}
	dec a
	add a
	add a
	add a
	add LOW(wMobilePrivatePalColors)
	ld l, a
	adc HIGH(wMobilePrivatePalColors)
	sub l
	ld h, a
.copy{d:hardware_color}
rept 1 palettes
	ld a, [hli]
	ld [de], a
	inc de
endr
endr
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
for palette_mask, 1 << 8
	REDEF free_color = 255
	for virtual_color, 8
		if !(palette_mask & (1 << virtual_color)) && free_color == 255
			REDEF free_color = virtual_color
		endc
	endr
	db free_color
endr
	PURGE free_color
MobileVirtualRendererEnd::
ASSERT MobileVirtualRendererEnd - MobileVirtualRendererStart <= 6144

INCLUDE "mobile_adapter/rom/virtual_fast_scene.asm"
INCLUDE "mobile_adapter/rom/virtual_graphics.asm"
