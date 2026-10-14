; Complete-figure OAM admission runs only in native idle time. Ordinary actors
; are immutable descriptors, never leased, deleted, frozen or reclassified.
SECTION "Mobile Virtual Renderer", ROMX, BANK[MOBILE_ROM_BANK]
MobileVirtualRendererStart::

MobileVirtualBeginRetainedComposition:
 ; Source capture owns bank 5 and preserves the complete native caller ABI.
 ld a, [wMobileNativeGraphicsFence]
 and a
 ret nz
 ldh a, [hInMenu]
 and a
 ret nz
 ld a, [wMobileWanted]
 and a
 ret z
 ld a, [wMobileOnline]
 and a
 ret z
 ld a, 1
 ld [wMobileVirtualStage], a
 xor a
 ld [wMobileVirtualFastAdmission], a
 ret

MobileVirtualComposeStep::
 ld a, [wMobileVirtualStage]
 and a
 ret z
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileOnline]
 and a
 jp z, .abort
 ld a, [wMobileWanted]
 and a
 jp z, .abort
 ld a, [wMobileStableSourceValid]
 cp 1
 jp nz, .abort
 ld a, [wMobileVirtualStage]
 cp 1
 jr z, .start
 cp 2
 jp z, .native
 cp 3
 jp z, .peers
 cp 6
 jp z, .weather
 cp 7
 jp z, .publish
 jp .abort
.start
 call MobileNativePrepareSpritesWork
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 xor a
 ld [wMobileVirtualFastAdmission], a
 ld [wMobileVirtualPeerMask], a
 ld [wMobileVirtualForceFull], a
 ld [wMobileVirtualLength], a
 ld [wMobileVirtualPaletteUsed], a
 ld [wMobileVirtualNativeAvatarColors], a
 ld [wMobileVirtualCursor], a
 ld [wMobileRenderedCount], a
 ld [wMobileVirtualPlannedPrivateColors], a
 ld [wMobileStableCapacityValid], a
 ld [wMobileStableEligible], a
 ld [wMobileStableEligible + 1], a
 ld [wMobileStablePendingPrivate], a
 ld [wMobileStablePendingPrivate + 1], a
 ld hl, wMobileVirtualLines
 ld b, SCREEN_HEIGHT_PX
.clear_rows
 ld [hli], a
 dec b
 jr nz, .clear_rows
 ld hl, wMobileVirtualBindings
 ld b, 8
.clear_bindings
 ld [hli], a
 dec b
 jr nz, .clear_bindings
 ld a, 6
 ld [wMobileVirtualStage], a
 ret
.weather
 ld a, [wMobileVirtualCursor]
 cp OAM_COUNT
 jr nc, .weather_done
 ld c, a
 ld e, a
 ld d, 0
 ld hl, wMobileStableOwners
 add hl, de
 ld a, [hl]
 cp $ff
 jr nz, .weather_next
 call MobileVirtualRetainedTuple
 ld a, [hl]
 cp 9
 jr c, .weather_next
 cp SCREEN_HEIGHT_PX + OAM_Y_OFS
 jr nc, .weather_next
 ld a, [wMobileVirtualLength]
 push af
 call MobileVirtualAdmitWeatherQuad
 pop bc
 ld a, [wMobileVirtualLength]
 cp b
 jp z, .abort
.weather_next
 ld hl, wMobileVirtualCursor
 inc [hl]
 ret
.weather_done
 xor a
 ld [wMobileVirtualCursor], a
 ld a, 2
 ld [wMobileVirtualStage], a
 ret
.native
 ; Native exact colors reserve their slots before any private color is lent.
 ; No admission-order rejection can silently remove an intended figure.
 call MobileVirtualNativeDeadline
 ret nc
 ld a, [wMobileVirtualCursor]
 cp NUM_OBJECT_STRUCTS
 jr nc, .native_done
 ld hl, wMobileVirtualCursor
 inc [hl]
 call MobileVirtualNativeGroup
 ret
.native_done
 xor a
 ld [wMobileVirtualCursor], a
 ld a, 3
 ld [wMobileVirtualStage], a
 ret
.peers
 ld a, [wMobileVirtualCursor]
 cp MOBILE_PEERS
 jr nc, .peers_done
 ld hl, wMobileVirtualCursor
 inc [hl]
 call MobilePhaseMarkVisiblePeer
 call MobileVirtualPeerGroup
 ret
.peers_done
 ld a, 7
 ld [wMobileVirtualStage], a
 ret
.publish
 ; Certify the entire all-fit transaction before its one final publication.
 ld a, [wMobileStableEligible]
 ld b, a
 ld a, [wMobileStablePendingPrivate]
 cp b
 jp nz, .abort
 ld a, [wMobileStableEligible + 1]
 ld b, a
 ld a, [wMobileStablePendingPrivate + 1]
 cp b
 jp nz, .abort
 ld a, 1
 ld [wMobileStableCapacityValid], a
 mobile_virtual_publish_page MobileVirtualComposeStep.abort
.abort
 xor a
 ld [wMobileStableCapacityValid], a
 ld [wMobileVirtualStage], a
 ret

; The finite existing mask schema uses native slots 0..12, peers 13..15.
; Marking preserves every caller register, including flags.
MobilePhaseSetMaskBit:
 push af
 push bc
 push de
 push hl
 ld b, a
 and 7
 ld c, a
 ld a, b
 cp 8
 jr c, .low
 inc hl
.low
 ld a, 1
 inc c
.shift
 dec c
 jr z, .store
 add a
 jr .shift
.store
 or [hl]
 ld [hl], a
 pop hl
 pop de
 pop bc
 pop af
 ret

MobilePhaseMarkVisiblePeer:
 ; Eligibility precedes cache/upload readiness. A cold figure is a failed
 ; completion obligation, never an invented graphic-resource shortage.
 push af
 push bc
 push de
 push hl
 ld b, a
 ld a, [wMobilePeerCount]
 cp b
 jr z, .done
 jr c, .done
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 ld a, b
 call MobileNativePeerPointer
 ld de, MOBILE_PEER_VISIBLE
 add hl, de
 ld a, [hl]
 and a
 jr z, .done
 ld a, [wMobileVirtualCursor]
 dec a
 add NUM_OBJECT_STRUCTS
 ld hl, wMobileStableEligible
 ld c, a
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, c
 call MobilePhaseSetMaskBit
.done
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 pop hl
 pop de
 pop bc
 pop af
 ret

MobileVirtualBuildOrder:
	mobile_virtual_build_order

MobileVirtualOrderedIndex:
; Resolve from the one compact rotated list. Save its ordinal for fairness.
	ld a, [wMobileVirtualCursor]
	ld [wMobileVirtualSelectedOrdinal], a
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualOrder
	add hl, de
	ld a, [hl]
	ret

MobileVirtualAdvanceNativeClass:
	mobile_virtual_advance_class

MobileVirtualFrameValid:
	ld a, [wMobileStableSourceValid]
	cp 1
	jr nz, .invalid
	ld a, [wMobileStableActorViewValid]
	cp 1
	jr nz, .invalid
	ld a, [wMobileStablePendingValid]
	cp 1
	jr nz, .invalid
	ld a, [wMobileNativeGraphicsFence]
	and a
	jr nz, .invalid
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

MobileVirtualChooseOutput::
 ldh a, [rWBK]
 push af
 ; One VBlank decision is shared by OBJ palette and OAM consumers.
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ldh a, [hOAMUpdate]
 and a
 jr nz, .retain
 call MobileVirtualFrameValid
 jr z, .native
 ; Pending native tile transfers can conflict with the captured presentation.
 ldh a, [hRequested2bpp]
 ld b, a
 ldh a, [hRequested1bpp]
 or b
 jr nz, .native
 ldh a, [hDMATransfer]
 and a
 jr nz, .native
 ldh a, [rVDMA_LEN]
 bit B_VDMA_LEN_BUSY, a
 jr z, .native
 ld a, 2
 ld [wMobileStableFinalDecision], a
 ld a, TRUE
 ldh [hCGBPalUpdate], a
 jr .done
.native
 ld a, 1
 ld [wMobileStableFinalDecision], a
 ld a, [wMobileStableHardwarePaletteProvenance]
 cp 1
 jr z, .done
 ld a, TRUE
 ldh [hCGBPalUpdate], a
 jr .done
.retain
 ld a, 3
 ld [wMobileStableFinalDecision], a
 jr .done

 .done
 pop af
 ldh [rWBK], a
 ret

MobileVirtualPushOAM::
 push af
 push hl
 push de
 ldh a, [rWBK]
 push af
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileStableFinalDecision]
 and $7f
 cp 2
 jr nz, .ordinary
 ; No second FrameValid decision after private RGB has been exposed.
 ld a, HIGH(wMobileVirtualOutput)
 push af
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
for peer, 1, MOBILE_PEERS + 1
 ld a, [wMobilePeer{d:peer}OAMStart + 1]
 ld [wMobilePeer{d:peer}OAMStart], a
endr
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 pop af
 jr .dma
.ordinary
 ld hl, sp + 1
 ld a, [hl]
 ldh [rWBK], a
 ld a, HIGH(wShadowOAM)
.dma
 ; Preserve the native DMA caller flags, including direct ForcePushOAM carry.
 ld d, a
 ld hl, sp + 6
 ld e, [hl]
 push de
 pop af
 mobile_actual_oam_dma_latch b
 lb bc, 40 + 1, LOW(rDMA)
 call hPushOAM
 push af
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 xor a
 ld [wMobileStablePendingValid], a
 ld [wMobileVirtualPreviousOAM], a
 ld [wMobileStableFinalDecision], a
 ; The legacy presentation rotation advances only on an actual DMA.
 ld a, [wMobileVirtualPhase]
 inc a
 cp MOBILE_PEERS
 jr c, .phase
 xor a
.phase
 ld [wMobileVirtualPhase], a
 ld hl, sp + 3
 ld a, [hl]
 ldh [rWBK], a
 pop af
 inc sp
 inc sp
 pop de
 pop hl
 inc sp
 inc sp
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

MobileVirtualNativeGroup:
 ld [wMobileVirtualIndex], a
 and a
 ld a, $ff
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
 ld h, HIGH(wMobileVirtualDescriptors)
 ld de, wMobileVirtualTemplate
 rept 8
 ld a, [hli]
 ld [de], a
 inc de
 endr
 call MobileVirtualEmitImmutableGroup
 ld a, [wMobileVirtualRejected]
 cp 1
 ret nz
 ld hl, wMobileVirtualGroup
 call MobileVirtualScratchGroup
	call MobileVirtualGroupVisible
	ret z
 ld a, [wMobileVirtualIndex]
 ld hl, wMobileStableEligible
 call MobilePhaseSetMaskBit
; Native palette ownership blocks private colors for exactly these presentations.
	ld a, [wMobileVirtualGroupPointer]
	ld l, a
	ld a, [wMobileVirtualGroupPointer + 1]
	ld h, a
	ld de, 3
	add hl, de
	ld a, [wMobileVirtualGroupLength]
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
 call MobileVirtualAdmitGroup
 ret c
 ld a, [wMobileVirtualIndex]
 ld hl, wMobileStablePendingPrivate
 jp MobilePhaseSetMaskBit

MobileVirtualGatherPeer::
	mobile_virtual_gather_peer MobileNativePeerPointer, MobileVirtualCheckExactRectangle

MobileVirtualPeerGroup:
 ld b, a

.gather
	ld a, b
	call MobileVirtualGatherPeer
.gathered
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileVirtualPeerGatherValid]
	and a
	ret z
 ; Constructed canonical colors must be clean for this source's lighting.
 ld a, [wMobileVirtualCacheHit]
 cp 1
 ret nz
 ld a, [wMobileBudgetIndex]
 ld e, a
 ld d, 0
 ld hl, MobileVirtualPaletteBits
 add hl, de
 ld a, [wMobilePrivatePalDirty]
 and [hl]
 ret nz
 ld a, [wPalFadeDelayFrames]
 and a
 ret nz ; exact intermediate fade construction remains an explicit cadence gap
 call MobileVirtualFindRetainedPeerColor
 jr c, .private_color
 ld [wMobileAvatarPalette], a
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
 jr MobileVirtualMaterializePeer
.private_color
 call MobileVirtualChoosePrivatePalette
 ret c
 jr MobileVirtualMaterializePeer

MobileVirtualChoosePrivatePalette:
	mobile_virtual_choose_private_palette MobileVirtualPaletteBits, MobileVirtualFreeColor

MobileVirtualMaterializePeer::
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
	ld a, [wMobileAvatarPalette]
	ld e, a
	ld d, 0
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
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, MobileVirtualPaletteBits
	add hl, de
	ld a, [wMobileVirtualPeerMask]
	or [hl]
	ld [wMobileVirtualPeerMask], a
	ld a, [wMobileVirtualCacheHit]
	cp 2
	jr nz, .private_rgb
	ld a, [wMobileAvatarPalette]
	add a
	add a
	add a
	ld e, a
	ld d, 0
	ld hl, wMobileStableNativeRGB
	add hl, de
	push hl
	ld a, [wMobileBudgetIndex]
	add a
	add a
	add a
	ld e, a
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
	jr .bound
.private_rgb
	ld a, [wMobileAvatarPalette]
	ld [wMobileVirtualDroppedSlot], a ; aligned mode admits exactly one guest
	inc a
	and 7
	ld [wMobileVirtualPaletteRotation], a
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
.count_peer
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileBudgetIndex]
 add NUM_OBJECT_STRUCTS
 ld hl, wMobileStablePendingPrivate
 call MobilePhaseSetMaskBit
	ld hl, wMobileRenderedCount
	inc [hl]
	ret

MobileVirtualWritePeerQuads:
	mobile_virtual_write_peer_quads

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
	jp MobileVirtualAdmitExactRectangle

MobileVirtualCheckExactRectangle:

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
	and a
	ret
.rejected
	scf
	ret

MobileVirtualAdmitExactRectangle:
	call MobileVirtualCheckExactRectangle
	ret c
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
	mobile_virtual_free_palette_table
MobileVirtualRendererEnd::
ASSERT MobileVirtualRendererEnd - MobileVirtualRendererStart <= 6144

INCLUDE "mobile_adapter/rom/retained_source.asm"
INCLUDE "mobile_adapter/rom/native_palette_write_guards.asm"
INCLUDE "mobile_adapter/rom/retained_emitter.asm"
INCLUDE "mobile_adapter/rom/virtual_graphics.asm"


MobileVirtualRetainedTuple:
 ld e, c
 ld d, 0
 ld hl, wMobileStableOriginalYs
 add hl, de
 ld a, [hl]
 ld [wMobileVirtualTemplate], a
 ld a, c
 add a
 add c
 ld e, a
 ld hl, wMobileStableRawXTileFlags
 add hl, de
 ld de, wMobileVirtualTemplate + 1
 rept 3
 ld a, [hli]
 ld [de], a
 inc de
 endr
 ld hl, wMobileVirtualTemplate
 ret

MobileVirtualFindRetainedPeerColor:
 ; Compare exact canonical cache RGB with each frozen native source.
 ld a, [wMobileBudgetIndex]
 add a
 add a
 add a
 ld e, a
 ld d, 0
 ld hl, wMobilePrivatePalColors
 add hl, de
 push hl
 ld c, 0
.palette
 pop de
 push de
 ld a, c
 add a
 add a
 add a
 add LOW(wMobileStableNativeRGB)
 ld l, a
 adc HIGH(wMobileStableNativeRGB)
 sub l
 ld h, a
 ld b, 8
.compare
 ld a, [de]
 cp [hl]
 jr nz, .next
 inc de
 inc hl
 dec b
 jr nz, .compare
 pop de
 ld a, c
 and a
 ret
.next
 inc c
 ld a, c
 cp 8
 jr c, .palette
 pop de
 scf
 ret
