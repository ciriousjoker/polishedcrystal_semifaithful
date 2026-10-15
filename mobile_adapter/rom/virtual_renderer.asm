; Complete measured cache-hit paths, including foreground RGB staging and
; caller return: one36808T, two42408T, three49208T. Prices retain phase/return
; reserve; an invalid native RGB lease additionally reserves seven LCD lines.
DEF MOBILE_COMPACT_COLD_ONE_T EQU 39000
DEF MOBILE_COMPACT_COLD_TWO_T EQU 45600
DEF MOBILE_COMPACT_COLD_THREE_T EQU 51000
DEF MOBILE_COMPACT_COLD_THREE_FAST_T EQU 48500
DEF MOBILE_COMPACT_LINE_T EQU 912
DEF MOBILE_COMPACT_NATIVE_LINE EQU 144
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
 ; Admit the complete cold cohort once. Constructor and transfer work cannot
 ; stop between eligible peers and silently publish a proper subset.
 ld c, 0
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
for peer, 1, MOBILE_PEERS + 1
 ld a, [wMobilePeer{d:peer}Visible]
 and a
 jr z, .count_cold_next{d:peer}
 ld a, [wMobilePeer{d:peer}NativeStage]
 cp 7
 jr z, .count_cold_next{d:peer}
 inc c
.count_cold_next{d:peer}
endr
 ld a, c
 and a
 jr z, .cold_jobs_done
 ; Price the whole foreground transaction, including immutable descriptors,
 ; transfer phase, exact emission, RGB publication and native caller return.
 ld b, MOBILE_COMPACT_NATIVE_LINE - MOBILE_COMPACT_COLD_ONE_T / MOBILE_COMPACT_LINE_T - 1
 dec a
 jr z, .cold_admit
 ld b, MOBILE_COMPACT_NATIVE_LINE - MOBILE_COMPACT_COLD_TWO_T / MOBILE_COMPACT_LINE_T - 1
 dec a
 jr z, .cold_admit
 ld b, MOBILE_COMPACT_NATIVE_LINE - MOBILE_COMPACT_COLD_THREE_T / MOBILE_COMPACT_LINE_T - 1
.cold_admit
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileNativeRGBLeaseValid]
 and a
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 jr nz, .native_rgb_priced
 ld a, b
 sub 7
 ld b, a
.native_rgb_priced
 ; Fresh-HBlank GDMA replaces four line waits only when native LCD IRQ is off.
 ; Complete core46444T plus separately charged256T hardware64B transfer,
 ; phase/return reserve and bounded helper support48500T. STAT uses51000T.
 ld a, c
 cp 3
 jr nz, .cold_price_ready
 ldh a, [rIE]
 and IE_STAT
 jr nz, .cold_price_ready
 ld a, b
 add MOBILE_COMPACT_COLD_THREE_T / MOBILE_COMPACT_LINE_T - MOBILE_COMPACT_COLD_THREE_FAST_T / MOBILE_COMPACT_LINE_T
 ld b, a
.cold_price_ready
 ldh a, [rLY]
 cp b
 ret nc
 farcall MobileCompactRetirePublication
 farcall MobileCompactFreshResources
 jp c, .abort
 jr .cold_prepare_done
.cold_jobs_done
 farcall MobileCompactRetirePublication
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileVirtualQueuedPeer]
 ld [wMobileStablePoseMask], a
 ld a, [wMobileStablePoseGeneration]
 ld [wMobileStablePoseToken], a
 ld a, [wMobileVirtualQueuedPeer]
 and a
 jr z, .cold_prepare_done
 call MobileVirtualPrevalidatePoseBatch
 jr c, .cold_prepare_done
 call MobileVirtualPrepareColdPose
.cold_prepare_done
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileVirtualQueuedPeer]
 ld [wMobileStablePoseMask], a
 ld a, [wMobileStablePoseGeneration]
 ld [wMobileStablePoseToken], a
 xor a
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
 farcall MobileVirtualCertifyCompact
 jr c, .general_rows
 ; The certificate leaves the complete native prefix in physical OAM order.
 ld a, 3
 ld [wMobileVirtualFastAdmission], a
 farcall MobileCompactEmitCohort
 jp c, .abort
 jp .publish
.general_rows
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
 ld a, [wMobileStableUnknownCount]
 and a
 jr z, .weather_done
.weather_scan
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
 jr nz, .weather_skip
 call MobileVirtualRetainedTuple
 ld a, [hl]
 cp 9
 jr c, .weather_skip
 cp SCREEN_HEIGHT_PX + OAM_Y_OFS
 jr nc, .weather_skip
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
.weather_skip
 ld hl, wMobileVirtualCursor
 inc [hl]
 jr .weather_scan
.weather_done
 xor a
 ld [wMobileVirtualCursor], a
 ld a, 2
 ld [wMobileVirtualStage], a
 ret
.native
 ; Empty descriptors have no figure, RGB or row work. Drain their bounded
 ; hardware count together instead of spending a complete idle dispatch each.
.empty_native
 ld a, [wMobileVirtualCursor]
 cp NUM_OBJECT_STRUCTS
 jr nc, .native_done
 add a
 add a
 add a
 add LOW(wMobileVirtualDescriptors + 6)
 ld l, a
 ld h, HIGH(wMobileVirtualDescriptors + 6)
 ld a, [hl]
 and $1f
 jr nz, .occupied_native
 ld hl, wMobileVirtualCursor
 inc [hl]
 jr .empty_native
.occupied_native
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
 farcall MobileVirtualPublish
 ret
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
 ld a, [wMobileStableFinalDecision]
 and a
 jr nz, .done
 ldh a, [hOAMUpdate]
 and a
 jr nz, .retain
 call MobileVirtualFrameValid
 jr z, .native
 ; The complete native-request/pose proof was sealed in foreground. IRQ work
 ; consumes the one-shot authority plus fixed hardware cancellation state.
 ldh a, [hDMATransfer]
 and a
 jr nz, .native
 ldh a, [rVDMA_LEN]
 bit B_VDMA_LEN_BUSY, a
 jr z, .native
 ld a, [wMobileStableFinalDecision]
 cp $82
 jr z, .private_selected
 ld a, 2
 ld [wMobileStableFinalDecision], a
.private_selected
 jr .done
.native
 ld a, 1
 ld [wMobileStableFinalDecision], a
 ld a, [wMobileStableHardwarePaletteProvenance]
 cp 1
 jr z, .done
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
 call MobileVirtualChooseOutput
 ld a, [wMobileStableFinalDecision]
 and $7f
 cp 2
 jr nz, .ordinary
 ; No second FrameValid decision after private RGB has been exposed.
 call MobileVirtualUpdateOBJOnly
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, HIGH(wMobileVirtualOutput)
 jr .dma
.ordinary
 call MobileVirtualUpdateOBJOnly
 ld a, HIGH(wShadowOAM)
.dma
 ; Preserve the native DMA caller flags, including direct ForcePushOAM carry.
 ld d, a
 ld hl, sp + 6
 ld e, [hl]
 push de
 pop af
 mobile_actual_oam_dma_latch b
 push af
 ld a, [wMobileStableFinalDecision]
 cp 2
 jr z, .private_overlap
 cp 1
 jr nz, .ordinary_dma
 ld a, [wMobileStableHardwarePaletteProvenance]
 cp 1
 jr z, .ordinary_dma
 ld a, [wMobileNativeRGBLeaseValid]
 cp 1
 jr nz, .ordinary_dma
 ld hl, $8740
 jr .overlap_ready
.private_overlap
 ld hl, $8700
.overlap_ready
 pop af
 call MobileVirtualOverlapDMA
 jr .dma_finished
.ordinary_dma
 pop af
 lb bc, 40 + 1, LOW(rDMA)
 call hPushOAM
.dma_finished
 push af
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ; Record only the membership whose page has just physically completed DMA.
 ld a, [wMobileActualOAMPage]
 cp HIGH(wMobileVirtualOutput)
 jr nz, .native_actual
 ld a, [wMobileStablePendingPrivate]
 ld [wMobileStableLastActual], a
 ld a, [wMobileStablePendingPrivate + 1]
 ld [wMobileStableLastActual + 1], a
 jr .actual_ready
.native_actual
 xor a
 ld [wMobileStableLastActual], a
 ld [wMobileStableLastActual + 1], a
.actual_ready
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

MobileVirtualOverlapDMA:
 ld d, a
 push af
 ldh a, [rVBK]
 push af
 ld a, 1
 ldh [rVBK], a
 ld a, $80
 ldh [rOBPI], a
 ld c, LOW(rOBPD)
 ld a, d
.overlap_trigger
 ldh [rDMA], a
 rept 64
 ld a, [hli]
 ldh [c], a
 endr
.overlap_done
 ; The1024T unrolled body has completed the physical640T OAM transfer.
 pop af
 ldh [rVBK], a
 ld a, d
 cp HIGH(wMobileVirtualOutput)
 ld a, 1
 jr nz, .native_provenance
 inc a
.native_provenance
 ld [wMobileStableHardwarePaletteProvenance], a
 pop af
 ld c, LOW(rDMA)
 ld b, 1
 dec b
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
 ; Empty native descriptors emit no quads and reserve no palette or rows.
 ; Avoid copying and rejecting the same empty figure through the general path.
 push hl
 ld de, 6
 add hl, de
 ld a, [hl]
 and $1f
 pop hl
 ret z
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

; The batch marker and generation bind the prepared OAM to exact pose jobs.
; A changed batch never turns a previously composed page into an upload plan.
MobileVirtualPoseBatchMatches:
 ld a, [wMobileVirtualQueuedPeer]
 ld b, a
 ld a, [wMobileStablePoseMask]
 cp b
 ret nz
 ld a, [wMobileStablePoseGeneration]
 ld b, a
 ld a, [wMobileStablePoseToken]
 cp b
 ret

MobileVirtualPeerHasStagedPose:
 push bc
 push de
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
 pop de
 pop bc
 ret

MobileVirtualGatherStagedPose:
 call MobileVirtualPeerHasStagedPose
 ret z ; AND cleared carry: continue with the committed pose
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileBudgetIndex]
 ld [wMobileVirtualWindow], a
 add a
 add a
 add a
 ld e, a
 ld d, 0
 ld hl, wMobileStablePoseBatch + 4 ; immutable facing, including mirror
 add hl, de
 ld a, [hl]
 ld [wMobileVirtualFrame], a
 ld a, 1
 ld [wMobileVirtualCacheHit], a
 ld [wMobileVirtualPeerGatherValid], a
 ld a, $ff
 ld [wMobileAvatarPalette], a
 scf
 ret

MobileVirtualGatherCompactPeer:
MobileVirtualGatherPeer::
	mobile_virtual_gather_peer MobileNativePeerPointer, MobileVirtualCheckExactRectangle

MobileVirtualPeerGroup:
 call MobileVirtualGatherPeer
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
 ld a, [wMobileVirtualFastAdmission]
 cp 3
 jr nz, .exact_color
 ; A distinct unused hardware slot is sufficient; exact deduplication remains
 ; the fallback when native allocation has exhausted the free set.
 call MobileVirtualChoosePrivatePalette
 jr nc, MobileVirtualMaterializePeer
.exact_color
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
 jr nz, .private_color ; a borrowed match does not establish palette pressure
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
 cp 3
 jr z, .peer_rectangle_ready
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

; Foreground only, before PendingValid. All aliases/bulk template writers use
; <=8 bytes. IRQ consumption owns CPU until both RGB and OAM use finish.
SECTION "Mobile Compact Transaction", ROMX, BANK[MOBILE_ROM_BANK + 5]
MobileVirtualPrepareRGB:
 ld de, wMobileVirtualPreparedRGB
for hardware_color, 8
 ld a, [wMobilePrivatePalBindings + hardware_color]
 cp 8
 jr nc, .peer{d:hardware_color}
 add a
 add a
 add a
 add LOW(wMobileStableNativeRGB)
 ld l, a
 adc HIGH(wMobileStableNativeRGB)
 sub l
 ld h, a
 jr .copy{d:hardware_color}
.peer{d:hardware_color}
 sub 8
 add a
 add a
 add a
 add LOW(wMobileVirtualFrameColors)
 ld l, a
 adc HIGH(wMobileVirtualFrameColors)
 sub l
 ld h, a
.copy{d:hardware_color}
 rept 8
 ld a, [hli]
 ld [de], a
 inc e
 endr
endr
 ret

; Finite resource certificate, independent of map/role. Every descriptor must
; name a completed four-tuple physical span, including any invisible figure.
; Two equal-Y pairs separated by eight rows bound each figure to two objects
; per scanline. At most four native+visible peer figures therefore use <=8.
; Ownership is proved for every tuple, including hidden-X Y-active objects.
MobileVirtualCertifyCompact:
 ld a, [wMobileVirtualFastAdmission]
 cp 3
 jp nz, .fresh_source
 ; No source bytes, camera, native RGB or descriptors changed. D700's native
 ; prefix is never touched by a peer-only composition or physical DMA.
 ld a, [wMobileCompactNativeLength]
 swap a
 and $f
 ld b, a
 ld a, [wMobileVisibleCount]
 add b
 cp 5
 jp nc, .failed
 ld a, [wMobileCompactNativeLength]
 ld [wMobileVirtualLength], a
 ld a, [wMobileCompactNativePalette]
 ld [wMobileVirtualPaletteUsed], a
 for byte, 2
 ld a, [wMobileCompactNativeMask + byte]
 ld [wMobileStableEligible + byte], a
 ld [wMobileStablePendingPrivate + byte], a
 endr
 xor a
 ld [wMobileVirtualCursor], a
 ld hl, wMobileVirtualBindings
 rept 8
 ld [hli], a
 endr
 ret
.fresh_source
 ldh a, [rLCDC]
 bit 2, a ; physical LCDC OBJ size bit, independent of legacy bit numbering
 jp nz, .failed
 ld a, [wMobileStableUnknownCount]
 and a
 jp nz, .failed
 ld a, [wMobileVisibleCount]
 ld [wMobileVirtualRectangleCount], a
 xor a
 ld [wMobileVirtualIndex], a
 ld [wMobileVirtualLength], a
 ld [wMobileVirtualPaletteUsed], a
 ld [wMobileVirtualGroup + 4], a ; complete native span count
 ld hl, wMobileVirtualGroup
 ld a, $ff
 rept 4
 ld [hli], a
 endr
 xor a
 ld hl, wMobileVirtualBindings
 rept 8
 ld [hli], a
 endr
.descriptor
 ld a, [wMobileVirtualIndex]
 add a
 add a
 add a
 add LOW(wMobileVirtualDescriptors + 6)
 ld l, a
 ld h, HIGH(wMobileVirtualDescriptors)
 ld a, [hli]
 ld b, a
 and $1f
 jp z, .next
 cp 4
 jp nz, .failed
 bit 7, b
 jp z, .failed
 ld a, [hl]
 cp OAM_SIZE - 16 + 1
 jp nc, .failed ; includes the explicit $ff overflow descriptor
 and 3
 jp nz, .failed
 ld a, [hl]
 rrca
 rrca
 ld c, a
 ld e, a
 ld d, 0
 ld hl, wMobileStableOwners
 add hl, de
 ld a, [wMobileVirtualIndex]
 rept 4
 cp [hl]
 jp nz, .failed
 inc hl
 endr
 ld hl, wMobileStableOriginalYs
 add hl, de
 ld a, [hli]
 ld b, a
 cp [hl]
 jp nz, .failed
 inc hl
 ld a, [hli]
 cp [hl]
 jp nz, .failed
 sub b
 cp 8
 jr z, .rectangle
 cp -8
 jp nz, .failed
.rectangle
 ; Sort certified source offsets, independently of descriptor/actor order.
 ; Producer ownership can only be created for these exact completed spans;
 ; every later raw writer revokes source proof before touching any tuple.
 ld b, c
 ld hl, wMobileVirtualGroup
 rept 4
 ld a, [hl]
 cp b
 jr c, :+
 ld d, a
 ld a, b
 ld [hl], a
 ld b, d
:
 inc hl
 endr
 ld hl, wMobileVirtualGroup + 4
 inc [hl]
 ld hl, wMobileVirtualRectangleCount
 inc [hl]
 ld a, [hl]
 cp 5
 jp nc, .failed
 ld a, [wMobileVirtualIndex]
 ld hl, wMobileStableEligible
 call MobileCompactSetMaskBit
 ld hl, wMobileStablePendingPrivate
 call MobileCompactSetMaskBit
.next
 ld hl, wMobileVirtualIndex
 inc [hl]
 ld a, [hl]
 cp NUM_OBJECT_STRUCTS
 jp c, .descriptor
 ; Copy only the certified spans in ascending physical native priority.
 ; No second raw scan or descriptor reconstruction is needed.
 xor a
 ld [wMobileVirtualIndex], a
.span
 ld a, [wMobileVirtualGroup + 4]
 ld b, a
 ld a, [wMobileVirtualIndex]
 cp b
 jr nc, .complete
 ld e, a
 ld d, 0
 ld hl, wMobileVirtualGroup
 add hl, de
 ld c, [hl]
 ld b, 4
.quad
 call MobileCompactRetainedTuple
 ld a, [wMobileVirtualLength]
 ld e, a
 ld d, HIGH(wMobileVirtualOutput)
 rept 3
 ld a, [hli]
 ld [de], a
 inc e
 endr
 ld a, [hl]
 ld [de], a
 and OAM_PALETTE
 push bc
 ld e, a
 ld d, 0
 ld hl, MobileCompactPaletteBits
 add hl, de
 ld a, [wMobileVirtualPaletteUsed]
 or [hl]
 ld [wMobileVirtualPaletteUsed], a
 pop bc
 ld a, [wMobileVirtualLength]
 add 4
 ld [wMobileVirtualLength], a
 inc c
 dec b
 jr nz, .quad
 ld hl, wMobileVirtualIndex
 inc [hl]
 jr .span
.complete
 ld a, [wMobileVirtualLength]
 ld [wMobileCompactNativeLength], a
 ld a, [wMobileVirtualPaletteUsed]
 ld [wMobileCompactNativePalette], a
 for byte, 2
 ld a, [wMobileStableEligible + byte]
 ld [wMobileCompactNativeMask + byte], a
 endr
 xor a
 ld [wMobileVirtualCursor], a
 ret
.failed
 xor a
 ld [wMobileVirtualFastAdmission], a
 ld [wMobileVirtualLength], a
 ld [wMobileVirtualPaletteUsed], a
 ld [wMobileStableEligible], a
 ld [wMobileStableEligible + 1], a
 ld [wMobileStablePendingPrivate], a
 ld [wMobileStablePendingPrivate + 1], a
 ld [wMobileVirtualCursor], a
 ld hl, wMobileVirtualBindings
 rept 8
 ld [hli], a
 endr
 scf
 ret


MobileCompactSetMaskBit:
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

MobileCompactRetainedTuple:
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


MobileCompactPaletteBits: db 1,2,4,8,16,32,64,128

SECTION "Mobile Fused Cohort", ROMX, BANK[MOBILE_ROM_BANK + 5]
; Foreground owns the accepted roster while serial IRQs are masked. VBlank
; never writes peer records. Validate the entire resident set before stores.
MobileCompactEmitCohort:
 xor a
 ld hl, wMobileVirtualBindings
 rept 8
 ld [hli], a
 endr
 ld a, [wPalFadeDelayFrames]
 and a
 jp nz, .failed
for peer, 1, MOBILE_PEERS + 1
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 ld a, [wMobilePeer{d:peer}Visible]
 and a
 jp z, .checked{d:peer}
 ld a, [wMobilePeer{d:peer}NativeStage]
 cp 7
 jp nz, .failed
 ld a, [wMobilePeer{d:peer} + 18]
 ld b, a
 ld a, [wMobilePeer{d:peer}LoadedSprite]
 cp b
 jp nz, .failed
 ld a, [wMobileVirtualWindowOwners + peer - 1]
 cp peer
 jp nz, .failed
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobilePrivatePalDirty]
 and 1 << (peer - 1)
 jp nz, .failed
 ; Choose all colors before emitting the first figure. Native slots win.
 ld hl, wMobileVirtualBindings
 ld c, 0
 ld b, 1
.color{d:peer}
 ld a, [wMobileVirtualPaletteUsed]
 and b
 jr nz, .next_color{d:peer}
 ld [hl], peer
 ld a, [wMobileVirtualPaletteUsed]
 or b
 ld [wMobileVirtualPaletteUsed], a
 jr .checked{d:peer}
.next_color{d:peer}
 inc hl
 inc c
 sla b
 jp nc, .color{d:peer}
 jp .failed
.checked{d:peer}
endr
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ; Native exact RGB is the immutable base. Peer payloads overwrite only their
 ; final unused hardware slots, avoiding frame-color intermediates.
 ld hl, wMobileStableNativeRGB
 ld de, wMobileVirtualPreparedRGB
 rept 64
 ld a, [hli]
 ld [de], a
 inc e
 endr
for peer, 1, MOBILE_PEERS + 1
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 ld a, [wMobilePeer{d:peer}Visible]
 and a
 jp z, .emitted{d:peer}
 ld a, [wMobilePeer{d:peer}NativeFacing]
 ld b, a
 and $c
 cp $c
 jr z, .mirror{d:peer}
 cp 8
 jr nc, .unmirrored{d:peer}
 ld a, b
 and 3
 cp 3
 jr nz, .unmirrored{d:peer}
.mirror{d:peer}
 ld c, OAM_XFLIP
 ld b, $64 + (peer - 1) * 4 + 1
 jr .base{d:peer}
.unmirrored{d:peer}
 ld c, 0
 ld b, $64 + (peer - 1) * 4
.base{d:peer}
 ld a, [wMobilePeer{d:peer} + MOBILE_PEER_SCREEN_X]
 ld e, a
 ld a, [wMobilePeer{d:peer} + MOBILE_PEER_SCREEN_Y]
 ld d, a
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld hl, wMobileVirtualBindings
 ld a, 0
.palette{d:peer}
 push af
 ld a, [hli]
 cp peer
 jr z, .found{d:peer}
 pop af
 inc a
 jr .palette{d:peer}
.found{d:peer}
 pop af
 or c
 ld c, a
 and 7
 ld [wMobileAvatarPalette], a
 ld a, [wMobileVirtualLength]
 ld [wMobileOAMReadCursor], a
 add LOW(wMobileVirtualOutput)
 ld l, a
 adc HIGH(wMobileVirtualOutput)
 sub l
 ld h, a
 call .quads
 ld a, [wMobileVirtualLength]
 add 16
 ld [wMobileVirtualLength], a
 ld a, [wMobileAvatarPalette]
 add a
 add a
 add a
 add LOW(wMobileVirtualPreparedRGB)
 ld e, a
 ld d, HIGH(wMobileVirtualPreparedRGB)
 ld hl, wMobilePrivatePalColors + (peer - 1) * 8
 rept 8
 ld a, [hli]
 ld [de], a
 inc e
 endr
 ld hl, wMobileVirtualPeerMask
 set peer - 1, [hl]
 ld hl, wMobileStableEligible + 1
 set peer + 4, [hl]
 ld hl, wMobileStablePendingPrivate + 1
 set peer + 4, [hl]
 ld a, MOBILE_WRAM_BANK
 ldh [rWBK], a
 ld a, [wMobileAvatarPalette]
 ld [wMobilePeer{d:peer} + MOBILE_PEER_PALETTE], a
 ld a, [wMobileOAMReadCursor]
 ld [wMobilePeer{d:peer} + MOBILE_PEER_OAM_START + 1], a
 ld hl, wMobileRenderedCount
 inc [hl]
.emitted{d:peer}
endr
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 and a
 ret
.failed
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 scf
 ret
.quads
 mobile_virtual_write_peer_quads

; The complete batch has passed admission. Its first write retires the old
; page, before any constructor can modify a resource or candidate dependency.
SECTION "Mobile Publication Retirement", ROMX, BANK[MOBILE_ROM_BANK + 5]
MobileCompactRetirePublication:
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 xor a
 ld [wMobileStablePendingValid], a
 ld [wMobileVirtualPreviousOAM], a
 ld [wMobileCompactKeepPublication], a
 ret

SECTION "Mobile Whole Publication", ROMX, BANK[MOBILE_ROM_BANK + 5]
MobileVirtualPublish:
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
 ld a, [wMobileVirtualQueuedPeer]
 and a
 jr z, .pose_batch_ready
 farcall MobileVirtualPrevalidatePoseBatch
 jp c, .abort
.pose_batch_ready
 farcall MobileVirtualNativeRequestsDisjoint
 jp c, .abort
 farcall MobileVirtualCommitPose
 jp c, .abort
 ld a, 1
 ld [wMobileStableCapacityValid], a
 mobile_virtual_publish_page MobileVirtualPublish.abort
.abort
 xor a
 ld [wMobileStableCapacityValid], a
 ld [wMobileVirtualStage], a
 ret
