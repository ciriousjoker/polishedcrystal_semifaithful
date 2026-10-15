SECTION "Mobile Resident", WRAM0[$cec4]
wMobileResidentStart::
wMobileOwner:: db
wMobileWire:: db
wMobileTransferActive:: db
wMobileBurst:: db
wMobileRemaining:: dw
wMobileTxPointer:: dw
wMobileRxPointer:: dw
wMobileHeaderLeft:: db
wMobileCommand:: db
wMobileTxSize:: db
wMobileStep:: db
wMobileDeadline:: db
wMobileCooldown:: db
wMobileKeyLatch:: db
wMobileError:: db
wMobileSuccessCount:: db
wMobileAttemptCount:: db
wMobileConnection:: db
wMobileAppCount:: db
wMobileSavedSB:: db
wMobileSavedSC:: db
wMobileSavedSerialIE:: db
wMobileSequence:: dw
wMobileAttemptTicks:: dw
wMobilePingSent:: db
wMobileRoundTrips:: db
wMobileIRQCount:: dw
wMobileAdapterError:: db
wMobileMode:: db ; 0: one-shot echo diagnostic; 1: persistent lobby
wMobileWanted:: db
wMobileOnline:: db
wMobilePhase:: db
wMobilePlayerId:: ds 2 ; wire byte order
wMobileGeneration:: ds 2 ; wire byte order
wMobilePeerCount:: db
wMobilePresenceCount:: dw
wMobileExpectedKind:: db
wMobileOutgoingLength:: db
wMobileServerError:: db
wMobileLocalValid:: db
wMobileMapEpoch:: dw
wMobileAvailability:: db
wMobileLocalStaging:: ds MOBILE_SNAPSHOT_SIZE
wMobileGraphicsReady:: db
wMobileVisibleCount:: db
wMobileRenderedCount:: db
wMobileDeferred:: db ; 1: OAM pressure; 2: large-sprite tiles; 3: unsafe scene
wMobileOAMDemand:: db
wMobileSavedFacings:: ds NUM_OBJECT_STRUCTS
wMobileCameraX:: dw
wMobileCameraY:: dw
wMobileAvatarPalette:: db
wMobileAvatarTile:: db
wMobileOAMReadCursor:: db
wMobileOAMWriteCursor:: db
wMobileNativeSlotLimit::
wMobileLocalOAMStart:: db
wMobileInstanceKey:: ds 2
wMobileClippedWeather:: db
wMobileMenuState:: db
wMobileLastFrame:: db
wMobileFrameTick:: db
wMobileSendCooldown:: db
wMobileKeepalive:: db
wMobileMenuDeferred:: db
wMobileAppPending:: db
wMobileRxConsumed:: db
wMobileBudgetPriority:: db
wMobileBudgetIndex:: db
wMobileBudgetOptional:: db ; compact cold cohort construction owns one lifetime
wMobileBudgetCursor:: db
wMobileIRQInstallOffset:: dw
wMobilePumpCount:: db
wMobileVirtualActorMode::
wMobileNativeActorMode::
wMobileFacingsSuppressed:: db
wMobileColdFrame:: db
wMobileEmergencyLatch:: db
wMobileEmergencyPending:: db
wMobileNativePrepareFrame:: db
wMobileNativeGraphicsFence:: db
wMobileResidentEnd::
ASSERT wMobileResidentEnd - wMobileResidentStart <= 128

; Last actually transferred page and its world-frame counter, including native
; and menu pages. Publication and a suppressed PushOAM never update this stamp.
SECTION "Mobile Actual OAM DMA", WRAM0[$cf39]
wMobileActualOAMPage:: db
wMobileActualOAMFrame:: db
wMobileActualOAMEnd::
ASSERT wMobileResidentEnd <= wMobileActualOAMPage
ASSERT wMobileActualOAMEnd <= $cf44

; Native palette scans need a normal object view without swapping live records.
; This unbanked view stays outside the original game's serialized WRAM span.
SECTION "Mobile Native Object Views", WRAM0[$cf44]
wMobileNativeOriginals::
	ds NUM_OBJECT_STRUCTS * 8
wMobilePrivatePalRequest:: db
wMobilePrivatePalSlot:: db
wMobilePrivatePalUsed:: db
wMobilePrivatePalBlocked:: db
wMobilePrivatePalCurrent:: db
wMobilePrivatePalSavedUpdate:: db
wMobileNativePaletteOwners:: ds NUM_OBJECT_STRUCTS
wMobileNativeOriginalsEnd::
wMobileAvatarBank:: db ; private bank decision, never inferred from mutable VBK
ASSERT @ <= $d000
ASSERT wMobileNativeOriginalsEnd <= $d000

SECTION "Mobile Buffers", WRAMX[$d4eb], BANK[MOBILE_WRAM_BANK]
wMobileBufferStart::
wMobileGuardStart:: db
wMobileTx:: ds MOBILE_BUFFER_SIZE
wMobileGuardTx:: db
wMobileRx:: ds MOBILE_BUFFER_SIZE
wMobileGuardRx:: db
wMobileDecodePhase:: db
wMobileApp:: ds MOBILE_FRAME_SIZE
wMobileOutgoing:: ds MOBILE_FRAME_SIZE
wMobileResumeToken:: ds 8
wMobileLocalSnapshot:: ds MOBILE_SNAPSHOT_SIZE
for peer, 1, MOBILE_PEERS + 1
wMobilePeer{d:peer}::
wMobilePeer{d:peer}Id:: ds 2
wMobilePeer{d:peer}Generation:: ds 2
wMobilePeer{d:peer}Revision:: ds 2
wMobilePeer{d:peer}MapGroup:: db
wMobilePeer{d:peer}MapNumber:: db
wMobilePeer{d:peer}Instance:: ds 2
wMobilePeer{d:peer}Epoch:: ds 2
wMobilePeer{d:peer}X:: ds 2
wMobilePeer{d:peer}Y:: ds 2
wMobilePeer{d:peer}Facing:: db
wMobilePeer{d:peer}Status:: db
wMobilePeer{d:peer}Avatar:: db
wMobilePeer{d:peer}Movement:: db
wMobilePeer{d:peer}DrawX:: dw
wMobilePeer{d:peer}DrawY:: dw
wMobilePeer{d:peer}DrawValid:: db
wMobilePeer{d:peer}ScreenX:: db
wMobilePeer{d:peer}ScreenY:: db
wMobilePeer{d:peer}Visible:: db
wMobilePeer{d:peer}OAMStart:: db
ds 1 ; pending OAM offset
wMobilePeer{d:peer}LoadedSprite:: db
wMobilePeer{d:peer}LoadedFrame:: db
wMobilePeer{d:peer}Palette:: db
wMobilePeer{d:peer}NativeSprite:: db
wMobilePeer{d:peer}NativeStage:: db
wMobilePeer{d:peer}NativeRole:: db
wMobilePeer{d:peer}LZState:: ds 16
wMobilePeer{d:peer}NativeFacing:: db
wMobilePeer{d:peer}BackupStage:: db
wMobilePeer{d:peer}PrivatePalValid:: db
wMobilePeer{d:peer}PendingLease:: db
wMobilePeer{d:peer}LoadedMovement:: db ; immutable GPU pose identity
ds MOBILE_PEER_SIZE - 57
endr
wMobileLZScratch::
wMobileLZBank:: db
wMobileLZInput:: dw
wMobileLZOutput:: dw
wMobileLZBase:: dw
wMobileLZLeft:: dw
wMobileLZCommand:: db
wMobileLZFirst:: db
wMobileLZSecond:: db
wMobileLZCopy:: dw
wMobileLZHalf:: db
wMobileLZPacked:: db
ASSERT @ - wMobileLZScratch == 16
wMobileLZBudget:: db
wMobileHashPointer:: dw
wMobileHashWidth:: db
wMobileHashColumns:: db
wMobileHashRows:: db
wMobileHashB:: db
wMobileHashC:: db
wMobileHashActive:: db
wMobileLeaseMask:: ds 3
wMobileLeaseBestSlot:: db
wMobileLeaseBestScore:: db
wMobileUploadedSnapshot:: ds MOBILE_SNAPSHOT_SIZE
wMobileChecksumCursor:: dw
wMobileChecksumLeft:: dw
wMobileChecksumSum:: dw
wMobileChecksumChunk:: db
wMobileGuardEnd:: db
wMobileBufferEnd::
ASSERT wMobileBufferEnd - wMobileBufferStart <= 1536
ASSERT wMobileBufferStart >= $d4eb && wMobileBufferEnd <= $e000

; Independent persistent graphics cache in the audited bank-2 tail.
SECTION "Mobile Avatar Cache", WRAMX[$db00], BANK[MOBILE_WRAM_BANK]
for peer, 1, MOBILE_PEERS + 1
wMobileAvatarCache{d:peer}:: ds 24 tiles
endr
wMobileAvatarCacheEnd::
ASSERT wMobileBufferEnd <= MobileIRQRuntimeStart
ASSERT MobileIRQRuntimeEnd <= wMobileAvatarCache1
ASSERT wMobileAvatarCacheEnd <= $e000

SECTION "Mobile Actor Backups", WRAMX[$d562], BANK[MOBILE_BACKUP_BANK]
for peer, 1, MOBILE_PEERS + 1
wMobileActorBackup{d:peer}:: ds OBJECT_LENGTH + 4 ; camera and BG anchor
endr
wMobileActorBackupEnd::
DEF wMobileVirtualFrameColors EQU wMobileActorBackup1 + 16
EXPORT wMobileVirtualFrameColors
ASSERT wMobileVirtualFrameColors + MOBILE_PEERS palettes <= wMobileActorBackupEnd
; GDMA ignores the source address's low four bits. Keep every private raw tile
; snapshot aligned, independently of the 38-byte native object records.
ds (16 - (LOW(@) & $f)) & $f
for peer, 1, MOBILE_PEERS + 1
wMobileActorTiles{d:peer}:: ds 24 tiles
endr
wMobileActorTilesEnd::
; Temporary ordinary buffers are restored before leaving a palette job.
wMobilePrivatePalTemporary:: ds 2 palettes
wMobilePrivatePalColors:: ds MOBILE_PEERS palettes
wMobilePrivatePalIdentities:: ds MOBILE_PEERS
wMobilePrivatePalBindings:: ds 8
wMobilePrivatePalDirty:: db
wMobilePrivatePalWasFading:: db
wMobilePrivatePalEnd::
ASSERT LOW(wMobileActorTiles1) & $f == 0
ASSERT wMobileActorBackupEnd <= $dd00
ASSERT wMobileActorTilesEnd <= $dd00
ASSERT wMobilePrivatePalEnd <= $dd00

SECTION "Mobile Virtual Scratch", WRAMX[$dab0], BANK[MOBILE_BACKUP_BANK]
wMobileVirtualScratch:: ds 224
wMobileVirtualScratchEnd::
ASSERT wMobilePrivatePalEnd <= wMobileVirtualScratch
ASSERT wMobileVirtualScratchEnd <= $dd00

; 13 immutable foreground descriptors, each x/y/tile/flags/source/count/relative.
; The virtual renderer never leases ordinary objects. Reuse the old lease-index
; storage exclusively for a private, bounded pending graphics transaction.
DEF wMobileVirtualQueuedPeer EQU wMobileSavedFacings
DEF wMobileVirtualQueuedWindow EQU wMobileSavedFacings + 1
DEF wMobileVirtualQueuedSource EQU wMobileSavedFacings + 2
DEF wMobileVirtualQueuedFrame EQU wMobileSavedFacings + 4
DEF wMobileVirtualQueuedFacing EQU wMobileSavedFacings + 5
DEF wMobileVirtualQueuedSprite EQU wMobileSavedFacings + 6
DEF wMobileVirtualQueuedAvatar EQU wMobileSavedFacings + 7
DEF wMobileVirtualQueuedMovement EQU wMobileSavedFacings + 8
DEF wMobileVirtualBackgroundFrame EQU wMobileSavedFacings + 9
ASSERT 10 <= NUM_OBJECT_STRUCTS
EXPORT wMobileVirtualQueuedPeer, wMobileVirtualQueuedWindow, wMobileVirtualQueuedSource
EXPORT wMobileVirtualQueuedFrame, wMobileVirtualQueuedFacing, wMobileVirtualQueuedSprite
EXPORT wMobileVirtualQueuedAvatar
EXPORT wMobileVirtualQueuedMovement
EXPORT wMobileVirtualBackgroundFrame
ASSERT MOBILE_PEERS == 3
ASSERT FIRST_VRAM1_OBJECT_STRUCT * 12 + 3 <= $64
ASSERT (NUM_OBJECT_STRUCTS - FIRST_VRAM1_OBJECT_STRUCT) * 12 + 3 <= $3f
DEF wMobileVirtualDescriptors EQU wMobileNativeOriginals
ASSERT NUM_OBJECT_STRUCTS * 8 <= wMobileNativeOriginalsEnd - wMobileNativeOriginals
DEF wMobileVirtualWindowOwners EQU wMobileNativePaletteOwners
DEF wMobileVirtualIndex EQU wMobileNativePaletteOwners + 3
DEF wMobileVirtualStage EQU wMobileNativePaletteOwners + 4
DEF wMobileVirtualLength EQU wMobileNativePaletteOwners + 5
DEF wMobileVirtualPhase EQU wMobileNativePaletteOwners + 6
DEF wMobileVirtualCursor EQU wMobileNativePaletteOwners + 7
DEF wMobileVirtualPaletteUsed EQU wMobileNativePaletteOwners + 8
DEF wMobileVirtualThirdBlocked EQU wMobileNativePaletteOwners + 9
DEF wMobileVirtualWindow EQU wMobileNativePaletteOwners + 10
DEF wMobileVirtualCacheHit EQU wMobileNativePaletteOwners + 11
DEF wMobileVirtualFrame EQU wMobileNativePaletteOwners + 12
DEF wMobileVirtualNativeRaw EQU wMobileActorTiles1
EXPORT wMobileVirtualNativeRaw
DEF wMobileVirtualOutput EQU (wMobileActorTiles1 + NUM_OBJECT_STRUCTS * 4 + $ff) & $ff00
DEF wMobileVirtualLines EQU wMobileVirtualOutput + OAM_SIZE
DEF wMobileVirtualGroup EQU wMobileVirtualScratch
DEF wMobileVirtualBindings EQU wMobileVirtualGroup + 64
DEF wMobileVirtualTemplate EQU wMobileVirtualBindings + 8
; Template readers and immutable descriptor copies use exactly bytes 0..7.
; This bank-5 payload is disjoint from the bank-2 avatar cache alias.
DEF wMobileVirtualPreparedRGB EQU wMobileVirtualTemplate + 8
EXPORT wMobileVirtualPreparedRGB
ASSERT LOW(wMobileVirtualPreparedRGB) == 0
ASSERT wMobileVirtualPreparedRGB + 64 <= wMobileVirtualTemplate + 80
; Source-bound native prefix cache. Peer-only revocation preserves this proof;
; every new source capture clears FastAdmission before any reuse can occur.
DEF wMobileCompactNativeLength EQU wMobileVirtualTemplate + 72
DEF wMobileCompactNativeMask EQU wMobileVirtualTemplate + 73
DEF wMobileCompactNativePalette EQU wMobileVirtualTemplate + 75
; Scoped roster-store barrier: preserve only an already sealed publication.
DEF wMobileCompactKeepPublication EQU wMobileVirtualTemplate + 76
ASSERT wMobileCompactKeepPublication == wMobileCompactNativePalette + 1
ASSERT wMobileCompactKeepPublication < wMobileVirtualTemplate + 80
EXPORT wMobileCompactKeepPublication
EXPORT wMobileCompactNativeLength, wMobileCompactNativeMask, wMobileCompactNativePalette
ASSERT wMobileVirtualPreparedRGB + 64 == wMobileCompactNativeLength
ASSERT wMobileCompactNativePalette < wMobileVirtualTemplate + 80
DEF wMobileVirtualGroupLength EQU wMobileVirtualTemplate + 80
DEF wMobileVirtualRejected EQU wMobileVirtualGroupLength + 1
DEF wMobileVirtualGroupPalette EQU wMobileVirtualRejected + 1
DEF wMobileVirtualMergedPalette EQU wMobileVirtualGroupPalette + 1
EXPORT wMobileVirtualMergedPalette
ASSERT wMobileVirtualNativeRaw + NUM_OBJECT_STRUCTS * 4 <= wMobileVirtualOutput
ASSERT LOW(wMobileVirtualOutput) == 0
ASSERT wMobileVirtualOutput + OAM_SIZE == wMobileVirtualLines
ASSERT wMobileVirtualLines + SCREEN_HEIGHT_PX <= wMobileActorTilesEnd
ASSERT wMobileVirtualMergedPalette + 8 palettes <= wMobileVirtualScratchEnd

EXPORT wMobileVirtualDescriptors
EXPORT wMobileVirtualWindowOwners
EXPORT wMobileVirtualIndex
EXPORT wMobileVirtualStage
EXPORT wMobileVirtualLength
EXPORT wMobileVirtualPhase
EXPORT wMobileVirtualCursor
EXPORT wMobileVirtualPaletteUsed
EXPORT wMobileVirtualThirdBlocked
DEF wMobileVirtualNativeAvatarColors EQU wMobileVirtualThirdBlocked
EXPORT wMobileVirtualNativeAvatarColors
EXPORT wMobileVirtualWindow
EXPORT wMobileVirtualCacheHit
EXPORT wMobileVirtualFrame
EXPORT wMobileVirtualOutput
EXPORT wMobileVirtualLines
EXPORT wMobileVirtualGroup
EXPORT wMobileVirtualBindings
EXPORT wMobileVirtualTemplate
EXPORT wMobileVirtualGroupLength
EXPORT wMobileVirtualRejected
EXPORT wMobileVirtualGroupPalette

SECTION "Mobile Virtual Admission", WRAM0[$cfc0]
wMobileVirtualFastAdmission:: db
wMobileVirtualAttempts:: db
wMobileVirtualPreviousOAM:: db ; one-shot completed private OAM DMA token
wMobileVirtualGroupPointer:: ds 2
wMobileVirtualNativeRotation:: db
wMobileVirtualPublished:: db
wMobileVirtualHeaderOAM:: db
wMobileVirtualUploadFrame:: db
wMobileVirtualMirror:: db
wMobileVirtualRectangleCount:: db
wMobileVirtualRectangleTop:: db
wMobileVirtualRemoteLimit:: db
wMobileVirtualGroupOwner:: db
wMobileIRQTrampoline:: ds MOBILE_IRQ_TRAMPOLINE_SIZE
ds OAM_COUNT / 2 - MOBILE_IRQ_TRAMPOLINE_SIZE
wMobileVirtualMapGroup:: db
wMobileVirtualMapNumber:: db
wMobileVirtualHasPrivateColors:: db
wMobileVirtualPlannedPrivateColors:: db
wMobileVirtualSerialSlice:: db
wMobileNativeCaptureActive:: db
wMobileVirtualDroppedSlot:: db
wMobileVirtualForceFull:: db
wMobileVirtualCoarseTrial:: db
wMobileVirtualDroppedSlot2:: db
DEF wMobileVirtualRectangleReturnBank EQU wMobileVirtualDroppedSlot2
EXPORT wMobileVirtualRectangleReturnBank
wMobileVirtualDropRemaining:: db
wMobileVirtualNativeColors:: ds NUM_PLAYER_GENDERS / 2
wMobileVirtualNeedColorAudit:: db
ASSERT NUM_PLAYER_GENDERS == 4
ASSERT @ <= $cff0

; Capture and composition are mutually exclusive phases of one world frame.
DEF wMobileVirtualCaptureClass EQU wMobileVirtualMirror
EXPORT wMobileVirtualCaptureClass

SECTION "Mobile Virtual Order", WRAMX[$dc00], BANK[MOBILE_BACKUP_BANK]
wMobileVirtualGroundRotation:: db
wMobileVirtualNativeNext:: db
wMobileVirtualGroundNext:: db
wMobileVirtualPeerMask:: db
wMobileVirtualPaletteRotation:: db
wMobileVirtualClass:: db
wMobileVirtualOrderCount:: db
wMobileVirtualNPCCount:: db
wMobileVirtualOrderValid:: db
wMobileVirtualSelectedOrdinal:: db
wMobileVirtualSelectedClass:: db
wMobileVirtualMembership:: ds NUM_OBJECT_STRUCTS - 1
wMobileVirtualOrder:: ds NUM_OBJECT_STRUCTS - 1
wMobileVirtualOrderBase:: ds NUM_OBJECT_STRUCTS - 1
wMobileVirtualOrderNativeRotation:: db
wMobileVirtualOrderGroundRotation:: db
ASSERT @ <= $dd00

; Retained normal source. No executable admission cache may write this span.
DEF wMobileStableOriginalYs EQU $d840
EXPORT wMobileStableOriginalYs
DEF wMobileStableOwners EQU $d868
EXPORT wMobileStableOwners
DEF wMobileStablePhases EQU $d890
EXPORT wMobileStablePhases
DEF wMobileStableBindingValid EQU $d8a0
EXPORT wMobileStableBindingValid
DEF wMobileStableEligible EQU $d8b0
EXPORT wMobileStableEligible
DEF wMobileStableContinuous EQU $d8b2
EXPORT wMobileStableContinuous
DEF wMobileStablePhaseMasks EQU $d8b4
EXPORT wMobileStablePhaseMasks
DEF wMobileStableLastActual EQU $d8b8
EXPORT wMobileStableLastActual
DEF wMobileStableLastEligible EQU $d8ba
EXPORT wMobileStableLastEligible
DEF wMobileStableDue EQU $d8bc
EXPORT wMobileStableDue
DEF wMobileStableNativeComplete EQU $d8be
EXPORT wMobileStableNativeComplete
DEF wMobileStablePendingNative EQU $d8c0
EXPORT wMobileStablePendingNative
DEF wMobileStablePendingPrivate EQU $d8c2
EXPORT wMobileStablePendingPrivate
DEF wMobileStableRetired EQU $d8c4
EXPORT wMobileStableRetired
DEF wMobileStableOutputOffsets EQU $d8c6
EXPORT wMobileStableOutputOffsets
DEF wMobileStablePhasePaletteBindings EQU $d8d6
EXPORT wMobileStablePhasePaletteBindings
DEF wMobileStableSourceValid EQU $d8e6
EXPORT wMobileStableSourceValid
DEF wMobileStableActorViewValid EQU $d8e7
EXPORT wMobileStableActorViewValid
DEF wMobileStableCapacityValid EQU $d8e8
EXPORT wMobileStableCapacityValid
DEF wMobileStablePendingValid EQU $d8e9
EXPORT wMobileStablePendingValid
DEF wMobileStableFinalDecision EQU $d8ea
EXPORT wMobileStableFinalDecision
DEF wMobileStableHardwarePaletteProvenance EQU $d8eb
EXPORT wMobileStableHardwarePaletteProvenance
DEF wMobileStableOutputPhase EQU $d8ec
EXPORT wMobileStableOutputPhase
DEF wMobileStableNativeCandidateValid EQU $d8ed
EXPORT wMobileStableNativeCandidateValid
DEF wMobileStableProducerActive EQU $d8ee
EXPORT wMobileStableProducerActive
DEF wMobileStableCurrentOwner EQU $d8ef
EXPORT wMobileStableCurrentOwner
DEF wMobileStableTransactionScratch EQU $d8f0
EXPORT wMobileStableTransactionScratch
DEF wMobileStableRemoteBindings EQU $d8fe
EXPORT wMobileStableRemoteBindings
DEF wMobileStableScratch EQU $d90a
EXPORT wMobileStableScratch
DEF wMobileStableRawXTileFlags EQU $d92a
EXPORT wMobileStableRawXTileFlags
DEF wMobileStableNativeRGB EQU wMobileVirtualMergedPalette
EXPORT wMobileStableNativeRGB
ASSERT wMobileStableNativeRGB == $db4b
ASSERT wMobileStableNativeRGB + 64 <= wMobileVirtualScratchEnd
; Small transient consumer state, outside the retained source.
DEF wMobileVirtualPeerGatherValid EQU $d621
EXPORT wMobileVirtualPeerGatherValid
EXPORT wMobileNativeCaptureActive


ASSERT wMobileVirtualGroup + 64 == wMobileVirtualBindings
ASSERT wMobileVirtualTemplate + 80 == wMobileVirtualGroupLength
ASSERT wMobileVirtualGroupPalette + 1 == wMobileStableNativeRGB
DEF wMobileStableCameraX EQU wMobileStableTransactionScratch
DEF wMobileStableCameraY EQU wMobileStableTransactionScratch + 2
EXPORT wMobileStableCameraX, wMobileStableCameraY
ASSERT wMobileStableCameraY + 2 <= wMobileStableRemoteBindings
; Producer certificate: number of visible raw quads with no actor owner.
; The camera occupies the preceding four bytes of this transient span.
DEF wMobileStableUnknownCount EQU wMobileStableTransactionScratch + 4
EXPORT wMobileStableUnknownCount
ASSERT wMobileStableUnknownCount + 1 <= wMobileStableRemoteBindings

; Three bounded immutable pose jobs. Existing global queue clears revoke the
; entire batch on map replacement or native graphics takeover.
DEF wMobileStablePoseBatch EQU wMobileStableScratch
DEF wMobileStablePoseGeneration EQU wMobileStableScratch + 24
DEF wMobileStablePoseMask EQU wMobileStableTransactionScratch + 5
DEF wMobileStablePoseToken EQU wMobileStableTransactionScratch + 6
DEF wMobileVirtualPoseBatchMask EQU wMobileSavedFacings + 10
DEF wMobileVirtualPoseBatchIndex EQU wMobileSavedFacings + 11
EXPORT wMobileStablePoseBatch, wMobileStablePoseGeneration
EXPORT wMobileStablePoseMask, wMobileStablePoseToken
EXPORT wMobileVirtualPoseBatchMask, wMobileVirtualPoseBatchIndex
ASSERT wMobileStablePoseGeneration + 1 <= wMobileStableRawXTileFlags
ASSERT wMobileVirtualPoseBatchIndex < wMobileSavedFacings + NUM_OBJECT_STRUCTS

; One foreground preparation is active at a time. An interrupt-side revoke is
; sticky until that preparation exits, independently of the generation byte.
DEF wMobileStablePosePreparing EQU wMobileStableScratch + 25
DEF wMobileStablePoseCanceled EQU wMobileStableScratch + 26
EXPORT wMobileStablePosePreparing, wMobileStablePoseCanceled
ASSERT wMobileStablePoseCanceled + 1 <= wMobileStableRawXTileFlags
