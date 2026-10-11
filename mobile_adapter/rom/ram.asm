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
wMobileBudgetOptional:: db
wMobileBudgetCursor:: db
wMobileIRQTrampoline:: ds MOBILE_IRQ_TRAMPOLINE_SIZE
wMobilePumpCount:: db
wMobileNativeActorMode::
wMobileFacingsSuppressed:: db
wMobileColdFrame:: db
wMobileEmergencyLatch:: db
wMobileEmergencyPending:: db
wMobileNativePrepareFrame:: db
wMobileResidentEnd::
ASSERT wMobileResidentEnd - wMobileResidentStart <= 128

; Native palette scans need a normal object view without swapping live records.
; This unbanked view stays outside the original game's serialized WRAM span.
SECTION "Mobile Native Object Views", WRAM0[$cf44]
wMobileNativeOriginals:: ds MOBILE_PEERS * OBJECT_LENGTH
wMobilePrivatePalRequest:: db
wMobilePrivatePalSlot:: db
wMobilePrivatePalUsed:: db
wMobilePrivatePalBlocked:: db
wMobilePrivatePalCurrent:: db
wMobilePrivatePalSavedUpdate:: db
wMobileNativePaletteOwners:: ds NUM_OBJECT_STRUCTS
wMobileNativeOriginalsEnd::
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
ds MOBILE_PEER_SIZE - 56
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
SECTION "Mobile Avatar Cache", WRAMX[$da40], BANK[MOBILE_WRAM_BANK]
for peer, 1, MOBILE_PEERS + 1
wMobileAvatarCache{d:peer}:: ds 24 tiles
endr
wMobileAvatarCacheEnd::
ASSERT wMobileBufferEnd <= wMobileAvatarCache1
ASSERT wMobileAvatarCacheEnd <= $e000

SECTION "Mobile Actor Backups", WRAMX[$d562], BANK[MOBILE_BACKUP_BANK]
for peer, 1, MOBILE_PEERS + 1
wMobileActorBackup{d:peer}:: ds OBJECT_LENGTH + 4 ; camera and BG anchor
endr
wMobileActorBackupEnd::
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
