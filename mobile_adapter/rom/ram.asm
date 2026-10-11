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
wMobileFacingsSuppressed:: db
wMobileColdFrame:: db
wMobileResidentEnd::
ASSERT wMobileResidentEnd - wMobileResidentStart <= 128

SECTION "Mobile Buffers", WRAMX[$d4eb], BANK[MOBILE_WRAM_BANK]
wMobileBufferStart::
wMobileGuardStart:: db
wMobileTx:: ds MOBILE_BUFFER_SIZE
wMobileGuardTx:: db
wMobileRx:: ds MOBILE_BUFFER_SIZE
wMobileGuardRx:: db
wMobileApp:: ds MOBILE_FRAME_SIZE
wMobileOutgoing:: ds MOBILE_FRAME_SIZE
wMobileResumeToken:: ds 8
wMobileLocalSnapshot:: ds MOBILE_SNAPSHOT_SIZE
wMobilePeer1::
wMobilePeer1Id:: ds 2
wMobilePeer1Generation:: ds 2
wMobilePeer1Revision:: ds 2
wMobilePeer1MapGroup:: db
wMobilePeer1MapNumber:: db
wMobilePeer1Instance:: ds 2
wMobilePeer1Epoch:: ds 2
wMobilePeer1X:: ds 2
wMobilePeer1Y:: ds 2
wMobilePeer1Facing:: db
wMobilePeer1Status:: db
wMobilePeer1Avatar:: db
wMobilePeer1Movement:: db
wMobilePeer1DrawX:: dw
wMobilePeer1DrawY:: dw
wMobilePeer1DrawValid:: db
wMobilePeer1ScreenX:: db
wMobilePeer1ScreenY:: db
wMobilePeer1Visible:: db
wMobilePeer1OAMStart:: db
ds 1 ; pending OAM offset
wMobilePeer1LoadedSprite:: db
wMobilePeer1LoadedFrame:: db
wMobilePeer1Palette:: db
ds MOBILE_PEER_SIZE - 33
wMobilePeer2::
wMobilePeer2Id:: ds 2
wMobilePeer2Generation:: ds 2
wMobilePeer2Revision:: ds 2
wMobilePeer2MapGroup:: db
wMobilePeer2MapNumber:: db
wMobilePeer2Instance:: ds 2
wMobilePeer2Epoch:: ds 2
wMobilePeer2X:: ds 2
wMobilePeer2Y:: ds 2
wMobilePeer2Facing:: db
wMobilePeer2Status:: db
wMobilePeer2Avatar:: db
wMobilePeer2Movement:: db
wMobilePeer2DrawX:: dw
wMobilePeer2DrawY:: dw
wMobilePeer2DrawValid:: db
wMobilePeer2ScreenX:: db
wMobilePeer2ScreenY:: db
wMobilePeer2Visible:: db
wMobilePeer2OAMStart:: db
ds 1 ; pending OAM offset
wMobilePeer2LoadedSprite:: db
wMobilePeer2LoadedFrame:: db
wMobilePeer2Palette:: db
ds MOBILE_PEER_SIZE - 33
wMobileOAMSnapshot:: ds OAM_SIZE
wMobileOAMOutput:: ds 2 * MOBILE_PEERS * 4 * OBJ_SIZE ; peer output plus overwritten prefix
wMobileNPCOptional:: ds OAM_COUNT
wMobileUploadedSnapshot:: ds MOBILE_SNAPSHOT_SIZE
wMobileGuardEnd:: db
wMobileBufferEnd::
ASSERT wMobileBufferEnd - wMobileBufferStart <= 1536
ASSERT wMobileBufferStart >= $d4eb && wMobileBufferEnd <= $e000

; Independent persistent graphics cache in the audited bank-2 tail.
SECTION "Mobile Avatar Cache", WRAMX[$dac0], BANK[MOBILE_WRAM_BANK]
wMobileAvatarCache1:: ds 24 tiles
wMobileAvatarCache2:: ds 24 tiles
wMobileAvatarCacheEnd::
ASSERT wMobileBufferEnd <= wMobileAvatarCache1
ASSERT wMobileAvatarCacheEnd <= $e000

; Optional map-rendering declarations are copied once when graphics reset.
SECTION "Mobile Render Mask", WRAMX[$ddc0], BANK[MOBILE_WRAM_BANK]
wMobileRenderMapMask:: ds 3
wMobileScanlineValid:: db
wMobileScanlineKey:: ds OAM_COUNT + 1
wMobileRenderMaskEnd::
ASSERT wMobileRenderMaskEnd <= $de00

; A page-aligned delta table avoids pointer arithmetic for each OAM quad.
SECTION "Mobile Scanline Delta", WRAMX[$de00], BANK[MOBILE_WRAM_BANK]
wMobileScanlineBudget:: ; weather and NPC sweeps run sequentially, never in IRQ
wMobileScanlineDelta:: ds SCREEN_HEIGHT_PX + 1
wMobileScanlineDeltaEnd::
ASSERT wMobileScanlineDeltaEnd <= $df00
