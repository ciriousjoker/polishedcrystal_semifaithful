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
wMobileResidentEnd::
ASSERT wMobileResidentEnd - wMobileResidentStart <= 96

SECTION "Mobile Buffers", WRAMX[$d4eb], BANK[MOBILE_WRAM_BANK]
wMobileBufferStart::
wMobileGuardStart:: db
wMobileTx:: ds MOBILE_BUFFER_SIZE
wMobileGuardTx:: db
wMobileRx:: ds MOBILE_BUFFER_SIZE
wMobileGuardRx:: db
wMobileApp:: ds MOBILE_FRAME_SIZE
wMobileGuardEnd:: db
wMobileBufferEnd::
ASSERT wMobileBufferEnd - wMobileBufferStart <= 1536
ASSERT wMobileBufferStart >= $d4eb && wMobileBufferEnd <= $e000
