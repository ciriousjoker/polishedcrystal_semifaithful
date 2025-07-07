; Multiplayer system RAM variables

SECTION "Multiplayer RAM", WRAM0

; Control variables
wMultiplayerIsMaster:: db          ; Flag: 1 if this cartridge is the master, 0 if slave

; Package buffers (8 bytes each)
wMultiplayerStart: ds 0             ; Marker for the start of multiplayer package buffers. Useful during initialization.
wMultiplayerIsEnabled:: db          ; Flag: 1 if multiplayer system is enabled and initialized, 0 if not.
wMultiplayerTempPackage:: ds 8      ; Temporary buffer for creating packages
wMultiplayerQueuedPackage:: ds 8    ; Package queued for transmission. This can be overwritten at any point to queue a different package.
wMultiplayerStaticNoopByte:: db     ; Constant $FF for noop byte. Sent when no package is being transmitted or to indicate that a transmission is restarted.
wMultiplayerBufferedPackage:: ds 8  ; Package currently being transmitted
wMultiplayerReceivedPackage:: ds 8  ; Package currently being received
wMultiplayerPackageToExecute:: ds 8 ; Complete received package ready for processing.

; Send state variables
wMultiplayerSendByteIdx:: db        ; Current byte being sent (0 for noop, 1-8 for the package bytes)
wMultiplayerSendNibbleIdx:: db      ; Current nibble being sent (1=high, 0=low)
wMultiplayerNextSeqToSend:: db      ; Next sequence bit to send (0/1). Is always a flipped version of the last sent Seq bit. On Ack failure, resets to 0.
wMultiplayerNextAckToSend:: db      ; Next ACK bit to send (0/1). Is always a flipped version of the last received Seq bit. On Ack failure, resets to 0.

; Receive state variables
wMultiplayerReceiveByteIdx:: db     ; Current byte being received (0-7)
wMultiplayerReceiveNibbleIdx:: db   ; Expected nibble idx to be received (1=high, 0=low). If the incoming nibble is not the expected one, we resync.
wMultiplayerLastReceivedByte:: db   ; Temporary storage for byte being assembled
  
; TODO: Seems unnecessary, remove?
; wMultiplayerLastReceivedSeq:: db    ; Last received sequence bit (0/1).

; Communication state
wMultiplayerHasQueuedPackage:: db   ; 1 if queued package is available
wMultiplayerHasBufferedPackage:: db ; 1 if buffered package is being transmitted

wMultiplayerEnd: ds 0               ; Marker for the end of the multiplayer RAM section. Useful during initialization.
