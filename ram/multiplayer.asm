; Multiplayer system RAM variables

SECTION "Multiplayer RAM", WRAM0

; Control variables
wMultiplayerIsMaster:: db          ; Flag: 1 if this cartridge is the master, 0 if slave

; Package buffers (8 bytes each)
wMultiplayerStart: ds 0             ; Marker for the start of multiplayer package buffers. Useful during initialization.
wMultiplayerIsEnabled:: db          ; Flag: 1 if multiplayer system is enabled and initialized, 0 if not.
wMultiplayerTempPackage:: ds 8      ; Temporary buffer for creating packages
wMultiplayerQueuedPackage:: ds 8    ; Package queued for transmission. This can be overwritten at any point to queue a different package.
wMultiplayerBufferedPackage:: ds 8  ; Package currently being transmitted
wMultiplayerReceivedPackage:: ds 8  ; Package currently being received
wMultiplayerPackageToExecute:: ds 8 ; Complete received package ready for processing.

; Send state variables
wMultiplayerSendByteIdx:: db        ; Current byte being sent (0 for noop, 1-8 for the package bytes)
wMultiplayerSendNibbleIdx:: db      ; Current nibble being sent (0=high, 1=low)

; Frame timing
wMultiplayerFrameCounter:: db       ; Frame counter for transmission timing (0-255, wraps around)

; Receive state variables
wMultiplayerReceiveByteIdx:: db     ; Current byte being received (0-7)
wMultiplayerReceiveNibbleIdx:: db   ; Expected nibble idx to be received (0=high, 1=low). If the incoming nibble is not the expected one, we resync.
wMultiplayerLastReceivedByte:: db   ; Temporary storage for byte being assembled

; Duplicate detection state
wMultiplayerLastReceivedRSB:: db    ; Last received rSB value for duplicate detection

; Communication state
wMultiplayerHasQueuedPackage:: db   ; 1 if queued package is available
wMultiplayerHasBufferedPackage:: db ; 1 if buffered package is being transmitted

wMultiplayerEnd: ds 0               ; Marker for the end of the multiplayer RAM section. Useful during initialization.
