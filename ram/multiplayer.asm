; Multiplayer system RAM variables

SECTION "Multiplayer RAM", WRAM0

; Control variables
wMultiplayerIsMaster:: db          ; Flag: 1 if this cartridge is the master, 0 if slave
wMultiplayerIsForcedSlave:: db     ; Flag: 1 if the other player is master and we can no longer become master

; Package buffers (8 bytes each)
wMultiplayerStart:                  ; Marker for the start of multiplayer package buffers. Useful during initialization.
wMultiplayerIsEnabled:: db          ; Flag: 1 if multiplayer system is enabled and initialized, 0 if not.
wMultiplayerTempPackage:: ds MULTIPLAYER_MAX_PACKAGE_SIZE      ; Temporary buffer for creating packages
wMultiplayerQueuedPackage:: ds MULTIPLAYER_MAX_PACKAGE_SIZE    ; Package queued for transmission. This can be overwritten at any point to queue a different package.
wMultiplayerBufferedPackage:: ds MULTIPLAYER_MAX_PACKAGE_SIZE  ; Package currently being transmitted
wMultiplayerReceivedPackage:: ds MULTIPLAYER_MAX_PACKAGE_SIZE  ; Package currently being received
wMultiplayerPackageToExecute:: ds MULTIPLAYER_MAX_PACKAGE_SIZE ; Complete received package ready for processing.
wMultiplayerBufferedPackageSize:: db ; Actual size of the package being transmitted
wMultiplayerReceivedPackageSize:: db ; Actual size of the package being received

; Send state variables
wMultiplayerSendByteIdx:: db        ; Current byte being sent (0 for noop, 1-8 for the package bytes)
wMultiplayerSendNibbleIdx:: db      ; Current nibble being sent (0=high, 1=low)

; Frame timing
wMultiplayerFrameCounter:: db       ; Frame counter for transmission timing (0-255, wraps around)

; Receive state variables
wMultiplayerReceiveByteIdx:: db     ; Current byte being received (0-7)
wMultiplayerReceiveNibbleIdx:: db   ; Expected nibble idx to be received (0=high, 1=low). If the incoming nibble is not the expected one, we resync.
wMultiplayerLastReceivedByte:: db   ; Temporary storage for byte being assembled

; Communication state
wMultiplayerHasQueuedPackage:: db   ; 1 if queued package is available
wMultiplayerHasQueuedCriticalPackage:: db ; 1 if the queued package is critical
wMultiplayerHasBufferedPackage:: db ; 1 if buffered package is being transmitted
wMultiplayerHasPackageToExecute:: db ; 1 if a package has been received and is waiting to be executed
wMultiplayerHasPackageToExecuteLazily:: db ; 1 if a package has been received and is waiting to be executed lazily
wMultiplayerPackageToExecuteLazily:: ds MULTIPLAYER_MAX_PACKAGE_SIZE
  
wMultiplayerHasOtherTrainerName:: db ; 1 if the other player's name has been received
wMultiplayerOtherTrainerName:: ds PLAYER_NAME_LENGTH
wMultiplayerOtherPlayerState:: ds 8 ; Player state (x, y, direction, state, map_group, map_number, unused, unused)

wMultiplayerEnd:                    ; Marker for the end of the multiplayer RAM section. Useful during initialization.
