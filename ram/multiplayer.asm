; Multiplayer system RAM variables

SECTION "Multiplayer RAM", WRAM0

; Control variables
wMultiplayerIsMaster:: db          ; Flag: 1 if this cartridge is the master, 0 if slave

wMultiplayerLastSB:: db            ; Last SB value received. This only ever contains the last
                                   ; received byte, not the last sent one.

; Package buffers (8 bytes each)
wMultiplayerQueuedPackage:: ds 8    ; Package queued for transmission
wMultiplayerBufferedPackage:: ds 8  ; Package currently being transmitted
wMultiplayerReceivedPackage:: ds 8  ; Package currently being received
wMultiplayerPackageToExecute:: ds 8 ; Complete received package ready for processing

; Send state variables
wMultiplayerSendByteIdx:: db        ; Current byte being sent (0-7)
wMultiplayerSendNibbleIdx:: db      ; Current nibble being sent (0=high, 1=low)
wMultiplayerSendSeq:: db            ; Send sequence bit (0/1)

; Receive state variables
wMultiplayerReceiveByteIdx:: db     ; Current byte being received (0-7)
wMultiplayerReceiveNibbleIdx:: db   ; Current nibble being received (0=high, 1=low)
wMultiplayerRecvSeq:: db            ; Expected receive sequence bit (0/1)

; Communication state
wMultiplayerTimeoutCounter:: db     ; Timeout counter in frames
wMultiplayerQueuedPackageFlag:: db  ; 1 if queued package is available
wMultiplayerBufferedPackageFlag:: db ; 1 if buffered package is being transmitted

; Temporary buffer for creating packages
wTempBuffer:: ds 8
