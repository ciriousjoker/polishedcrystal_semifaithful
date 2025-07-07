; Frame-based multiplayer communication system
; Full-duplex, piggy-back-ACK link protocol
; Each frame processes one nibble with sequence/ACK bits

SECTION "Frame Multiplayer", ROMX

; SB Register bit layout (7 6 5 4 3 2 1 0):
; Bit 7: SeqOut - Toggle bit for sequence control. Toggled each frame if AckIn matched SeqOut.
; Bit 6: AckOut - Flipped version of the last received SeqIn (ie we predict the happy path and wait for sync if it fails).
; Bit 5: H/L - 1=High nibble, 0=Low nibble
; Bit 4: Master/Slave - 1 if master, 0 if slave (used to avoid interpreting own nibbles as received ones)
; Bits 3-0: Payload nibble

DEF MULTIPLAYER_PACKAGE_SIZE EQU 8

; MultiplayerInitialize:
; Purpose: Initializes all multiplayer-related RAM variables and hardware registers to a clean state.
;          This should be called once when activating the multiplayer feature.
MultiplayerInitialize::
	; Clear all package buffers to 0.
	ld hl, wMultiplayerStart
	ld bc, wMultiplayerEnd - wMultiplayerStart ; Zero out all multiplayer WRAM variables up to the temp buffer
	xor a
	rst ByteFill

	; Initialize the static noop byte constant
	ld a, $FF
	ld [wMultiplayerStaticNoopByte], a

	; Reset serial registers to a known, safe state.
	ld a, $FF
	ldh [rSB], a ; $FF is a safe value indicating no connection.
	xor a
	ldh [rSC], a ; Clear serial control register.

  ld a, 1
  ld [wMultiplayerIsEnabled], a
	ret

; Queue a package for transmission
; Input: HL = source address of 8-byte package
MultiplayerQueuePackage::
	push hl
	push bc
	push de
	
	; Copy package to queued buffer
	ld de, wMultiplayerQueuedPackage
	ld bc, MULTIPLAYER_PACKAGE_SIZE
	rst CopyBytes
	
	; Set queued flag
	ld a, 1
	ld [wMultiplayerHasQueuedPackage], a
	
	; Debug output
	call OpenText
	ld hl, .QueuedPackageText
	call PrintText
	call CloseText

	call MultiplayerCopyQueuedPackageToSendBufferIfPossible
	
	pop de
	pop bc
	pop hl
	ret

.QueuedPackageText:
	text "Queued package!"
	done

; Send package (called once per frame)
; Moves queued package to buffered package if ready
MultiplayerCopyQueuedPackageToSendBufferIfPossible::
	; Check if we have a queued package
	ld a, [wMultiplayerHasQueuedPackage]
	and a
	ret z
	
	; Check if we're still waiting for ACK on buffered package
	ld a, [wMultiplayerHasBufferedPackage]
	and a
	ret nz
	
	; Copy queued package to buffered package
	ld hl, wMultiplayerQueuedPackage
	ld de, wMultiplayerBufferedPackage
	ld bc, MULTIPLAYER_PACKAGE_SIZE
	rst CopyBytes
	
	; Set buffered flag, clear queued flag
	ld a, 1
	ld [wMultiplayerHasBufferedPackage], a
	xor a
	ld [wMultiplayerHasQueuedPackage], a
	
	; Reset send indices
	xor a
	ld [wMultiplayerSendByteIdx], a
	ld [wMultiplayerSendNibbleIdx], a
	
	ret

; Main frame communication function
; Handles sending and receiving nibbles with ACK protocol
MultiplayerSendReceiveNibble::
	; First, check if the serial hardware is busy. If a transfer is still in
	; progress (rSC bit 7 is 1), we must wait and do nothing this frame.
	; This is the gatekeeper for the entire send/receive process.
	ldh a, [rSC]
	bit 7, a
	ret nz  ; Return if still shifting.

	push hl
	push bc
	push de
	
	; Read received byte from serial buffer and store in E for reuse
	ldh a, [rSB]
	ld e, a
	
	; Check if SB is floating ($FF indicates no connection)
	cp $FF
	jp z, .restart_package
	
	; Check if we received our own nibble (should not happen in normal operation)
	call IfSBContainsOwnNibble
	jp z, .start_transmission

	; Check if received ACK bit is invalid
	call IfReceivedInvalidAck
	jp nz, .restart_package

	; Check if H/L bit mismatches expected nibble type
	call IfHLBitMismatchesExpectedNibble
	jp nz, .restart_package

  ; ;; Logic to update the seq/ack variables
  ; flip wMultiplayerNextSeqToSend
  ; set wMultiplayerNextAckToSend to just received a flipped SeqIn
  ;
  ; ;; Logic to handle the received nibble:
  ; call MultiplayerOnNibbleReceived
  ;   store received nibble in wMultiplayerLastReceivedByte ; offset needs to be determined based on rSB's H/L bit
  ;   increment wMultiplayerReceiveNibbleIdx
  ;   if wMultiplayerReceiveNibbleIdx == 2:
  ;     reset wMultiplayerReceiveNibbleIdx     
  ;     increment wMultiplayerReceiveByteIdx

  ;     if wMultiplayerLastReceivedByte == 0xFF:
  ;       ; received a full noop byte, so we need to treat everything afterwards
  ;       ; as a new package. This usually means either a desync or that the last package was fully sent
  ;       ; and the other side doesn't have a useful package to send yet.
  ;       set wMultiplayerReceiveByteIdx to 0
  ;
  ;     store wMultiplayerLastReceivedByte in wMultiplayerReceivePackage at wMultiplayerReceiveByteIdx
  ;     if wMultiplayerReceiveByteIdx == MULTIPLAYER_PACKAGE_SIZE:
  ;       store wMultiplayerReceivePackage in wMultiplayerPackageToExecute at wMultiplayerReceiveByteIdx
  ;       call MultiplayerOnPackageReceived
  ;         reset wMultiplayerReceiveByteIdx
  ;         reset wMultiplayerReceiveNibbleIdx
  ;         reset wMultiplayerReceivePackag

.handle_sent_nibble:
  ; ;; Logic to handle the sent nibble
  ; call MultiplayerOnNibbleSent:
  ;   increment wMultiplayerSendNibbleIdx
  ;   if wMultiplayerSendNibbleIdx == 2:
  ;   reset wMultiplayerSendNibbleIdx
  ;
  ;   ; NOTE: now the previous byte is fully sent (even if it was a high-noop + low-noop)
  ;   if wMultiplayerHasBufferedPackage:
  ;     increment wMultiplayerSendByteIdx ; if previous byte was a noop, this will now be 1
  ;     if wMultiplayerSendByteIdx == MULTIPLAYER_PACKAGE_SIZE + 1: ; +1 to account for the initial noop that signals the start of a package
  ;       reset wMultiplayerSendByteIdx
  ;       reset wMultiplayerHasBufferedPackage
  ;       call MultiplayerOnCompletePackageSent
  ; jump .send_nibble

.restart_package:
  ; reset send byte counter
  ; NOTE: Use compiler constants for the initial values
  ; reset wMultiplayerReceiveNibbleIdx to initial value
  ; reset wMultiplayerReceiveByteIdx to initial value
  ; reset wMultiplayerNextSeqToSend to initial value
  ; reset wMultiplayerNextAckToSend to initial value
  ; reset wMultiplayerSendNibbleIdx to initial value
  ; reset wMultiplayerSendByteIdx to initial value

.send_nibble:
  ; ;; Logic to send the next nibble:
  ; call PrepareNextNibble
  ;   BuildByteToSend:
  ;     bit 7: use wMultiplayerNextSeqToSend
  ;     bit 6: use wMultiplayerNextAckToSend
  ;     bit 5: use wMultiplayerSendNibbleIdx (1 for high nibble, 0 for low nibble)
  ;     bit 4: use wMultiplayerIsMaster (1 for master, 0 for slave)
  ;     bits 3-0: load data from [wMultiplayerStaticNoopNibble, ...wMultiplayerBufferedPackage] at wMultiplayerSendByteIdx at wMultiplayerSendNibbleIdx
  ;     ; NOTE: [wMultiplayerStaticNoopNibble] is the a constant that is used to treat wMultiplayerSendByteIdx == 0 as a noop byte (0b11111111).
  ;      
  ; call MultiplayerBusyWaitForTransmissionComplete ; NOTE: This isn't strictly necessary, but it ensures that rSC hasn't changed since the last check.
  ; ; NOTE: Keep in mind that emulators might be paused entirely, so there's probably a chance of the previously loaded rSC being stale.
  ;
  ; call SendNibble:
  ;   store byte in rSB

.start_transmission
  ; call IfIsMaster:
  ;   set rSC bit 7 to 1 (start transmission)

.cleanup:
	pop de
	pop bc
	pop hl
	ret


; Check if received byte has invalid ACK bit
; Input: E = received byte from rSB
; Output: Z flag set if ACK is valid, clear if invalid
IfReceivedInvalidAck:
	; Extract ACK bit (bit 6) from E and shift to bit 0
	ld a, e
	and %01000000
	rrca
	rrca
	ld b, a  ; Store received ACK in B
	
	; Get expected ACK (flipped version of last sent SEQ)
	ld a, [wMultiplayerNextSeqToSend]
	xor 1  ; Flip to get the last sent SEQ bit
	
	; Compare with received ACK
	cp b
	ret z  ; Z flag set if valid ACK
	
	; Invalid ACK - desync error!
	ld a, ERR_MULTIPLAYER_DESYNC
	jmp Crash


; Check if received byte contains our own nibble (echoed back)
; Input: E = received byte from rSB
; Output: Z flag set if we received our own nibble, clear if it's from remote
IfSBContainsOwnNibble:
  ; Extract Master/Slave bit (bit 4) from received byte
  ld a, e
  and %00010000  ; Isolate bit 4
  ld b, a  ; Store received M/S bit in B
  
  ; Get our current role and shift to bit 4 position
  ld a, [wMultiplayerIsMaster]
  and a
  jr z, .we_are_slave
  
  ; We are master, so our M/S bit should be 1 (bit 4 set)
  ld a, %00010000
  jr .compare
    
.we_are_slave:
  ; We are slave, so our M/S bit should be 0 (bit 4 clear)
  ld a, %00000000
    
.compare:
  ; Compare our expected M/S bit with received M/S bit
  cp b
  jr z, .own_nibble  ; Z flag set if same (our own nibble)
  
  ; Different M/S bit - this is a remote nibble (normal case)
  ret

.own_nibble:
  ; We received our own nibble - this should never happen!
  ld a, ERR_MULTIPLAYER_DESYNC
  jmp Crash


; Check if H/L bit mismatches expected nibble type
; Input: E = received byte from rSB
; Output: Z flag set if H/L bit matches expected, clear if mismatch (desync)
IfHLBitMismatchesExpectedNibble:
	; Extract H/L bit (bit 5) from received byte
	ld a, e
	and %00100000  ; Isolate bit 5 (H/L bit)
	ld b, a  ; Store received H/L bit in B
	
	; Get expected nibble index (1=high nibble expected, 0=low nibble expected)
	ld a, [wMultiplayerReceiveNibbleIdx]
	and a
	jr z, .expect_high_nibble
	
	; We expect low nibble (wReceiveNibbleIdx=1), so H/L bit should be 0
	ld a, %00000000
	jr .compare
	
.expect_high_nibble:
	; We expect high nibble (wReceiveNibbleIdx=0), so H/L bit should be 1
	ld a, %00100000
	
.compare:
	; Compare expected H/L bit with received H/L bit
	cp b
	ret z  ; Z flag set if match (correct nibble)
	
	; H/L bit mismatch - desync error!
	ld a, ERR_MULTIPLAYER_DESYNC
	jmp Crash

; VBlank interrupt handler for multiplayer
MultiplayerVBlankHandler::
  ; Only process if multiplayer is enabled
  ld a, [wMultiplayerIsEnabled]
  and a
  ret z
	
	; Call frame-based multiplayer functions
	call MultiplayerSendReceiveNibble
	ret
