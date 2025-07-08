; Frame-based multiplayer communication system
; Full-duplex, piggy-back-ACK link protocol
; Each frame processes one nibble with sequence/ACK bits
;
; Frame Timing: The VBlank handler increments a frame counter and only calls
; MultiplayerSendReceiveNibble when MULTIPLAYER_IDLE_FRAMES frames
; have elapsed, providing a controlled transmission rate to prevent overloading.

SECTION "Frame Multiplayer", ROMX

; SB Register bit layout (7 6 5 4 3 2 1 0):
; Bit 7: Valid - Always 0 to differentiate from floating line (0xFF)
; Bit 6: Master/Slave - 1 if master, 0 if slave (used to avoid interpreting own chunks as received ones)
; Bit 5: Reset - 1 to signal start of new package (resets receiver indices)
; Bit 4: Nibble Index - 0=High nibble, 1=Low nibble  
; Bits 3-0: Payload nibble (4 bits)

DEF MULTIPLAYER_PACKAGE_SIZE EQU 8
DEF MULTIPLAYER_IDLE_FRAMES EQU 10 ; 5 * 60fps -> ~5s

; MultiplayerInitialize:
; Purpose: Initializes all multiplayer-related RAM variables and hardware registers to a clean state.
;          This should be called once when activating the multiplayer feature.
MultiplayerInitialize::
	; Clear all package buffers to 0.
	ld hl, wMultiplayerStart
	ld bc, wMultiplayerEnd - wMultiplayerStart ; Zero out all multiplayer WRAM variables up to the temp buffer
	xor a
	rst ByteFill

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
	jp z, .restart_own_package
	
	; Check if bit 7 is set (invalid - should always be 0)
	bit 7, a
	jp nz, .restart_own_package
	
	; Check if we received our own nibble FIRST (before duplicate detection)
	call IfSBContainsOwnNibble
	jr z, .cleanup  ; If our own nibble, ignore it completely
	
	; Store new rSB value for future duplicate detection (only remote nibbles)
	ld hl, wMultiplayerLastReceivedRSB
	ld [hl], e
	
	; Normal processing - new nibble from remote
	jr .process_new_nibble

; .handle_duplicate:
; 	; Handle duplicate chunk from remote side
; 	; (We already know it's not our own chunk from the check above)
; 	call PrepareChunkToResend
; 	jr .send_chunk

.process_new_nibble:
	; Normal processing of new nibble
	; Check if N (nibble index) bit mismatches expected nibble type
	call IfNibbleIndexMismatchesExpected
	jp z, .restart_their_package

	; Handle the received nibble
	call HandleReceivedNibble

	; Advance send state after successful reception
	call AdvanceSendState
	jr .send_nibble

; .resync_seq:
; 	; Resend the exact same byte to trigger duplicate detection on the other side
; 	call ResendExactSameByte
; 	jr .start_transmission

.restart_own_package:
	; Complete desync detected - reset all state variables to initial values
	; This is for when we detect our own connection issues (floating line, invalid bits)
	
	; Reset send state variables to initial values
	xor a
	ld [wMultiplayerSendNibbleIdx], a       ; 0 = send high nibble
	ld [wMultiplayerSendByteIdx], a         ; 0 = send noop byte
	
	; Force detection of next packet by setting last received to invalid value
	ld a, $FF
	ld [wMultiplayerLastReceivedRSB], a
	
	; Continue to send_nibble - we'll start sending noop + nibble 0, etc.
	jr .send_nibble

.restart_their_package:
	; Their package is out of sync - send restart signal once
	; This is for when we detect their nibble/chunk indices are wrong

	; Reset our own receive indices since we're out of sync with them
	xor a
	ld [wMultiplayerReceiveNibbleIdx], a    ; 0 = expect high nibble
	ld [wMultiplayerReceiveByteIdx], a      ; 0 = expect first byte
	
	; Force detection of next packet by setting last received to invalid value
	ld a, $FF
	ld [wMultiplayerLastReceivedRSB], a
	
	; Send restart signal (0xFF) to tell remote side to restart
	ld a, %10000000
	ldh [rSB], a
	jr .start_transmission

.send_nibble:
	; Prepare the next nibble to send
	call PrepareNextNibble
	
	; Send the prepared nibble via serial
	call SendNibble

.start_transmission:
	; Both master and slave need to set rSC for transfer to work
	ld a, [wMultiplayerIsMaster]
	and a
	jr z, .slave_transmission
	
	; We are master - start the serial transmission with internal clock
	ld a, %10000001	; Bit 7=1 (start transfer), Bit 0=1 (internal clock)
	ldh [rSC], a
	jr .cleanup
	
.slave_transmission:
	; We are slave - prepare for transmission with external clock
	ld a, %10000000	; Bit 7=1 (start transfer), Bit 0=0 (external clock)
	ldh [rSC], a
	jr .cleanup

.cleanup:
	pop de
	pop bc
	pop hl
	ret

; Check if received byte contains our own nibble (echoed back)
; Input: E = received byte from rSB
; Output: Z flag set if we received our own nibble, clear if it's from remote
; TODO: This should never actually be necessary. Maybe crash if it happens?
IfSBContainsOwnNibble:
  ; Extract Master/Slave bit (bit 6) from received byte
  ld a, e
  and %01000000  ; Isolate bit 6
  ld b, a  ; Store received M/S bit in B
  
  ; Get our current role and shift to bit 6 position
  ld a, [wMultiplayerIsMaster]
  and a
  jr z, .we_are_slave
  
  ; We are master, so our M/S bit should be 1 (bit 6 set)
  ld a, %01000000
  jr .compare
    
.we_are_slave:
  ; We are slave, so our M/S bit should be 0 (bit 6 clear)
  ld a, %00000000
    
.compare:
  ; Compare our expected M/S bit with received M/S bit
  cp b
  jr z, .own_nibble  ; Z flag set if same (our own nibble)
  
  ; Different M/S bit - this is a remote nibble (normal case)
  ; Clear Z flag to indicate remote nibble
  or 1
  ret

.own_nibble:
  ; call Desync
  ; Z flag is already set from the cp instruction
  ; Return with Z flag set to indicate own nibble
  ret

; Check if nibble index bit mismatches expected nibble type
; Input: E = received byte from rSB
; Output: Z flag set if nibble index mismatches (desync), clear if matches
IfNibbleIndexMismatchesExpected:
	; Extract nibble index bit (bit 4) from received byte
	ld a, e
	and %00010000	; Isolate bit 4 (nibble index bit)
	ld b, a	; Store received nibble index bit in B
	
	; Get expected nibble index (0=high nibble expected, 1=low nibble expected)
	ld a, [wMultiplayerReceiveNibbleIdx]
	and a
	jr z, .expect_high_nibble
	
	; We expect low nibble (wReceiveNibbleIdx=1), so nibble index bit should be 1
	ld a, %00010000
	jr .compare
    
.expect_high_nibble:
	; We expect high nibble (wReceiveNibbleIdx=0), so nibble index bit should be 0
	ld a, %00000000
    
.compare:
	; Compare expected nibble index bit with received nibble index bit
	cp b
	jr z, .match	; Jump if they match (correct nibble)
	
	; call Desync
	; Mismatch - set Z flag and return
	xor a	; Set Z flag
	ret
    
.match:
	; Match - clear Z flag and return
	or 1	; Clear Z flag
	ret



Desync:
	; H/L bit mismatch - desync error!
	ld a, ERR_MULTIPLAYER_DESYNC
	jmp Crash

; Check if enough frames have passed since the last transmission.
; Input: None
; Output: Z flag set if ready for transmission, clear if not ready yet
; Destroys: A
IfIsReady:
	; Increment frame counter
	ld a, [wMultiplayerFrameCounter]
	inc a
	ld [wMultiplayerFrameCounter], a
	
	; Check if counter reached transmission threshold
	cp MULTIPLAYER_IDLE_FRAMES
	jr c, .not_ready
	
	; Reset counter and signal ready
	xor a
	ld [wMultiplayerFrameCounter], a
	; Z flag is already set from 'xor a'
	ret
	
.not_ready:
	; Clear Z flag to indicate not ready
	or 1
	ret

; VBlank interrupt handler for multiplayer
MultiplayerVBlankHandler::
	; Only process if multiplayer is enabled
	ld a, [wMultiplayerIsEnabled]
	and a
	ret z
	
	; We don't need maximum performance here,
  ; so we throttle the transmission rate a bit.
  ; This makes desyncs less likely and should improve the fps.
	call IfIsReady
	ret nz
	
	; Call frame-based multiplayer functions
	call MultiplayerSendReceiveNibble
	ret

; Process a received nibble and assemble it into bytes/packages
; Input: E = received byte from rSB  
; Destroys: A, B, C, D, H, L
HandleReceivedNibble:
	; Check if this nibble has the reset flag (bit 5)
	ld a, e
	and %00100000	; Isolate reset bit (bit 5)
	jr z, .process_nibble
	
	; Reset flag is set - reset receive indices and validate nibble index
	xor a
	ld [wMultiplayerReceiveByteIdx], a      ; Reset to start of package
	ld [wMultiplayerReceiveNibbleIdx], a    ; Reset to high nibble
	
.process_nibble:
	; Extract 4-bit payload from bits 3-0
	ld a, e
	and %00001111	; Isolate payload bits (3-0)
	
	; Check which nibble we're receiving (high or low)
	ld b, a	; Store payload in B
	ld a, [wMultiplayerReceiveNibbleIdx]
	and a
	jr z, .store_high_nibble
	
	; Low nibble - combine with stored high nibble
	ld a, [wMultiplayerLastReceivedByte]
	and %11110000	; Keep high nibble
	or b	; Combine with low nibble
	ld [wMultiplayerLastReceivedByte], a
	
	; Complete byte received - handle it
	call HandleCompletedByte
	jr .advance_nibble
	
.store_high_nibble:
	; High nibble - store in upper 4 bits
	ld a, b
	swap a	; Move to high nibble position
	ld [wMultiplayerLastReceivedByte], a
	
.advance_nibble:
	; Advance to next nibble
	ld a, [wMultiplayerReceiveNibbleIdx]
	xor 1	; Toggle between 0 and 1
	ld [wMultiplayerReceiveNibbleIdx], a
	ret

; Handle a completed byte (both nibbles received)
; Destroys: A, B, C, D, H, L
HandleCompletedByte:
	; Reset nibble index for next byte
	xor a
	ld [wMultiplayerReceiveNibbleIdx], a
	
	; Store byte in receive package
	ld a, [wMultiplayerReceiveByteIdx]
	ld c, a	; Store byte index in C
	ld b, 0
	ld hl, wMultiplayerReceivedPackage
	add hl, bc	; HL now points to correct byte in ram
	
	ld a, [wMultiplayerLastReceivedByte]
	ld [hl], a	; Store the byte
	
	; Increment byte index
	inc c
	ld a, c
	ld [wMultiplayerReceiveByteIdx], a
	
	; Check if we've completed a full package
	cp MULTIPLAYER_PACKAGE_SIZE
	jr z, .package_complete
	ret
	
.package_complete:
	; Copy received package to execution buffer
	ld hl, wMultiplayerReceivedPackage
	ld de, wMultiplayerPackageToExecute
	ld bc, MULTIPLAYER_PACKAGE_SIZE
	rst CopyBytes
	
	; Call package received handler
	call MultiplayerOnPackageReceived
	ret

; Handle when a complete package has been received
; Destroys: A, H, L
MultiplayerOnPackageReceived:
	; Reset receive state for next package
	xor a
	ld [wMultiplayerReceiveByteIdx], a
	ld [wMultiplayerReceiveNibbleIdx], a
	
	; Clear the received package buffer
	ld hl, wMultiplayerReceivedPackage
	ld bc, MULTIPLAYER_PACKAGE_SIZE
	xor a
	rst ByteFill
	
	; TODO: Process the package in wMultiplayerPackageToExecute
	call PrintTextLazy
	ret

; Advance send state machine after successful reception
; Progresses through: nibble -> byte -> package
; Destroys: A, B, C
AdvanceSendState:
	; Advance to next nibble
	call AdvanceToNextNibble
	ret

; Advance to next nibble after completing current nibble
; Progresses nibble state and checks for completed bytes/packages
; Destroys: A, B, C
AdvanceToNextNibble:
	; Increment nibble index
	ld a, [wMultiplayerSendNibbleIdx]
	inc a
	ld [wMultiplayerSendNibbleIdx], a
	
	; Check if we've completed a byte (sent both nibbles)
	cp 2
	ret c	; Return if we haven't completed a byte yet
	
	; Reset nibble index and advance to next byte
	xor a
	ld [wMultiplayerSendNibbleIdx], a
	call AdvanceToNextByte
	ret

; Advance to next byte after completing current byte
; Progresses byte state and checks for completed packages
; Destroys: A, B, C
AdvanceToNextByte:
	; Always increment byte index when completing a byte
	ld a, [wMultiplayerSendByteIdx]
	inc a
	ld [wMultiplayerSendByteIdx], a
	
	; Check if we have a buffered package to send
	ld a, [wMultiplayerHasBufferedPackage]
	and a
	jr z, .no_package	; If no package, just return
	
	; Check if we've completed the entire package
	ld a, [wMultiplayerSendByteIdx]  ; Re-load the incremented value
	cp MULTIPLAYER_PACKAGE_SIZE
	ret c	; Return if package not complete yet
	
	; Package complete! Reset state and call completion handler
	xor a
	ld [wMultiplayerSendByteIdx], a
	ld [wMultiplayerHasBufferedPackage], a
	call OnPackageTransmissionComplete
	ret

.no_package:
	; No package to send, but byte index was already incremented
	; This allows continuous transmission when no package is buffered
	ret

; Handle completion of entire package transmission
; Called when all bytes (including noop) have been successfully acknowledged
; Destroys: A, H, L
OnPackageTransmissionComplete:
	; Try to move queued package to buffered if available
	call MultiplayerCopyQueuedPackageToSendBufferIfPossible
	
	; ld a, ERR_EGG_SPECIES
	; jmp Crash

	; TODO: Add any other completion logic here (e.g., notify game logic)
	ret

; Prepare the next nibble for transmission
; Builds the complete 8-bit transmission byte with metadata and payload
; Output: A = prepared byte ready for rSB
; Destroys: A, B, C, D, H, L
PrepareNextNibble:
	; Start with bit 7 = 0 (valid bit)
	ld a, 0
	ld d, a	; D will accumulate the final byte
	
	; Bit 6: Master/Slave bit
	ld a, [wMultiplayerIsMaster]
	and 1
	add a, a
	add a, a
	add a, a
	add a, a
	add a, a
	add a, a	; Shift to bit 6
	or d
	ld d, a
	
	; Bit 5: Reset bit - set if this is the first nibble of a new package
	call ShouldSetResetBit
	and 1
	add a, a
	add a, a
	add a, a
	add a, a
	add a, a	; Shift to bit 5
	or d
	ld d, a
	
	; Bit 4: Nibble index (0=high, 1=low)
	ld a, [wMultiplayerSendNibbleIdx]
	and 1
	add a, a
	add a, a
	add a, a
	add a, a	; Shift to bit 4
	or d
	ld d, a
	
	; Bits 3-0: Payload nibble (4 bits from current byte/nibble)
	call GetCurrentPayloadNibble
	and %00001111	; Ensure only 4 bits
	or d
	ld d, a
	
	; Return prepared byte in A
	ld a, d
	ret

; Check if we should set the reset bit (start of new package)
; Output: A = 1 if reset bit should be set, 0 otherwise
; Destroys: A
ShouldSetResetBit:
	; Reset bit should be set if:
	; - No package is being sent, OR
	; - We're at the first nibble of a package (SendByte == 0 && SendNibble == 0)
	
	; Check if we have a buffered package
	ld a, [wMultiplayerHasBufferedPackage]
	and a
	jr z, .set_reset	; No package = always set reset
	
	; We have a package - check if we're at the very first nibble
	ld a, [wMultiplayerSendByteIdx]
	and a
	jr nz, .clear_reset	; Not byte 0 = clear reset
	
	; We're at byte 0 - check if we're at nibble 0
	ld a, [wMultiplayerSendNibbleIdx]
	and a
	jr nz, .clear_reset	; Not nibble 0 = clear reset
	
	; We're at byte 0, nibble 0 = first nibble of package
.set_reset:
	ld a, 1
	ret
	
.clear_reset:
	ld a, 0
	ret

; Get the current 4-bit payload nibble to send
; Returns the nibble from either buffered package or any value if no package
; Output: A = 4-bit nibble (bits 3-0 only)
; Destroys: A, B, C, H, L
GetCurrentPayloadNibble:
	; Check if we have a buffered package
	ld a, [wMultiplayerHasBufferedPackage]
	and a
	jr z, .no_package	; No package, send any value
	
	; Get byte from buffered package
	ld a, [wMultiplayerSendByteIdx]
	ld c, a
	ld b, 0
	ld hl, wMultiplayerBufferedPackage
	add hl, bc	; HL points to current byte
	ld a, [hl]	; Load the byte
	jr .extract_nibble
	
.no_package:
	; No package to send - send any value (it doesn't matter)
	; The reset flag will signal the start of actual package data
	ld a, $00	; Send zeros when no package
	
.extract_nibble:
	; A now contains the source byte
	; Extract nibble based on wMultiplayerSendNibbleIdx
	ld b, a	; Save original byte
	ld a, [wMultiplayerSendNibbleIdx]
	and a
	jr z, .high_nibble
	
	; Low nibble (bits 3-0)
	ld a, b
	and %00001111
	ret
	
.high_nibble:
	; High nibble (bits 7-4), shift to low position
	ld a, b
	swap a
	and %00001111
	ret

; Send the prepared nibble via serial communication
; Input: A = byte to send
; Destroys: A
SendNibble:
	; Store byte in serial buffer
	ldh [rSB], a
	ret

