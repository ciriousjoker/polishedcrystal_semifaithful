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
; Bit 6: SeqOut - Toggle bit for sequence control. Toggled each frame if AckIn matched SeqOut.
; Bit 5: AckOut - Flipped version of the last received SeqIn (ie we predict the happy path and wait for sync if it fails).
; Bit 4: Master/Slave - 1 if master, 0 if slave (used to avoid interpreting own chunks as received ones)
; Bit 3: Nibble Index - 0=High nibble, 1=Low nibble  
; Bit 2: Chunk Index - 0=High chunk (bits 3-2), 1=Low chunk (bits 1-0)
; Bits 1-0: Payload chunk (2 bits)

DEF MULTIPLAYER_PACKAGE_SIZE EQU 8
DEF MULTIPLAYER_IDLE_FRAMES EQU 10 ; 5 * 60fps -> ~5s
DEF MULTIPLAYER_NOOP_BYTE EQU $FF  ; Use 0xFF as a noop (which also is the package start marker).
                                   ; It's transferred across 4 chunks.

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
	ld a, MULTIPLAYER_NOOP_BYTE
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
	ld [wMultiplayerSendChunkIdx], a
	
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
	
	; Check if we received our own chunk FIRST (before duplicate detection)
	call IfSBContainsOwnChunk
	jr z, .cleanup  ; If our own chunk, ignore it completely
	
	; Now check for duplicates (only for remote chunks)
	call IfIsDuplicate
	jr z, .handle_duplicate
	
	; Store new rSB value for future duplicate detection (only remote chunks)
	ld hl, wMultiplayerLastReceivedRSB
	ld [hl], e
	
	; Normal processing - new chunk from remote
	jr .process_new_chunk

.handle_duplicate:
	; Handle duplicate chunk from remote side
	; (We already know it's not our own chunk from the check above)
	call PrepareChunkToResend
	jr .send_chunk

.process_new_chunk:
	; Normal processing of new chunk
	; Check if H/L bit mismatches expected nibble type
	call IfNibbleIndexMismatchesExpected
	jp z, .restart_their_package

	; Check if C (chunk index) bit mismatches expected chunk type  
	call IfChunkIndexMismatchesExpected
	jp z, .restart_their_package

	; Check if received ACK is invalid (they didn't acknowledge our data properly)
	call IfReceivedInvalidAck
	jp z, .restart_own_package  ; Invalid ACK = restart our own transmission

	; Update sequence/acknowledgment variables
	call UpdateSequenceBit
	call UpdateAckBit

	; Handle the received chunk
	call HandleReceivedChunk

	; Advance send state after successful acknowledgment
	; Only advances if the ACK validates that the remote side received our chunk
	call AdvanceSendState
	jr .send_chunk

.restart_own_package:
	; Complete desync detected - reset all state variables to initial values
	; This is for when we detect our own connection issues (floating line, invalid bits)
	
	; Reset send state variables to initial values
  xor a
	ld [wMultiplayerSendNibbleIdx], a       ; 0 = send high nibble
	ld [wMultiplayerSendChunkIdx], a        ; 0 = send high chunk
	ld [wMultiplayerSendByteIdx], a         ; 0 = send noop byte
	
	; Reset ACK variable to initial value
	ld [wMultiplayerNextAckToSend], a       ; 0 = initial ACK
	
	; Force detection of next packet by setting last received to invalid value
	ld a, $FF
	ld [wMultiplayerLastReceivedRSB], a
	
	; Continue to send_chunk - we'll start sending noop + chunk 0, nibble 0, etc.
	jr .send_chunk

.restart_their_package:
	; Their package is out of sync - send restart signal once
	; This is for when we detect their nibble/chunk indices are wrong
	
	; Reset our own receive indices since we're out of sync with them
	xor a
	ld [wMultiplayerReceiveNibbleIdx], a    ; 0 = expect high nibble
	ld [wMultiplayerReceiveChunkIdx], a     ; 0 = expect high chunk  
	ld [wMultiplayerReceiveByteIdx], a      ; 0 = expect first byte
	
	; Force detection of next packet by setting last received to invalid value
	ld a, $FF
	ld [wMultiplayerLastReceivedRSB], a
	
	; Send restart signal (0xFF) to tell remote side to restart
	ld a, %10000000
	ldh [rSB], a
	jr .start_transmission

.send_chunk:
	; Prepare the next chunk to send
	call PrepareNextChunk
	
	; Send the prepared chunk via serial
	call SendChunk

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

; Flip the next sequence bit to send
; We ALWAYS flip the sequence bit, so the remote side can detect if we sent a new package,
; even if it was otherwise identical to the last one (e.g. we resend a nibble that was lost).
UpdateSequenceBit:
	ld a, [wMultiplayerNextSeqToSend]
	xor 1
	ld [wMultiplayerNextSeqToSend], a
	ret

; Set next ACK to send based on received sequence bit
; Input: E = received byte from rSB
UpdateAckBit:
	ld a, e
	and %01000000	; Extract SEQ bit (bit 6)
	rrca
	rrca
	rrca
	rrca
	rrca
	rrca	; Shift to bit 0
	xor 1	; Flip it
	ld [wMultiplayerNextAckToSend], a
	ret

; Check if the received rSB value is a duplicate of the last received value
; Input: E = received byte from rSB
; Output: Z flag set if duplicate, clear if not duplicate
; Destroys: A, H, L
IfIsDuplicate:
	; Compare with last received rSB value to detect duplicates
	ld hl, wMultiplayerLastReceivedRSB
	ld a, [hl]
	cp e  ; Compare with received byte
	ret   ; Z flag will be set if duplicate, clear if not

; Prepare chunk for retransmission after duplicate detection
; Flips the SEQ bit and sets up for resending current chunk
; Destroys: A
PrepareChunkToResend:
	; For duplicates, flip SEQ bit to indicate retransmission
	ld a, [wMultiplayerNextSeqToSend]
	xor 1  ; Flip the bit for retransmission
	ld [wMultiplayerNextSeqToSend], a
	ret

; Check if received rSB has an invalid ACK bit
; Must be called before wMultiplayerNextSeqToSend is flipped for the current package
; Input:
;   - E = received byte from rSB
;   - wMultiplayerNextSeqToSend (unmodified, ie before flipping it for the current package)
; Output: Z flag set if ACK is invalid, clear if valid
IfReceivedInvalidAck:
	; Extract ACK bit (bit 5) from E and shift to bit 0
	ld a, e
	and %00100000
	rrca
	rrca
	rrca
	rrca
	rrca	; Shift to bit 0
	ld b, a  ; Store received ACK in B

  ; The received ACK must match the last sent SEQ directly
	ld a, [wMultiplayerNextSeqToSend]
	
	; Compare with received ACK
	cp b
	jr z, .valid_ack  ; Jump if ACKs match (valid)
	
  ; call Desync
	xor a  ; Set Z flag
	ret

.valid_ack:
	; Valid ACK - clear Z flag and return
	or 1   ; Clear Z flag
	ret

; Check if received byte contains our own chunk (echoed back)
; Input: E = received byte from rSB
; Output: Z flag set if we received our own chunk, clear if it's from remote
IfSBContainsOwnChunk:
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
  jr z, .own_chunk  ; Z flag set if same (our own chunk)
  
  ; Different M/S bit - this is a remote chunk (normal case)
  ; Clear Z flag to indicate remote chunk
  or 1
  ret

.own_chunk:
  ; call Desync
  ; Z flag is already set from the cp instruction
  ; Return with Z flag set to indicate own chunk
  ret

; Check if nibble index bit mismatches expected nibble type
; Input: E = received byte from rSB
; Output: Z flag set if nibble index mismatches (desync), clear if matches
IfNibbleIndexMismatchesExpected:
	; Extract nibble index bit (bit 3) from received byte
	ld a, e
	and %00001000	; Isolate bit 3 (nibble index bit)
	ld b, a	; Store received nibble index bit in B
	
	; Get expected nibble index (0=high nibble expected, 1=low nibble expected)
	ld a, [wMultiplayerReceiveNibbleIdx]
	and a
	jr z, .expect_high_nibble
	
	; We expect low nibble (wReceiveNibbleIdx=1), so nibble index bit should be 1
	ld a, %00001000
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

; Check if chunk index bit mismatches expected chunk type
; Input: E = received byte from rSB
; Output: Z flag set if chunk index mismatches (desync), clear if matches
IfChunkIndexMismatchesExpected:
	; Extract chunk index bit (bit 2) from received byte
	ld a, e
	and %00000100	; Isolate bit 2 (chunk index bit)
	ld b, a	; Store received chunk index bit in B
	
	; Get expected chunk index (0=low chunk expected, 1=high chunk expected)
	ld a, [wMultiplayerReceiveChunkIdx]
	and a
	jr z, .expect_low_chunk
	
	; We expect high chunk (wReceiveChunkIdx=1), so chunk index bit should be 1
	ld a, %00000100
	jr .compare_chunk
    
.expect_low_chunk:
	; We expect low chunk (wReceiveChunkIdx=0), so chunk index bit should be 0
	ld a, %00000000
    
.compare_chunk:
	; Compare expected chunk index bit with received chunk index bit
	cp b
	jr z, .chunk_match	; Jump if they match (correct chunk)
	
	; Mismatch - set Z flag and return
	xor a	; Set Z flag
	ret
    
.chunk_match:
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

; Process a received chunk and assemble it into nibbles/bytes/packages
; Input: E = received byte from rSB  
; Destroys: A, B, C, D, H, L
HandleReceivedChunk:
	; Store the received chunk in the correct position
	call StoreReceivedChunk
	
	; Increment chunk index
	ld a, [wMultiplayerReceiveChunkIdx]
	inc a
	ld [wMultiplayerReceiveChunkIdx], a
	
	; Check if we've received both chunks of a nibble (high and low)
	cp 2
	ret c	; Return if we haven't completed a nibble yet
	
	; We've completed a nibble, handle it
	call HandleCompletedNibble
	ret

; Store received chunk in wMultiplayerLastReceivedNibble
; Input: E = received byte from rSB
; Destroys: A, B
StoreReceivedChunk:
	; Extract the payload chunk (bits 1-0)
	ld a, e
	and %00000011
	ld b, a	; Store chunk in B
	
	; Check which chunk position we're expecting
	ld a, [wMultiplayerReceiveChunkIdx]
	and a
	jr z, .store_high_chunk
	
	; Store as low chunk (bits 1-0)
	ld a, [wMultiplayerLastReceivedNibble]
	and %11111100	; Clear low chunk (bits 1-0)
	or b	; Combine with new low chunk
	ld [wMultiplayerLastReceivedNibble], a
	ret
	
.store_high_chunk:
	; Store as high chunk (bits 3-2)
	ld a, [wMultiplayerLastReceivedNibble]
	and %11110011	; Clear high chunk
	ld c, a
	ld a, b
	add a, a
	add a, a	; Shift chunk to high position (bits 3-2)
	or c	; Combine with existing nibble
	ld [wMultiplayerLastReceivedNibble], a
	ret

; Handle a completed nibble (both chunks received)
; Destroys: A, B, C, D, H, L
HandleCompletedNibble:
	; Reset chunk index for next nibble
	xor a
	ld [wMultiplayerReceiveChunkIdx], a
	
	; Store nibble in correct position of current byte
	call StoreReceivedNibble
	
	; Increment nibble index
	ld a, [wMultiplayerReceiveNibbleIdx]
	inc a
	ld [wMultiplayerReceiveNibbleIdx], a
	
	; Check if we've received both nibbles (high and low)
	cp 2
	ret c	; Return if we haven't completed a byte yet
	
	; We've completed a byte, handle it
	call HandleCompletedByte
	ret

; Store received nibble in wMultiplayerLastReceivedByte
; Input: Uses wMultiplayerLastReceivedNibble (assembled from chunks)
; Destroys: A, B
StoreReceivedNibble:
	; Get the completed nibble
	ld a, [wMultiplayerLastReceivedNibble]
	ld b, a	; Store nibble in B
	
	; Check which nibble position we're expecting
	ld a, [wMultiplayerReceiveNibbleIdx]
	and a
	jr z, .store_high_nibble
	
	; Store as low nibble (bits 3-0)
	ld a, [wMultiplayerLastReceivedByte]
	and %11110000	; Clear low nibble
	or b	; Set low nibble
	ld [wMultiplayerLastReceivedByte], a
	ret
	
.store_high_nibble:
	; Store as high nibble (bits 7-4)
	ld a, b
	add a, a
	add a, a
	add a, a
	add a, a	; Shift nibble to high position
	ld [wMultiplayerLastReceivedByte], a
	ret

; Handle a completed byte (both nibbles received)
; Destroys: A, B, C, D, H, L
HandleCompletedByte:
	; Reset nibble index for next byte
	xor a
	ld [wMultiplayerReceiveNibbleIdx], a
	
	; Check if received byte is a noop ($81)
	ld a, [wMultiplayerLastReceivedByte]
	cp MULTIPLAYER_NOOP_BYTE
	jr z, .received_noop
	
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
	
.received_noop:
	; Received noop byte - reset to start of new package
	xor a
	ld [wMultiplayerReceiveByteIdx], a
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

; Advance send state machine after successful ACK
; Only called when the remote side has acknowledged our last chunk
; Progresses through: chunk -> nibble -> byte -> package
; Destroys: A, B, C
AdvanceSendState:
	; Increment chunk index
	ld a, [wMultiplayerSendChunkIdx]
	inc a
	ld [wMultiplayerSendChunkIdx], a
	
	; Check if we've completed a nibble (sent both chunks)
	cp 2
	ret c	; Return if we haven't completed a nibble yet
	
	; Reset chunk index and advance to next nibble
	xor a
	ld [wMultiplayerSendChunkIdx], a
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
	; +1 to account for the initial noop that signals the start of a package
	ld a, [wMultiplayerSendByteIdx]  ; Re-load the incremented value
	cp MULTIPLAYER_PACKAGE_SIZE + 1
	ret c	; Return if package not complete yet
	
	; Package complete! Reset state and call completion handler
	xor a
	ld [wMultiplayerSendByteIdx], a
	ld [wMultiplayerHasBufferedPackage], a
	call OnPackageTransmissionComplete
	ret

.no_package:
	; No package to send, but byte index was already incremented
	; This allows continuous noop transmission when no package is buffered
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

; Prepare the next chunk for transmission
; Builds the complete 8-bit transmission byte with metadata and payload
; Output: A = prepared byte ready for rSB
; Destroys: A, B, C, D, H, L
PrepareNextChunk:
	; Start with bit 7 = 0 (valid bit)
	ld a, 0
	ld d, a	; D will accumulate the final byte
	
	; Bit 6: SEQ bit
	ld a, [wMultiplayerNextSeqToSend]
	and 1
	add a, a
	add a, a
	add a, a
	add a, a
	add a, a
	add a, a	; Shift to bit 6
	or d
	ld d, a
	
	; Bit 5: ACK bit  
	ld a, [wMultiplayerNextAckToSend]
	and 1
	add a, a
	add a, a
	add a, a
	add a, a
	add a, a	; Shift to bit 5
	or d
	ld d, a
	
	; Bit 4: Master/Slave bit
	ld a, [wMultiplayerIsMaster]
	and 1
	add a, a
	add a, a
	add a, a
	add a, a	; Shift to bit 4
	or d
	ld d, a
	
	; Bit 3: Nibble index (0=high, 1=low)
	ld a, [wMultiplayerSendNibbleIdx]
	and 1
	add a, a
	add a, a
	add a, a	; Shift to bit 3
	or d
	ld d, a
	
	; Bit 2: Chunk index (0=high, 1=low)
	ld a, [wMultiplayerSendChunkIdx]
	and 1
	add a, a
	add a, a	; Shift to bit 2
	or d
	ld d, a
	
	; Bits 1-0: Payload chunk (2 bits from current byte/nibble/chunk)
	call GetCurrentPayloadChunk
	and %00000011	; Ensure only 2 bits
	or d
	ld d, a
	
	; Return prepared byte in A
	ld a, d
	ret

; Get the current 2-bit payload chunk to send
; Returns the chunk from either noop byte or buffered package
; Output: A = 2-bit chunk (bits 1-0 only)
; Destroys: A, B, C, H, L
GetCurrentPayloadChunk:
	; Check if we're sending noop (byte index 0) or package data
	ld a, [wMultiplayerSendByteIdx]
	and a
	jr z, .send_noop
	
	; Check if we have a buffered package
	ld a, [wMultiplayerHasBufferedPackage]
	and a
	jr z, .send_noop	; No package, send noop
	
	; Get byte from buffered package (byte index 1-8 maps to array index 0-7)
	ld a, [wMultiplayerSendByteIdx]
	dec a	; Convert to 0-based index
	ld c, a
	ld b, 0
	ld hl, wMultiplayerBufferedPackage
	add hl, bc	; HL points to current byte
	ld a, [hl]	; Load the byte
	jr .extract_chunk
	
.send_noop:
	; Send noop byte
	ld a, [wMultiplayerStaticNoopByte]
	; Fall through to extract_chunk
	
.extract_chunk:
	; A now contains the source byte
	; Extract nibble based on wMultiplayerSendNibbleIdx
	ld b, a	; Save original byte
	ld a, [wMultiplayerSendNibbleIdx]
	and a
	jr z, .high_nibble
	
	; Low nibble (bits 3-0)
	ld a, b
	and %00001111
	jr .extract_chunk_from_nibble
	
.high_nibble:
	; High nibble (bits 7-4), shift to low position
	ld a, b
	rrca
	rrca
	rrca
	rrca
	and %00001111
	
.extract_chunk_from_nibble:
	; A now contains the 4-bit nibble
	; Extract chunk based on wMultiplayerSendChunkIdx
	ld b, a	; Save nibble
	ld a, [wMultiplayerSendChunkIdx]
	and a
	jr z, .high_chunk
	
	; Low chunk (bits 1-0)
	ld a, b
	and %00000011
	ret
	
.high_chunk:
	; High chunk (bits 3-2), shift to low position
	ld a, b
	rrca
	rrca
	and %00000011
	ret

; Send the prepared chunk via serial communication
; Input: A = byte to send
; Destroys: A
SendChunk:
	; Store byte in serial buffer
	ldh [rSB], a
	ret
