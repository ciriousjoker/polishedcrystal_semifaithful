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
DEF MULTIPLAYER_NOOP_BYTE EQU $0F  ; Use 0x0F (0b00001111) as noop - bit 7 is 0

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
	
	; Check for duplicates (emulator speed mismatch tolerance)
	; Compare with last received rSB value to detect duplicates
	; Since SEQ bit always flips, adjacent chunks can never be identical,
	; making full rSB comparison safe for duplicate detection
	ld hl, wMultiplayerLastReceivedRSB
	cp [hl]
	jr z, .cleanup  ; If same as last received, ignore this duplicate
	
	; Store new rSB value for future duplicate detection
	ld [hl], a
	
  ; NOTE: This isnt necessary, checking bit 7 is enough to detect floating line.
	; Check if SB is floating ($FF indicates no connection)
	cp $FF
	jp z, .restart_package
	
	; Check if bit 7 is set (invalid - should always be 0)
	bit 7, a
	jp nz, .restart_package
	
	; Check if we received our own chunk (should not happen in normal operation)
	call IfSBContainsOwnChunk
	jp z, .start_transmission

	; ; Check if received ACK bit is invalid
	; call IfReceivedInvalidAck
	; jp z, .restart_package

	; Check if H/L bit mismatches expected nibble type
	call IfNibbleIndexMismatchesExpected
	jp z, .restart_package

	; Check if C (chunk index) bit mismatches expected chunk type  
	call IfChunkIndexMismatchesExpected
	jp z, .restart_package

	; Update sequence/acknowledgment variables
	call UpdateSequenceBit
	call UpdateAckBit

	; Handle the received chunk
	call HandleReceivedChunk

.handle_sent_nibble:
  ; ;; Logic to handle the sent chunk
  ; call MultiplayerOnChunkSent:
  ;   increment wMultiplayerSendChunkIdx
  ;   if wMultiplayerSendChunkIdx == 2:
  ;     reset wMultiplayerSendChunkIdx
  ;     increment wMultiplayerSendNibbleIdx
  ;     if wMultiplayerSendNibbleIdx == 2:
  ;       reset wMultiplayerSendNibbleIdx
  ;
  ;       ; NOTE: now the previous byte is fully sent (even if it was a high-noop + low-noop)
  ;       if wMultiplayerHasBufferedPackage:
  ;         increment wMultiplayerSendByteIdx ; if previous byte was a noop, this will now be 1
  ;         if wMultiplayerSendByteIdx == MULTIPLAYER_PACKAGE_SIZE + 1: ; +1 to account for the initial noop that signals the start of a package
  ;           reset wMultiplayerSendByteIdx
  ;           reset wMultiplayerHasBufferedPackage
  ;           call MultiplayerOnCompletePackageSent
  ; jump .send_chunk

.restart_package:
  ; reset send byte counter
  ; NOTE: Use compiler constants for the initial values
  ; reset wMultiplayerReceiveNibbleIdx to initial value
  ; reset wMultiplayerReceiveChunkIdx to initial value
  ; reset wMultiplayerReceiveByteIdx to initial value
  ; reset wMultiplayerNextSeqToSend to initial value
  ; reset wMultiplayerNextAckToSend to initial value
  ; reset wMultiplayerSendNibbleIdx to initial value
  ; reset wMultiplayerSendChunkIdx to initial value
  ; reset wMultiplayerSendByteIdx to initial value
  ; reset wMultiplayerLastReceivedRSB to $FF (force detection of next packet)

.send_chunk:
  ; ;; Logic to send the next chunk:
  ; call PrepareNextChunk
  ;   BuildByteToSend:
  ;     bit 7: always 0 (valid bit - differentiates from floating 0xFF)
  ;     bit 6: use wMultiplayerNextSeqToSend
  ;     bit 5: use wMultiplayerNextAckToSend
  ;     bit 4: use wMultiplayerIsMaster (1 for master, 0 for slave)
  ;     bit 3: use wMultiplayerSendNibbleIdx (0 for high nibble, 1 for low nibble)
  ;     bit 2: use wMultiplayerSendChunkIdx (0 for high chunk, 1 for low chunk)
  ;     bits 1-0: load data from [wMultiplayerStaticNoopByte, ...wMultiplayerBufferedPackage] at wMultiplayerSendByteIdx at wMultiplayerSendNibbleIdx at wMultiplayerSendChunkIdx
  ;     ; NOTE: [wMultiplayerStaticNoopByte] is a constant that is used to treat wMultiplayerSendByteIdx == 0 as a noop byte.
  ;      
  ; call MultiplayerBusyWaitForTransmissionComplete ; NOTE: This isn't strictly necessary, but it ensures that rSC hasn't changed since the last check.
  ; ; NOTE: Keep in mind that emulators might be paused entirely, so there's probably a chance of the previously loaded rSC being stale.
  ;
  ; call SendChunk:
  ;   store byte in rSB

.start_transmission
  ; call IfIsMaster:
  ;   set rSC bit 7 to 1 (start transmission)

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
	
  call Desync
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
  jr z, .own_nibble  ; Z flag set if same (our own nibble)
  
  ; Different M/S bit - this is a remote chunk (normal case)
  ret

.own_nibble:
  call Desync

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
