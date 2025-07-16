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
;
; Protocol Notes:
; - When no package is being sent, nibble index stays at 0 and reset flag stays set
; - Send state only advances when transmitting real packages, not "filler" nibbles
; - Reset flag can ONLY be set when nibble index is 0 (prevents invalid combinations)

MultiplayerPackageLengths:
		db 1  ; MULTIPLAYER_PKG_INIT
		db 9  ; MULTIPLAYER_PKG_NAME
		db 2  ; MULTIPLAYER_PKG_GIFT_ITEM
		db 1  ; MULTIPLAYER_PKG_NOOP
		db 4  ; MULTIPLAYER_PKG_SEND_POSITION
		db 1  ; MULTIPLAYER_PKG_PHONECALL
		; Add more as needed, in the same order as the constants

; Gets the package size based on the package type.
; Input:  A = package type (index)
; Output: A = package size
; Destroys: HL, B, C
MultiplayerGetPackageSize::
	ld hl, MultiplayerPackageLengths
	ld c, a
	ld b, 0
	add hl, bc
	ld a, [hl]
	ret

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

	; Initialize frame counter to 1.
  ; This fixes a once-per launch underflow error where
  ; the first package takes 0xFF frames to send.
	ld a, 1
	ld [wMultiplayerFrameCounter], a

	ld [wMultiplayerIsEnabled], a
	ret

; Queue a package for transmission
; Input: HL = source address of package (first byte = type)
MultiplayerQueuePackage::
	push hl
	push bc
	push de

	; Check if a critical package is already queued.
	ld a, [wMultiplayerHasQueuedCriticalPackage]
	and a
	jr z, .can_queue ; No critical package queued, proceed.

	; A critical package is already queued. Check if the new one is also critical.
	ld a, [hl] ; package type is in first byte of source
	cp MULTIPLAYER_PKG_NOOP
	jr c, .desync_now ; If new package is critical, desync.

	; New package is not critical, but a critical one is queued. So we do nothing.
	jr .done

.desync_now:
	jp Desync
	
.can_queue:
	; Copy the entire max size to queued buffer
	ld de, wMultiplayerQueuedPackage
	ld bc, MULTIPLAYER_MAX_PACKAGE_SIZE
	rst CopyBytes
	
	; Set queued flag
	ld a, 1
	ld [wMultiplayerHasQueuedPackage], a

	; Check if the new package is critical and set the flag.
	ld a, [wMultiplayerQueuedPackage]
	cp MULTIPLAYER_PKG_NOOP
	jr nc, .not_critical

	ld a, 1
	ld [wMultiplayerHasQueuedCriticalPackage], a

.not_critical:
	call MultiplayerSendQueuedPackages

.done:
	pop de
	pop bc
	pop hl
	ret

; Send package (called once per frame)
; Moves queued package to buffered package if ready
MultiplayerSendQueuedPackages:
	; Check if we have a queued package
	ld a, [wMultiplayerHasQueuedPackage]
	and a
	ret z
	
	; Check if we're still waiting for ACK on buffered package
	ld a, [wMultiplayerHasBufferedPackage]
	and a
	ret nz
	
	; Get package type and size for the queued package
	ld a, [wMultiplayerQueuedPackage]
	call MultiplayerGetPackageSize
	ld [wMultiplayerBufferedPackageSize], a
	ld b, a
	
	; Copy only the actual package size to buffered package
	ld hl, wMultiplayerQueuedPackage
	ld de, wMultiplayerBufferedPackage
	ld bc, MULTIPLAYER_MAX_PACKAGE_SIZE
	rst CopyBytes
	
	; Set buffered flag, clear queued flag
	ld a, 1
	ld [wMultiplayerHasBufferedPackage], a
	xor a
	ld [wMultiplayerHasQueuedPackage], a
	ld [wMultiplayerHasQueuedCriticalPackage], a
	
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

	; Do not do anything if a package is waiting to be executed.
	; While waiting, the old rSB value will still be present
	; and we can just handle it on a later callback.
	ld a, [wMultiplayerHasPackageToExecute]
	and a
	ret nz

	push hl
	push bc
	push de
	
	; Read received byte from serial buffer and store in E for reuse
	ldh a, [rSB]
	ld e, a

	; Check if SB is floating ($FF indicates no connection)
	cp $FF
	jr z, .restart_own_package

	; Check if bit 7 is set (invalid - should always be 0)
	bit 7, a
	jr nz, .restart_own_package

	; Check if we received our own nibble FIRST (before duplicate detection)
	call AssertRemoteNibble
	
	; Normal processing - new nibble from remote
	jr .process_new_nibble

; .handle_duplicate:
; 	; Handle duplicate chunk from remote side
; 	; (We already know it's not our own chunk from the check above)
; 	call PrepareChunkToResend
; 	jr .send_chunk

.process_new_nibble:
	; First check for reset flag and handle it BEFORE nibble index validation
	call HandleResetFlag

	; Now check if N (nibble index) bit mismatches expected nibble type
	call IfNibbleIndexMismatchesExpected
	jr z, .restart_their_package

	; Handle the received nibble
	call HandleReceivedNibble

	; Only advance send state if we're actually sending a real package
	; Don't advance when just sending "filler" nibbles with reset flag
	ld a, [wMultiplayerHasBufferedPackage]
	and a
	jr z, .send_nibble	; No package = don't advance state
	
	; We have a real package - advance send state after successful reception
	call AdvanceSendState
	jr .send_nibble

.restart_own_package:
	; Other cartridge told us to restart our package, so we do that.
	
	; Reset send state variables to initial values
	xor a
	ld [wMultiplayerSendNibbleIdx], a
	ld [wMultiplayerSendByteIdx], a

	jr .send_nibble

.restart_their_package:
	; Received nibble index mismatches the expected one,
  ; so we tell them to restart their package.

	; Reset our own receive indices since we're out of sync with them
	xor a
	ld [wMultiplayerReceiveNibbleIdx], a
	ld [wMultiplayerReceiveByteIdx], a

	; Set the invalid flag to force the other side to restart
	; TODO: Use hardware constants
	ld a, %10000000
	ldh [rSB], a
	jr .start_transmission

.send_nibble:
	call PrepareNextNibble
	ldh [rSB], a

.start_transmission:
	; NOTE:
  ; Both master and slave need to set rSC's bit 7 for the transfer to occur
	ld a, [wMultiplayerIsMaster]
	and a
	jr z, .slave_transmission

	; We are master - start the serial transmission with internal clock
	ld a, %10000011	; Bit 7=1 (start transfer), Bit 0=1 (internal clock)
	ldh [rSC], a
	jr .cleanup

.slave_transmission:
	; We are slave - use external clock
	ld a, %10000010
	ldh [rSC], a
.cleanup:
	pop de
	pop bc
	pop hl
	ret

; Assert that we did NOT receive our own nibble (i.e., master should NOT see master bit, slave should NOT see slave bit)
; Input: E = received byte from rSB
; If the bit matches our role, desync immediately.
; TODO: This crash has never occurred, might be removed for performance.
; NOTE: This apparently only happens sometimes when the role is switched from slave to master,
; probably mid-transmission.
AssertRemoteNibble:
	ld a, [wMultiplayerIsMaster]
	add a, a
	add a, a
	add a, a
	add a, a
	add a, a
	add a, a
	xor e
	bit 6, a
	jr nz, .ok	; If bit 6 is different, this is a remote nibble (normal)
	ld a, ERR_EGG_SPECIES
	jmp Crash
.ok
	ret

; Check if nibble index bit mismatches expected nibble type
; Input: E = received byte from rSB
; Output: Z flag set if nibble index mismatches (desync), clear if matches
IfNibbleIndexMismatchesExpected:
	; Move received nibble index (bit 4 of E) to bit 0
	ld a, e
	swap a
	ld b, a ; Store swapped byte in B
	
	; Load expected nibble index (0 or 1)
	ld a, [wMultiplayerReceiveNibbleIdx]
	
	; Compare expected index (in A) with received index (in B).
	; The result of the comparison will be in bit 0 of A.
	; (bit 0 of [w...Idx]) XOR (bit 0 of swapped E)
	xor b
	
	; Isolate the result bit. A is 0 if they matched, 1 if they mismatched.
	; This sets the Z flag if A is 0 (a match).
	and 1
	
	; We want the opposite: Z set for mismatch, clear for match.
	; `dec a` flips the Z flag in this specific case.
	dec a ; If A was 0 (match), it becomes 255 (Z=0).
	      ; If A was 1 (mismatch), it becomes 0 (Z=1).
	ret


; Check if enough frames have passed since the last transmission.
; Input: None
; Output: Z flag set if ready for transmission, clear if not ready yet
; Destroys: A
; TODO: Conditionally call this based on a hardcoded compile flag
IfIsReady:
	; Decrement frame counter
	ld a, [wMultiplayerFrameCounter]
	dec a
	ld [wMultiplayerFrameCounter], a
	
	; If the counter is not zero yet, we are not ready.
	; The 'dec a' instruction clears the Z flag if the result is non-zero.
	ret nz
	
	; The counter has reached zero, so we are ready.
	; Reset the counter to the threshold value.
	ld a, MULTIPLAYER_IDLE_FRAMES
	ld [wMultiplayerFrameCounter], a
	
	; The Z flag is already set from the 'dec a' instruction that resulted in zero.
	ret

; VBlank interrupt handler for multiplayer
MultiplayerVBlankHandler::
	; Only process if multiplayer is enabled
	ld a, [wMultiplayerIsEnabled]
	and a
	ret z

	
  ; TODO: We should probably put this behind a compile flag
  ; to enable throttling only conditionally.
	call IfIsReady
	ret nz

	; Check if a new package is queued and the buffer is free.
	; This is done here to ensure it's handled cleanly between frames.
	call MultiplayerSendQueuedPackages

	jp MultiplayerSendReceiveNibble

; Handle reset flag in received nibble
; Input: E = received byte from rSB  
; Destroys: A
HandleResetFlag:
	bit 5, e
	ret z
	bit 4, e
  ; We desync here, because the reset flag is set (bit 5),
  ; but the nibble index isn't 0 (bit 4).
	jr nz, .reset_desync
	xor a
	ld [wMultiplayerReceiveByteIdx], a
	ld [wMultiplayerReceiveNibbleIdx], a
	ret
.reset_desync:
	call Desync
	ret

; Process a received nibble and assemble it into bytes/packages
; Input: E = received byte from rSB  
; Destroys: A, B, C, D, H, L
HandleReceivedNibble:
	; Extract 4-bit payload from bits 3-0
	ld a, e
	and %00001111
	ld b, a

	; Toggle the nibble index (0->1 or 1->0) and update the flags.
	; If the index *was* 1, it becomes 0, and the Z flag is SET.
	; If the index *was* 0, it becomes 1, and the Z flag is CLEAR.
	ld a, [wMultiplayerReceiveNibbleIdx]
	xor 1
	ld [wMultiplayerReceiveNibbleIdx], a

	; If Z flag is set, we just finished a low nibble.
	jr z, .was_low_nibble

.was_high_nibble:
	; The index was 0 (high nibble). Move payload to upper 4 bits and store.
	ld a, b
	swap a
	ld [wMultiplayerLastReceivedByte], a
	ret

.was_low_nibble:
	; The index was 1 (low nibble). Combine with the stored high nibble.
	ld a, [wMultiplayerLastReceivedByte]
	or b
	ld [wMultiplayerLastReceivedByte], a

	; Handle the now-completed byte.
	call HandleCompletedByte
	ret

; Handle a completed byte (both nibbles received)
; Destroys: A, B, C, D, H, L
HandleCompletedByte:
	; Store byte in receive package
	ld a, [wMultiplayerReceiveByteIdx]
	ld c, a ; Store current byte index in C
	ld b, 0
	ld hl, wMultiplayerReceivedPackage
	add hl, bc ; HL now points to correct byte in ram

	ld a, [wMultiplayerLastReceivedByte]
	ld [hl], a ; Store the completed byte

	; Now that the byte is stored, increment the index for the *next* byte
	ld a, c
	inc a
	ld [wMultiplayerReceiveByteIdx], a
	
	; If this was the first byte (index was 0), get the package size
	ld a, c
	and a
	jr nz, .check_completion

	; --- This was the first byte (index 0) ---
	ld a, [wMultiplayerLastReceivedByte] ; package type
	cp MULTIPLAYER_PKG_NOOP
	jr z, .noop_package
	call MultiplayerGetPackageSize
	ld [wMultiplayerReceivedPackageSize], a
	
.check_completion:
	; Check if we've completed a full package
	ld a, [wMultiplayerReceiveByteIdx] ; Use the *new* index (which is now number of bytes received)
	ld b, a
	ld a, [wMultiplayerReceivedPackageSize] 
	cp b ; Compare number of bytes received with expected size
	ret nz ; Not complete yet

.package_complete:
	; Set the flag to indicate a package is ready to be executed
	ld a, 1
	ld [wMultiplayerHasPackageToExecute], a

	; Copy received package to execution buffer
	ld hl, wMultiplayerReceivedPackage
	ld de, wMultiplayerPackageToExecute
	ld bc, MULTIPLAYER_MAX_PACKAGE_SIZE
	rst CopyBytes
	
	; Call package received handler
	jp MultiplayerOnPackageReceived

.noop_package:
	; A NOOP package was received. We don't need to do anything with it,
	; so we just reset the receive indices and continue.
	xor a
	ld [wMultiplayerReceiveByteIdx], a
	ld [wMultiplayerReceiveNibbleIdx], a
	ret

; Advance send state machine after successful reception
; Progresses through: nibble -> byte -> package
; Destroys: A, B, C
AdvanceSendState:
	jp AdvanceToNextNibble

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
	jp AdvanceToNextByte

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
	
	; Check if we've completed the entire package (use actual size)
	ld a, [wMultiplayerSendByteIdx]  ; Re-load the incremented value
	ld b, a
	ld a, [wMultiplayerBufferedPackageSize]
	cp b
	jr c, .package_complete ; if size < index, something is wrong, but complete anyway
	ret nz ; if size > index, return. if size == index, fall through

; .error:
;   ld a, ERR_NEWBOX
; 	jmp Crash

.package_complete:
	; Package complete! Reset state and call completion handler
	xor a
	ld [wMultiplayerSendByteIdx], a
	ld [wMultiplayerHasBufferedPackage], a

	; The buffered package has been fully sent.
	; We now return to the main loop, which will handle sending NOOPs
	; or starting a new package transmission in a subsequent frame.
	ret

.no_package:
	; No package to send, but byte index was already incremented
	; This allows continuous transmission when no package is buffered
	ret

; Prepare the next nibble for transmission
; Output: A = prepared byte ready for rSB
; Destroys: A, B, C, H, L
PrepareNextNibble:
	; === Part 1: Get the 4-bit payload and store it in B ===
	ld a, [wMultiplayerHasBufferedPackage]
	and a
	jr z, .no_package
	
	; We have a package, get the correct byte from the buffer.
	ld a, [wMultiplayerSendByteIdx]
	ld c, a
	ld b, 0
	ld hl, wMultiplayerBufferedPackage
	add hl, bc      ; HL points to the current byte in the package
	ld a, [hl]      ; Load the byte
	jr .extract_nibble

.no_package:
  ; This is why MULTIPLAYER_PKG_NOOP exists,
  ; otherwise sending the 0x00 package (for example "get name request")
  ; might produce an endless loop.
	ld a, MULTIPLAYER_PKG_NOOP

.extract_nibble:
	; A contains the full source byte. Extract the correct nibble.
	ld c, a
	ld a, [wMultiplayerSendNibbleIdx]
	and a
	jr z, .get_high_nibble

.get_low_nibble:
	; Extract low nibble (bits 3-0)
	ld a, c
	and %00001111
	jr .loaded_payload

.get_high_nibble:
	; Extract high nibble (bits 7-4) and shift it down
	ld a, c
	swap a
	and %00001111

.loaded_payload:
	; The 4-bit payload (%0000PPPP) is now in A. Save it in B.
	ld b, a

	; === Part 2: Build the metadata and combine with the payload ===
	; Start with Bit 6: Master/Slave bit
	ld a, [wMultiplayerIsMaster]
	rlca            ; A = %000000M0
	ld c, a         ; Store intermediate result in C

	; OR in Bit 5: Reset bit
	call ShouldSetResetBit ; A = %0000000R
	or c            ; A = %000000MR

	; OR in Bit 4: Nibble Index bit
	rlca            ; A = %00000MR0
	ld c, a         ; Store intermediate result in C
	ld a, [wMultiplayerSendNibbleIdx] ; A = %0000000I
	or c            ; A = %00000MRI

	; We now have the metadata nibble in the lower 4 bits of A.
	; Shift it to the upper 4 bits.
	swap a          ; A = %0MRI0000

	; Finally, combine with the payload we saved in B.
	or b            ; A = %0MRIPPPP
	
	ret

; Check if we should set the reset bit (start of new package)
; Output: A = 1 if reset bit should be set, 0 otherwise
; Destroys: A, L
ShouldSetResetBit:
	; The reset bit is set if we are at the very start of a package
	; (both send indices are 0), OR if there is no package buffered at all.
	
	; First, check if both send indices are 0, using L as a temporary register.
	ld a, [wMultiplayerSendByteIdx]
	ld l, a
	ld a, [wMultiplayerSendNibbleIdx]
	or l ; A is 0 if and only if both indices were 0
	jr z, .set_reset ; If so, we must set the reset bit.
	
	; If we are not at the start of a package, the reset bit is only set
	; if there is no package currently buffered.
	ld a, [wMultiplayerHasBufferedPackage]
	and a ; Sets Z flag if a is 0.
	jr z, .set_reset ; If no package, set the reset bit.
	
	; If we reach here, there is a package and we are not at the start.
	; So, we clear the reset bit.
.clear_reset:
	xor a ; Sets A to 0 and sets the Z flag.
	ret

.set_reset:
	ld a, 1 ; Sets A to 1.
	ret

; Handle when a complete package has been received
; Destroys: A, H, L
MultiplayerOnPackageReceived:
	; Clear the received package buffer
	ld hl, wMultiplayerReceivedPackage
	ld bc, MULTIPLAYER_MAX_PACKAGE_SIZE ; Clear max size
	xor a
	rst ByteFill

	; Reset received package size for the next package
	xor a
	ld [wMultiplayerReceivedPackageSize], a

	; Check package type
	ld hl, wMultiplayerPackageToExecute
	ld a, [hl]
	cp MULTIPLAYER_PKG_INIT
	jr z, .handle_init
	cp MULTIPLAYER_PKG_NAME
	jr z, .handle_name
	cp MULTIPLAYER_PKG_NOOP
	jr z, .mark_package_as_executed
	cp MULTIPLAYER_PKG_PHONECALL
	jr z, .handle_package_lazily
	cp MULTIPLAYER_PKG_SEND_POSITION
	jr z, .handle_package_lazily
	cp MULTIPLAYER_PKG_GIFT_ITEM
	jr z, .handle_package_lazily

	; Fallback for unknown packages
	call PrintTextLazy ; TODO: Remove this debug output
	jr .mark_package_as_executed

.handle_init:
	; The other player has declared themselves master.
	; We are now a forced slave and cannot become master,
	; otherwise things would break.
	ld a, 1
	ld [wMultiplayerIsForcedSlave], a

	; We must respond with our name.
	call MultiplayerSendName
	jr .mark_package_as_executed

.handle_name:
	; Store the other trainer's name
	ld hl, wMultiplayerPackageToExecute + 1
	ld de, wMultiplayerOtherTrainerName
	ld bc, PLAYER_NAME_LENGTH ; Copy 8 bytes of the name (7 chars + terminator)
	rst CopyBytes

	ld a, 1
	ld [wMultiplayerHasOtherTrainerName], a

	; If we are master, we must respond with our name.
	; If we are slave, we do nothing to prevent a name-sending loop.
	ld a, [wMultiplayerIsMaster]
	and a
	jr z, .mark_package_as_executed ; Not master, so we're done

	; We are master, so we send our name.
	call MultiplayerSendName
	jr .mark_package_as_executed

.handle_package_lazily:
	ld a, 1
	ld [wMultiplayerHasPackageToExecuteLazily], a
	ld hl, wMultiplayerPackageToExecute
	ld de, wMultiplayerPackageToExecuteLazily
	ld bc, MULTIPLAYER_MAX_PACKAGE_SIZE
	rst CopyBytes
	jr .mark_package_as_executed

.mark_package_as_executed:
	xor a
	ld [wMultiplayerHasPackageToExecute], a
	ret

; Prepares and queues a package containing the player's name.
MultiplayerSendName:
	ld hl, wMultiplayerTempPackage
	ld bc, MULTIPLAYER_MAX_PACKAGE_SIZE
	xor a
	rst ByteFill

	ld a, MULTIPLAYER_PKG_NAME
	ld [wMultiplayerTempPackage], a

	ld hl, wPlayerName
	ld de, wMultiplayerTempPackage + 1
	ld bc, PLAYER_NAME_LENGTH
	rst CopyBytes

	ld hl, wMultiplayerTempPackage
	jp MultiplayerQueuePackage

Desync:
  ; Something went catastrophically wrong in the multiplayer protocol.
	ld a, ERR_MULTIPLAYER_DESYNC
	jmp Crash
