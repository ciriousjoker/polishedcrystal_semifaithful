; Frame-based multiplayer communication system
; Full-duplex, piggy-back-ACK link protocol
; Each frame processes one nibble with sequence/ACK bits

SECTION "Frame Multiplayer", ROMX

; SB Register bit layout (7 6 5 4 3 2 1 0):
; Bit 7: SeqOut - Toggle bit for sequence control
; Bit 6: AckOut - Mirror of last received SeqIn
; Bit 5: H/L - 1=High nibble, 0=Low nibble
; Bit 4: Master/Slave - 1 if master, 0 if slave (used to avoid interpreting own nibbles as received ones)
; Bits 3-0: Payload nibble

DEF MULTIPLAYER_PACKAGE_SIZE EQU 8
DEF MULTIPLAYER_TIMEOUT_FRAMES EQU 60

; MultiplayerInitialize:
; Purpose: Initializes all multiplayer-related RAM variables and hardware registers to a clean state.
;          This should be called once when activating the multiplayer feature.
MultiplayerInitialize::
	; Clear all package buffers to 0.
	ld hl, wMultiplayerQueuedPackage
	ld bc, wTempBuffer - wMultiplayerQueuedPackage ; Zero out all multiplayer WRAM variables up to the temp buffer
	xor a
	rst ByteFill

	; Reset state variables to their initial values.
	xor a
	ld [wMultiplayerSendSeq], a
	ld [wMultiplayerRecvSeq], a
	ld [wMultiplayerLastSB], a ; Using 0 as initial "last SB" is fine, as valid SB is never 0.

	; Reset serial registers to a known, safe state.
	ld a, $FF
	ldh [rSB], a ; $FF is a safe value indicating no connection.
	xor a
	ldh [rSC], a ; Clear serial control register.
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
	ld [wMultiplayerQueuedPackageFlag], a
	
	; Debug output
	call OpenText
	ld hl, .QueuedPackageText
	call PrintText
	call CloseText

	call MultiplayerSendPackage
	
	pop de
	pop bc
	pop hl
	ret

.QueuedPackageText:
	text "Queued package!"
	done

; Send package (called once per frame)
; Moves queued package to buffered package if ready
MultiplayerSendPackage::
	; Check if we have a queued package
	ld a, [wMultiplayerQueuedPackageFlag]
	and a
	ret z
	
	; Check if we're still waiting for ACK on buffered package
	ld a, [wMultiplayerBufferedPackageFlag]
	and a
	ret nz
	
	; Copy queued package to buffered package
	ld hl, wMultiplayerQueuedPackage
	ld de, wMultiplayerBufferedPackage
	ld bc, MULTIPLAYER_PACKAGE_SIZE
	rst CopyBytes
	
	; Set buffered flag, clear queued flag
	ld a, 1
	ld [wMultiplayerBufferedPackageFlag], a
	xor a
	ld [wMultiplayerQueuedPackageFlag], a
	
	; Reset send indices
	xor a
	ld [wMultiplayerSendByteIdx], a
	ld [wMultiplayerSendNibbleIdx], a
	ld [wMultiplayerTimeoutCounter], a
	
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

	; ; Check if the byte in the serial buffer is an echo of our own last transmission.
	; ; If so, we must ignore it and do nothing this frame.
	; call IfSBContainsOwnNibble
	; ret c ; Return if Carry is set (it was our own nibble).

	push hl
	push bc
	push de
	
	; 1) Process incoming data
	call ProcessIncomingNibble
	
	; ld a, [wMultiplayerIsMaster]
	; and a
	; jr z, .master_mode

  ; NOTE: For now, we only send stuff on slave

	; 2) Handle ACK/timeout for outgoing nibble
	call ProcessOutgoingAck
	
	; 3) Prepare and send next nibble
	call PrepareNextNibble
; .master_mode:
	pop de
	pop bc
	pop hl
	ret

; Process incoming nibble from partner
ProcessIncomingNibble:
	; Discard if SB is 0xFF (invalid value, no link connected)
	ldh a, [rSB]
	cp $FF
	ret z
	ld b, a
	; Check if this is different from last received SB
	ld a, [wMultiplayerLastSB]
	cp b
	ret z  ; Same as last time, ignore

	; Store new SB value
	ld a, b
	ld [wMultiplayerLastSB], a

	; Extract H/L bit (bit 5)
	ld a, b
	and %00100000
	jr z, .low_nibble
	
	; Check if we expect high nibble
	ld a, [wMultiplayerReceiveNibbleIdx]
	and a
	ret nz  ; Expected low nibble but got high
	jr .process_nibble
	
.low_nibble:
	; Check if we expect low nibble
	ld a, [wMultiplayerReceiveNibbleIdx]
	and a
	ret z  ; Expected high nibble but got low

.process_nibble:
	; Extract sequence bit (bit 7)
	ld a, b
	and %10000000
	jr z, .seq_zero
	ld a, 1
	jr .check_seq
.seq_zero:
	xor a
	
.check_seq:
	ld c, a
	ld a, [wMultiplayerRecvSeq]
	cp c
	ret nz  ; Wrong sequence, ignore
	
	; Extract payload nibble (bits 3-0)
	ld a, b
	and %00001111
	ld c, a
	
	; Store nibble based on H/L
	ld a, [wMultiplayerReceiveNibbleIdx]
	and a
	jr nz, .store_low_nibble
	
	; Store high nibble
	ld a, [wMultiplayerReceiveByteIdx]
	cp MULTIPLAYER_PACKAGE_SIZE
	ret nc  ; Buffer full
	
	ld l, a
	ld h, 0
	ld de, wMultiplayerReceivedPackage
	add hl, de
	
	ld a, c
	swap a  ; Move to high nibble
	ld [hl], a
	
	jr .advance_nibble
	
.store_low_nibble:
	; Store low nibble
	ld a, [wMultiplayerReceiveByteIdx]
	cp MULTIPLAYER_PACKAGE_SIZE
	ret nc  ; Buffer full
	
	ld l, a
	ld h, 0
	ld de, wMultiplayerReceivedPackage
	add hl, de
	
	ld a, [hl]
	or c  ; OR with low nibble
	ld [hl], a
	
	; Advance to next byte
	ld hl, wMultiplayerReceiveByteIdx
	inc [hl]
	
.advance_nibble:
	; Toggle receive sequence
	ld hl, wMultiplayerRecvSeq
	ld a, [hl]
	xor 1
	ld [hl], a
	
	; Toggle nibble index
	ld hl, wMultiplayerReceiveNibbleIdx
	ld a, [hl]
	xor 1
	ld [hl], a
	
	; Check if package is complete
	ld a, [wMultiplayerReceiveByteIdx]
	cp MULTIPLAYER_PACKAGE_SIZE
	ret nz
	
	; Package complete, handle it
	call MultiplayerOnPackageReceived
	ret

; Process ACK for outgoing nibble and handle timeout
ProcessOutgoingAck:
	; Check if we have a buffered package
	ld a, [wMultiplayerBufferedPackageFlag]
	and a
	ret z
	
	; Get ACK bit (bit 6 of received SB)
	ldh a, [rSB]
	and %01000000
	jr z, .ack_zero
	ld a, 1
	jr .check_ack
.ack_zero:
	xor a
	
.check_ack:
	ld b, a
	ld a, [wMultiplayerSendSeq]
	cp b
	jr nz, .no_ack
	
	; ACK received, advance to next nibble
	ld hl, wMultiplayerSendNibbleIdx
	inc [hl]
	ld a, [hl]
	cp 2
	jr nz, .reset_timeout
	
	; Completed both nibbles of a byte
	xor a
	ld [hl], a  ; Reset nibble index
	
	ld hl, wMultiplayerSendByteIdx
	inc [hl]
	
	; Check if package is complete
	ld a, [hl]
	cp MULTIPLAYER_PACKAGE_SIZE
	jr nz, .toggle_seq
	
	; Package complete, clear buffered flag
	xor a
	ld [wMultiplayerBufferedPackageFlag], a
	
.toggle_seq:
	; Toggle send sequence
	ld hl, wMultiplayerSendSeq
	ld a, [hl]
	xor 1
	ld [hl], a
	
.reset_timeout:
	; Reset timeout counter
	xor a
	ld [wMultiplayerTimeoutCounter], a
	ret
	
.no_ack:
	; No ACK, increment timeout
	ld hl, wMultiplayerTimeoutCounter
	inc [hl]
	ld a, [hl]
	cp MULTIPLAYER_TIMEOUT_FRAMES
	ret c
	
	; Timeout reached, clear buffered package
	xor a
	ld [wMultiplayerBufferedPackageFlag], a
	ld [wMultiplayerTimeoutCounter], a
	
  ; TODO: Check why this timeout occurs
  ; call PrintTextLazy
	; ; Debug output
	; call OpenText
	; ld hl, .TimeoutText
	; call PrintText
	; call CloseText
	
	ret

; .TimeoutText:
; 	text "Package timeout!"
; 	done

; Prepare next nibble for transmission
PrepareNextNibble:
	; Check if we have a buffered package
	ld a, [wMultiplayerBufferedPackageFlag]
	and a
	jr z, .idle_nibble
	
	; Get current byte from buffered package
	ld a, [wMultiplayerSendByteIdx]
	ld l, a
	ld h, 0
	ld de, wMultiplayerBufferedPackage
	add hl, de
	ld a, [hl]  ; Current byte
	
	; Extract nibble based on send nibble index
	ld b, a
	ld a, [wMultiplayerSendNibbleIdx]
	and a
	jr nz, .low_nibble_out
	
	; High nibble
	ld a, b
	swap a
	and %00001111
	ld c, a
	ld a, %00100000  ; H/L bit = 1
	jr .build_sb
	
.low_nibble_out:
	; Low nibble
	ld a, b
	and %00001111
	ld c, a
	xor a  ; H/L bit = 0
	jr .build_sb
	
.idle_nibble:
	; No package, send idle nibble
	ld c, 0  ; Payload = 0
	xor a    ; H/L bit = 0
	
.build_sb:
	ld b, a  ; Save H/L bit
	
	; Build SB register value
	; Bit 7: SendSeq
	ld a, [wMultiplayerSendSeq]
	and a
	jr z, .seq_bit_clear
	ld a, %10000000
	jr .add_ack
.seq_bit_clear:
	xor a
	
.add_ack:
	ld d, a  ; Save seq bit
	
	; Bit 6: AckOut (mirror of last received SeqIn)
	ld a, [wMultiplayerRecvSeq]
	and a
	jr z, .ack_bit_clear
	ld a, %01000000
	jr .combine_bits
.ack_bit_clear:
	xor a
	
.combine_bits:
	or d  ; Combine with seq bit
	or b  ; Combine with H/L bit
	or c  ; Combine with payload nibble
	
	; Bit 4: Master/Slave
	ld e, a
	ld a, [wMultiplayerIsMaster]
	and a
	ld a, e
	jr z, .master_bit_done
	or %00010000
.master_bit_done:

	; Write to SB register
	ldh [rSB], a
	
	; Start transmission if master
	ld a, [wMultiplayerIsMaster]
	and a
	jr z, .slave_mode
	
	; Master mode: start internal clock
	ld a, %10000001
	ldh [rSC], a
	ret
	
.slave_mode:
	; Slave mode: wait for master clock
	xor a
	ldh [rSC], a
	ret

; Handle received complete package
MultiplayerOnPackageReceived:
	; Copy received package to execute buffer
	ld hl, wMultiplayerReceivedPackage
	ld de, wMultiplayerPackageToExecute
	ld bc, MULTIPLAYER_PACKAGE_SIZE
	rst CopyBytes
	
	; Reset receive indices
	xor a
	ld [wMultiplayerReceiveByteIdx], a
	ld [wMultiplayerReceiveNibbleIdx], a
	
	; Handle the package
	call HandleMultiplayerPackage
	ret

; Checks if the rSB register currently holds a nibble sent by this device.
; This is determined by comparing our own Master/Slave role with Bit 4 of the rSB byte.
; Sets the Carry Flag (C=1) if rSB contains our own nibble.
; Clears the Carry Flag (C=0) otherwise.
IfSBContainsOwnNibble:
	; Load the byte from the serial buffer and our current role.
	ldh a, [rSB]
	ld b, a ; b holds the byte from rSB.
	ld a, [wMultiplayerIsMaster]
	ld c, a ; c holds our role (1 for Master, 0 for Slave).

	; Check Bit 4 of the rSB byte to see who sent it.
	bit 4, b
	; The Zero Flag (Z) is now set if Bit 4 was 0 (sent by a Slave).
	; The Zero Flag (Z) is clear if Bit 4 was 1 (sent by a Master).

	; Case 1: Check if we are the Master.
	ld a, c
	and a ; This checks if a is 0 without changing it.
	jr z, .we_are_slave_check ; If a is 0, we are the Slave.

	; We are the Master (our role is 1).
	; If the sender was also a Master (Bit 4 was 1 -> Z flag is clear), it's our own nibble.
	jr z, .not_own_nibble ; If Z is set, sender was Slave. Not ours.
	jr .is_own_nibble     ; If Z is clear, sender was Master. It's ours.

.we_are_slave_check:
	; We are the Slave (our role is 0).
	; If the sender was also a Slave (Bit 4 was 0 -> Z flag is set), it's our own nibble.
	jr z, .is_own_nibble ; If Z is set, sender was Slave. It's ours.
	; Fallthrough to .not_own_nibble if Z is clear (sender was Master).

.not_own_nibble:
	xor a ; A common way to clear the Carry Flag.
	ret   ; Return with C=0.

.is_own_nibble:
	scf   ; Set the Carry Flag.
	ret   ; Return with C=1.

; Process received package
HandleMultiplayerPackage:
	call OpenText
	ld hl, .ReceivedPackageText
	call PrintText
	
	; Display package contents as hex
	ld hl, wMultiplayerPackageToExecute
	ld b, MULTIPLAYER_PACKAGE_SIZE
.display_loop:
	push bc
	push hl
	
	ld a, [hl]
	ld hl, .HexText
	call PrintText
	
	pop hl
	pop bc
	inc hl
	dec b
	jr nz, .display_loop
	
	call CloseText
	
	; Clear package buffer
	ld hl, wMultiplayerPackageToExecute
	ld bc, MULTIPLAYER_PACKAGE_SIZE
	xor a
	rst ByteFill
	
	ret

.ReceivedPackageText:
	text "Received package:"
	done

.HexText:
	text " @"
	done

; VBlank interrupt handler for multiplayer
MultiplayerVBlankHandler::
	; Only process if multiplayer is initialized
	ld a, [wMultiplayerIsMaster]
	cp $FF  ; Check if uninitialized
	ret z
	
	; Call frame-based multiplayer functions
	; call MultiplayerSendPackage
  ; TODO: Check this later
	call MultiplayerSendReceiveNibble
	ret
