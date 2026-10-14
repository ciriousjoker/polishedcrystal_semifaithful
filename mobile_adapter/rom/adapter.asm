SECTION "Mobile Adapter", ROMX, BANK[MOBILE_ROM_BANK]
MobileCodeStart::
INCLUDE "mobile_adapter/rom/irq_helpers.asm"

MobileTickCredit::
; Dense presentation may replenish frame credit without parsing a packet first.
; A pending frame tick remains available to the next ordinary transport pump.
	push af
	ld a, 1
	jr MobileTick.credit_mode
MobileTick::
	push af
	xor a
.credit_mode
	ld [wMobileVirtualAttempts], a
	push bc
	push de
	push hl
	ldh a, [rWBK]
	push af
	ld a, 1
	ldh [rWBK], a
	ldh a, [hCGB]
	cp 1
	jp nz, .return
	ldh a, [hVBlankCounter]
	ld hl, wMobileLastFrame
	cp [hl]
	jr z, .same_frame
	ld [hl], a
; Only an early native wait has room for a larger byte budget. Busy maps retain
; the cadence-tested default; the per-byte LY deadline remains authoritative.
	ldh a, [rLY]
	cp 96
	ld a, MOBILE_BURST_BYTES
	jr nc, .byte_budget
	ld a, 32
.byte_budget
	ld [wMobileBurst], a
	ld a, MOBILE_PUMPS_PER_FRAME
	ld [wMobilePumpCount], a
	ld a, 1
	ld [wMobileFrameTick], a
	jr .new_frame
.same_frame
	jr .key
.new_frame
	ld hl, wMobileKeepalive
	ld a, [hl]
	and a
	jr z, .send_cooldown
	dec [hl]
.send_cooldown
	ld hl, wMobileSendCooldown
	ld a, [hl]
	and a
	jr z, .cooldown
	dec [hl]
.cooldown
	ld a, [wMobileOwner]
	and a
	jr nz, .key
	ld hl, wMobileCooldown
	ld a, [hl]
	and a
	jr z, .key
	dec [hl]
.key
	ld a, [wMobileVirtualAttempts]
	and a
	jp nz, .return
.pump
	ld a, [wMobileKeyLatch]
	and a
	call nz, MobileConsumeKey ; consume the entire held chord, including later frames
	ld a, [wMobileTransferActive]
	and a
	jp nz, .return ; in-flight bytes do not consume foreground state-transition credit
	ld hl, wMobilePumpCount
	ld a, [hl]
	and a
	jp z, .return
	dec [hl]
	ld a, [wMobileOwner]
	and a
	jr nz, .owned
	ld a, [wMobileWanted]
	and a
	jp z, .return
	ld a, [wMobileCooldown]
	and a
	jp nz, .return
	ld a, [wMapStatus]
	cp MAPSTATUS_HANDLE
	jp nz, .return
	ldh a, [hInMenu]
	and a
	jp nz, .return
	call MobileSceneIsSafe
	jp nz, .return
	call MobileBegin
	jp .return
.owned
	ld a, 1
	ldh [rWBK], a
; Existing link/printer state must not share the serial owner.
	ld a, [wLinkMode]
	and a
	jr nz, .busy
	ld a, [wPrinterConnectionOpen]
	bit PRINTER_CONNECTION_OPEN, a
	jr z, .deadline
.busy
	xor a
	ld [wMobileWanted], a
	ld a, MOBILE_ERROR_BUSY
	call MobileFail
	jp .return
.deadline
	ld a, [wMobileOnline]
	and a
	jr nz, .active
	ld a, [wMobileFrameTick]
	and a
	jr z, .active
	ld hl, wMobileAttemptTicks
	call MobileDecrement
	jp z, .timeout
.active
	ld a, 1
	ld [wMobileAvailability], a
	ld a, [wMobileMenuState]
	and a
	jr nz, .paused
	ldh a, [hInMenu]
	and a
	jr nz, .paused
	call MobileSceneIsSafe
	jr nz, .paused
	ld a, [wMapStatus]
	cp MAPSTATUS_HANDLE
	jr nz, .paused
	ld a, [wBattleMode]
	and a
	jr z, .available
.paused
	xor a
	ld [wMobileAvailability], a
	call MobileFrozenOverworld
	jr z, .available
	farcall MobileNativeInvalidateHash
	xor a
	ld [wMobileAvailability], a
	ld [wMobileLocalValid], a
	ld [wMobileGraphicsReady], a
.available
	ld a, [wMobileLocalValid]
	and a
	jr nz, .fingerprint_ready
	ld [wMobileAvailability], a ; do not publish a previous map while hashing
.fingerprint_ready
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	call MobileCheckGuards
	jp nz, .packet_error
	ld a, [wMobileWire]
	and a
	jr z, .prepare
	ld hl, wMobileDeadline
	ld a, [wMobileFrameTick]
	and a
	jr z, .wire_state
	dec [hl]
	jp z, .timeout
.wire_state
	ld a, [wMobileWire]
	cp MOBILE_WIRE_APP
	jr z, .app_continue
	cp MOBILE_WIRE_TX_DONE
	jr z, .request_done
	cp MOBILE_WIRE_DONE
	jr z, .response_done
	cp MOBILE_WIRE_BAD
	jp z, .packet_error
	ld a, [wMobileTransferActive]
	and a
	jp nz, .return
	call MobileClock
	jp .return
.prepare
; IRQs may have completed a byte since the outer admission check. Check the
; budget again after choosing the actual expensive state, before touching it.
	ldh a, [rLY]
	cp 130
	jp nc, .return
	call MobilePrepareCommand
	jp .return
.app_continue
; IRQs may have completed a byte since the outer admission check. Check the
; budget again after choosing the actual expensive state, before touching it.
	ldh a, [rLY]
	cp 130
	jp nc, .return
	call MobileAppendPending
	jp c, .protocol_error
	ld a, [wMobileWire]
	cp MOBILE_WIRE_APP
	jp z, .return
	jr .command_complete
.request_done
; In 8-bit full duplex, the two ACKs arrive on the first trailing clocks.
	ld a, [wMobileTxSize]
	sub 4
	ld e, a
	ld d, 0
	ld hl, wMobileRx
	add hl, de
	ld a, [hli]
	cp MOBILE_DEVICE
	jr nz, .adapter_error
	ld a, [wMobileCommand]
	xor $80
	cp [hl]
	jr nz, .adapter_error
	ld a, MOBILE_WIRE_WAIT
	ld [wMobileWire], a
	ld hl, wMobileRx
	call MobileSetRxPointer
	call MobileClock
	jr .return
.response_done
; IRQs may have completed a byte since the outer admission check. Check the
; budget again after choosing the actual expensive state, before touching it.
	ldh a, [rLY]
	cp 130
	jp nc, .return
	call MobileValidateResponseSlice
	jp c, .packet_error
	jp z, .return ; checksum work resumes in a later bounded foreground pump
	ld a, [wMobileRx + 2]
	cp $ee ; adapter command error, not application data
	jr z, .adapter_error_detail
	ld a, [wMobileCommand]
	or $80
	ld hl, wMobileRx + 2
	cp [hl]
	jr nz, .packet_error
	call MobileConsumeResponse
	jr c, .protocol_error
	ld a, [wMobileWire]
	cp MOBILE_WIRE_APP
	jr z, .return
.command_complete
	ld a, [wMobileOwner]
	and a
	jr z, .return
	xor a
	ld [wMobileWire], a
	; Keep response parsing and the next request's checksum/copy in separate
	; foreground pumps. The next MobileTick starts the request without waiting.
	jr .return
.adapter_error_detail
	ld a, [wMobileRx + 7]
	ld [wMobileAdapterError], a
.adapter_error
	ld a, MOBILE_ERROR_ADAPTER
	jr .fail
.protocol_error
	ld a, MOBILE_ERROR_PROTOCOL
	jr .fail
.packet_error
	ld a, MOBILE_ERROR_PACKET
	jr .fail
.timeout
	ld a, MOBILE_ERROR_TIMEOUT
.fail
	call MobileFail
.return
	ld a, [wMobileVirtualAttempts]
	and a
	jr nz, .frame_pending
	xor a
	ld [wMobileFrameTick], a
.frame_pending
	ld a, [wMobileEmergencyPending]
	and a
	jr z, .restore_registers
	xor a
	ld [wMobileWanted], a
	ld [wMobileOnline], a
	ld [wMobilePeerCount], a
	ld a, [wMobileOwner]
	and a
	call nz, MobileRelease
.restore_registers
	pop af
	ldh [rWBK], a
	pop hl
	pop de
	pop bc
	pop af
	ret

; Filter mirrored game input, preserving physical input for held-key debounce.
; MobileMenuInput pumps again after GetJoypad, before ordinary player events.
MobileConsumeKey:
	ldh a, [hJoyDown]
	and ~(PAD_SELECT | PAD_B)
	ldh [hJoyDown], a
	ldh a, [hJoyPressed]
	and ~(PAD_SELECT | PAD_B)
	ldh [hJoyPressed], a
	ret

; Cable/printer foreground entry points yield the serial port before touching
; their registers. This also covers paths that do not wait for another frame.
MobileYieldSerial::
	push af
	push bc
	xor a
	ld [wMobileWanted], a
	ld a, [wMobileOwner]
	and a
	jr z, .done
	ld a, MOBILE_ERROR_BUSY
	call MobileFail
.done
	pop bc
	pop af
	ret

MobileBegin:
	ld a, [wMobileEmergencyPending]
	and a
	ret nz
	ld a, [wLinkMode]
	and a
	ret nz
	ld a, [wPrinterConnectionOpen]
	bit PRINTER_CONNECTION_OPEN, a
	ret nz
	ldh a, [rSC]
	bit 7, a
	ret nz
	call MobileInstallIRQSlice
	ret nc ; serial remains unclaimed until the protected image is complete
	ldh a, [rSC]
	ld [wMobileSavedSC], a
	ldh a, [rSB]
	ld [wMobileSavedSB], a
	ldh a, [rIE]
	and IE_SERIAL
	ld [wMobileSavedSerialIE], a
	; Copy before claiming serial. The fixed WRAM0 trampoline is separate
	; from save scratch. IME stays disabled throughout the byte handler.
	ld hl, MobileIRQTrampolineSource
	ld de, wMobileIRQTrampoline
	ld bc, MOBILE_IRQ_TRAMPOLINE_SIZE
	rst CopyBytes
	ld hl, wMobileAttemptCount
	inc [hl]
	ld hl, wMobileSequence
	inc [hl]
	jr nz, .sequence
	inc hl
	inc [hl]
.sequence
	xor a
	ld [wMobileWire], a
	ld [wMobileTransferActive], a
	ld [wMobileStep], a
	ld [wMobileAppCount], a
	ld [wMobilePingSent], a
	ld [wMobileError], a
	ld [wMobileAdapterError], a
	ld [wMobileServerError], a
	ld [wMobileOnline], a
	ld [wMobilePeerCount], a
	ld [wMobilePhase], a
	ld [wMobileSendCooldown], a
	ld [wMobileKeepalive], a
	ld a, 1
	ld [wMobileNativeActorMode], a ; identify the experimental path once per join
	ld a, LOW(MOBILE_ATTEMPT_TICKS)
	ld [wMobileAttemptTicks], a
	ld a, HIGH(MOBILE_ATTEMPT_TICKS)
	ld [wMobileAttemptTicks + 1], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, $a5
	ld [wMobileGuardStart], a
	ld [wMobileGuardTx], a
	ld [wMobileGuardRx], a
	ld [wMobileGuardEnd], a
	ld a, [wMobileMode]
	and a
	jr nz, .hello
	ld [wMobileWanted], a ; consume one diagnostic request, never auto-repeat it
	jr .claim
.hello
	call MobileBuildHello
.claim
; Claim/clear serial atomically. Other interrupt-enable bits are preserved.
	di
	ld a, 1
	ld [wMobileOwner], a
	ld a, 2
	ld [wMobileKeyLatch], a
	ldh a, [rIF]
	and ~IE_SERIAL
	ldh [rIF], a
	ldh a, [rIE]
	or IE_SERIAL
	ldh [rIE], a
	ei
	ret

; Install at most 16 bytes per early idle call, before acquiring serial.
; Fresh joins rewrite the complete image, including after a restored savestate.
MobileInstallIRQSlice:
	ldh a, [rLY]
	cp 132
	ret nc
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileIRQInstallOffset]
	ld c, a
	ld a, [wMobileIRQInstallOffset + 1]
	ld b, a
	ld hl, MobileIRQImageSource
	add hl, bc
	push hl
	ld hl, MobileIRQRuntimeStart
	add hl, bc
	ld d, h
	ld e, l
	ld hl, MobileIRQImageEnd - MobileIRQImageSource
	ld a, l
	sub c
	ld l, a
	ld a, h
	sbc b
	ld h, a
	jr c, .restart
	or l
	jr z, .restart
	ld a, h
	and a
	ld a, 16
	jr nz, .chunk
	ld a, l
	cp 16
	jr c, .chunk
	ld a, 16
.chunk
	ld c, a
	ld b, 0
	ld hl, wMobileIRQInstallOffset
	add [hl]
	ld [hli], a
	jr nc, .offset
	inc [hl]
.offset
	pop hl
	rst CopyBytes
	ld a, [wMobileIRQInstallOffset]
	cp LOW(MobileIRQImageEnd - MobileIRQImageSource)
	jr nz, .pending
	ld a, [wMobileIRQInstallOffset + 1]
	cp HIGH(MobileIRQImageEnd - MobileIRQImageSource)
	jr nz, .pending
	xor a
	ld [wMobileIRQInstallOffset], a
	ld [wMobileIRQInstallOffset + 1], a
	scf
	ret
.restart
	pop hl
	xor a
	ld [wMobileIRQInstallOffset], a
	ld [wMobileIRQInstallOffset + 1], a
.pending
	and a
	ret

MobileIRQTrampolineSource:
	ldh a, [rWBK]
	push af
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	call MobileSerialIRQ
	pop af
	ldh [rWBK], a
	jp Serial.end
MobileIRQTrampolineEnd:
ASSERT MobileIRQTrampolineEnd - MobileIRQTrampolineSource == MOBILE_IRQ_TRAMPOLINE_SIZE

MobileFail:
	ld [wMobileError], a
	ld a, MOBILE_COOLDOWN_TICKS
	ld [wMobileCooldown], a
; An aborted packet resets in libmobile after serial inactivity. Do not send
; a fresh START into a partial packet before this cooldown has elapsed.
MobileRelease:
	call MobileReleaseNoIME
	ei
	ret
MobileReleaseNoIME:
	di
	xor a
	ldh [rSC], a
	ld [wMobileOwner], a
	ld [wMobileTransferActive], a
	ld [wMobileWire], a
	ld [wMobileOnline], a
	ld [wMobilePeerCount], a
	ldh a, [rIF]
	and ~IE_SERIAL
	ldh [rIF], a
	ldh a, [rIE]
	and ~IE_SERIAL
	ld b, a
	ld a, [wMobileSavedSerialIE]
	or b
	ldh [rIE], a
	ld a, [wMobileSavedSB]
	ldh [rSB], a
	ld a, [wMobileSavedSC]
	and $03 ; never restart a previous transfer
	ldh [rSC], a
	ret

MobileCheckGuards:
	ld a, [wMobileGuardStart]
	cp $a5
	ret nz
	ld a, [wMobileGuardTx]
	cp $a5
	ret nz
	ld a, [wMobileGuardRx]
	cp $a5
	ret nz
	ld a, [wMobileGuardEnd]
	cp $a5
	ret

; Decrement a little-endian resident word at HL. Z iff it becomes zero.
MobileDecrement:
	mobile_serial_decrement

; HL bytes, BC length -> DE unsigned 16-bit sum. Foreground only.
MobileChecksum:
	ld de, 0
.loop
; Sum eight bytes per counted iteration. Keep the full16-bit length and exact
; carry arithmetic, including resumed response chunks. Wire budgets are unchanged.
	ld a, b
	and a
	jr nz, .eight
	ld a, c
	cp 8
	jr c, .tail
.eight
rept 8
	ld a, [hli]
	add e
	ld e, a
	adc d
	sub e
	ld d, a
endr
	ld a, c
	sub 8
	ld c, a
	jr nc, .loop
	dec b
	jr .loop
.tail
	ld a, c
	and a
	ret z
.tail_loop
	ld a, [hli]
	add e
	ld e, a
	adc d
	sub e
	ld d, a
	dec c
	jr nz, .tail_loop
	ret

MobilePrepareCommand:
	ld a, [wMobileStep]
	cp (MobileCommandsEnd - MobileCommands) / 4
	jp nc, .invalid
	add a
	add a
	ld e, a
	ld d, 0
	ld hl, MobileCommands
	add hl, de
	ld a, [hli]
	ld [wMobileCommand], a
	ld c, [hl]
	inc hl
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld a, [wMobileStep]
	cp 4
	jr nz, .size
	ld a, [wMobileMode]
	and a
	jr z, .echo
	ld a, [wMobilePhase]
	and a
	jr z, .hello
	cp MOBILE_PHASE_LEAVE
	jr z, .poll
	ld a, [wMobileSendCooldown]
	and a
	jr nz, .poll
	ld a, [wMobileWanted]
	and a
	jr z, .update
	call MobilePresenceChanged
	jr nz, .update
	ld a, [wMobileKeepalive]
	and a
	jr nz, .poll
.update
	call MobileBuildUpdate
	ld a, [wMobilePhase]
	cp MOBILE_PHASE_LEAVE
	call nz, MobileNextSequence
	ld a, 1 ; one pending latest snapshot, bounded by serial frame budget
	ld [wMobileSendCooldown], a
	ld a, 30
	ld [wMobileKeepalive], a
	jr .outgoing
.hello
	ld a, [wMobilePingSent]
	and a
	jr nz, .poll
.outgoing
	ld hl, wMobileOutgoing
	ld a, [wMobileOutgoingLength]
	cp 8 ; application frame header
	jp c, .invalid
	cp MOBILE_FRAME_SIZE + 1
	jp nc, .invalid
	ld c, a
	inc c
	ld a, 1
	ld [wMobilePingSent], a
	jr .size
.poll
	; libmobile waits up to one second on an empty TCP receive. Sending an
	; application POLL avoids that delay without generating roster traffic.
	ld hl, MobilePollVector
	ld c, 9
	jr .size
.echo
	ld a, [wMobilePingSent]
	and a
	jr z, .first_ping
	ld c, 1
	jr .size
.first_ping
	inc a
	ld [wMobilePingSent], a
.size
	ld a, c
	add 12
	ld [wMobileTxSize], a
	push bc
	ld de, wMobileTx + 6
	ld a, c
	and a
	jr z, .copied
.copy
	ld a, [wMobileMode]
	and a
	jr z, .copy_byte
	ld a, [wMobileStep]
	cp 4
	jr nz, .copy_byte
; Reserve the first payload byte for the connection ID, then copy the frame.
	inc de
	dec c
	jr z, .copied
.copy_mobile
	ld a, [hli]
	ld [de], a
	inc de
	dec c
	jr nz, .copy_mobile
	jr .copied
.copy_byte
	ld a, [hli]
	ld [de], a
	inc de
	dec c
	jr nz, .copy
.copied
	ld a, [wMobileStep]
	cp 4
	jr z, .connection
	cp 5
	jr nz, .header
.connection
	ld a, [wMobileConnection]
	ld [wMobileTx + 6], a
	ld a, [wMobileStep]
	cp 4
	jr nz, .header
	ld a, [wMobileMode]
	and a
	jr nz, .header
	ld a, [wMobileSequence + 1]
	ld [wMobileTx + 11], a
	ld a, [wMobileSequence]
	ld [wMobileTx + 12], a
.header
	pop bc
	xor a
	ld [wMobileChecksumCursor + 1], a
	ld hl, wMobileTx
	ld a, $99
	ld [hli], a
	ld a, $66
	ld [hli], a
	ld a, [wMobileCommand]
	ld [hli], a
	xor a
	ld [hli], a
	ld [hli], a
	ld [hl], c
	ld b, 0
	ld hl, wMobileTx + 2
	inc bc
	inc bc
	inc bc
	inc bc
	call MobileChecksum
	ld a, d
	ld [hli], a
	ld a, e
	ld [hli], a
	ld a, $80
	ld [hli], a
	xor a
	ld [hli], a
	ld a, $4b
	ld [hli], a
	ld [hl], a
	ld a, [wMobileTxSize]
	ld [wMobileRemaining], a
	xor a
	ld [wMobileRemaining + 1], a
	ld hl, wMobileTx
	ld a, l
	ld [wMobileTxPointer], a
	ld a, h
	ld [wMobileTxPointer + 1], a
	ld hl, wMobileRx
	call MobileSetRxPointer
	ld a, MOBILE_COMMAND_TICKS
	ld [wMobileDeadline], a
	ld a, MOBILE_WIRE_TX
	ld [wMobileWire], a
	jmp MobileClock
.invalid
; Abandon the partial stream and retain the normal reconnect cooldown.
	ld a, MOBILE_ERROR_PACKET
	jp MobileFail

MobileSetRxPointer:
	ld a, l
	ld [wMobileRxPointer], a
	ld a, h
	ld [wMobileRxPointer + 1], a
	ret

; The native ISR saves registers. Its unbanked trampoline selects WRAM 2.
; All callees remain in this immutable island until RET to that trampoline.
MobileIRQImageSource::
LOAD "Mobile Serial IRQ", WRAMX[$d980], BANK[MOBILE_WRAM_BANK]
MobileIRQRuntimeStart::
MobileSerialIRQ::
	xor a
	ld [wMobileTransferActive], a
	ld hl, wMobileIRQCount
	inc [hl]
	jr nz, .counted
	inc hl
	inc [hl]
.counted
	ld hl, wMobileBurst
	dec [hl]
	ld hl, wMobileVirtualSerialSlice
	dec [hl]
	ldh a, [rSB]
	ld d, a
	ld a, [wMobileWire]
	cp MOBILE_WIRE_TX
	jr z, .tx
	cp MOBILE_WIRE_WAIT
	jr z, .waiting
	cp MOBILE_WIRE_RX
	jr z, .rx
	jp .bad
.waiting
	ld a, d
	cp $99
	jp nz, .return
	call MobileIRQStoreRx
	jp c, .bad
	ld a, MOBILE_WIRE_RX
	ld [wMobileWire], a
	ld a, 5
	ld [wMobileHeaderLeft], a
	jr .next
.tx
	call MobileIRQStoreRx
	jr c, .bad
	ld hl, wMobileRemaining
	call MobileIRQDecrement
	jr nz, .next
	ld a, MOBILE_WIRE_TX_DONE
	ld [wMobileWire], a
	jr .return
.rx
	ld a, [wMobileHeaderLeft]
	and a
	jr z, .body
	cp 5
	jr nz, .header
	ld a, d
	cp $66
	jr nz, .bad
.header
	call MobileIRQStoreRx
	jr c, .bad
	ld hl, wMobileHeaderLeft
	dec [hl]
	jr nz, .next
	ld a, [wMobileRx + 4]
	and a
	jr nz, .bad
	ld a, [wMobileRx + 5]
	add 4 ; data + two checksum bytes + adapter ID + ACK clock
	ld [wMobileRemaining], a
	ld a, 0
	adc 0
	ld [wMobileRemaining + 1], a
	jr .next
.body
	call MobileIRQStoreRx
	jr c, .bad
	ld hl, wMobileRemaining
	call MobileIRQDecrement
	jr nz, .next
	ld a, MOBILE_WIRE_DONE
	ld [wMobileWire], a
	jr .return
.next
	ldh a, [rLY]
	cp 136
	jr nc, .return ; finish this byte, then give the awaited VBlank to the game
	ldh a, [hVBlankOccurred]
	and a
	jr nz, .return
	ld a, [wMobileVirtualSerialSlice]
	and a
	jr z, .return ; return the idle CPU to presentation after a bounded byte slice
	ld a, [wMobileBurst]
	and a
	jr z, .return
	call MobileIRQClock
	jr .return
.bad
	ld a, MOBILE_WIRE_BAD
	ld [wMobileWire], a
.return
	ret

MobileIRQStoreRx:
	ld a, [wMobileRxPointer + 1]
	ld h, a
	ld a, [wMobileRxPointer]
	ld l, a
	ld a, h
	cp HIGH(wMobileRx)
	jr c, .overflow
	jr nz, .upper
	ld a, l
	cp LOW(wMobileRx)
	jr c, .overflow
.upper
	ld a, h
	cp HIGH(wMobileGuardRx)
	jr c, .store
	jr nz, .overflow
	ld a, l
	cp LOW(wMobileGuardRx)
	jr nc, .overflow
.store
	ld a, d
	ld [hli], a
	ld a, l
	ld [wMobileRxPointer], a
	ld a, h
	ld [wMobileRxPointer + 1], a
	and a
	ret
.overflow
	scf
	ret

MobileIRQDecrement:
	mobile_serial_decrement
MobileIRQClock:
	mobile_serial_clock
MobileIRQRuntimeEnd::
ASSERT MobileIRQRuntimeEnd <= $db00
ENDL
MobileIRQImageEnd::
ASSERT MobileIRQImageEnd - MobileIRQImageSource == MobileIRQRuntimeEnd - MobileIRQRuntimeStart
ASSERT MobileIRQImageEnd - MobileIRQImageSource <= $180

MobileClock:
	mobile_serial_clock

MobileValidateResponseSlice:
; A maximum-length response must not monopolize the native frame wait. The
; decoder sees no payload until this bounded checksum and trailer check finish.
	ld a, [wMobileChecksumCursor + 1]
	and a
	jr nz, .resume
	ld a, [wMobileRx + 3]
	and a
	jp nz, .bad
	ld a, [wMobileRx + 5]
	ld l, a
	ld h, 0
	ld bc, 4
	add hl, bc
	ld a, l
	ld [wMobileChecksumLeft], a
	ld a, h
	ld [wMobileChecksumLeft + 1], a
	ld hl, wMobileRx + 2
	ld a, l
	ld [wMobileChecksumCursor], a
	ld a, h
	ld [wMobileChecksumCursor + 1], a
	xor a
	ld [wMobileChecksumSum], a
	ld [wMobileChecksumSum + 1], a
.resume
	ld a, [wMobileChecksumCursor]
	ld l, a
	ld a, [wMobileChecksumCursor + 1]
	ld h, a
	ld a, [wMobileChecksumSum]
	ld e, a
	ld a, [wMobileChecksumSum + 1]
	ld d, a
	ld a, [wMobileChecksumLeft]
	ld c, a
	ld a, [wMobileChecksumLeft + 1]
	or c
	jr z, .complete
	ld a, [wMobileChecksumLeft + 1]
	and a
	ld a, 32
	jr nz, .chunk
	ld a, [wMobileChecksumLeft]
	cp 32
	jr c, .chunk
	ld a, 32
.chunk
	ld [wMobileChecksumChunk], a
	ld c, a
	ld b, 0
	call MobileChecksum.loop
	ld a, l
	ld [wMobileChecksumCursor], a
	ld a, h
	ld [wMobileChecksumCursor + 1], a
	ld a, e
	ld [wMobileChecksumSum], a
	ld a, d
	ld [wMobileChecksumSum + 1], a
	ld a, [wMobileChecksumChunk]
	ld c, a
	ld a, [wMobileChecksumLeft]
	sub c
	ld [wMobileChecksumLeft], a
	ld c, a
	ld a, [wMobileChecksumLeft + 1]
	sbc 0
	ld [wMobileChecksumLeft + 1], a
	or c
	jr z, .complete
	xor a ; Z means pending, carry clear
	ret
.complete
	ld a, [hli]
	cp d
	jr nz, .bad
	ld a, [hli]
	cp e
	jr nz, .bad
	ld a, [hli]
	cp MOBILE_DEVICE
	jr nz, .bad
	ld a, [hl]
	and a
	jr nz, .bad
	inc a ; NZ means complete, carry clear
	ret
.bad
	scf
	ret

MobileValidateResponse:
	ld a, [wMobileRx + 3]
	and a
	jr nz, .bad
	ld a, [wMobileRx + 5]
	ld c, a
	ld b, 0
	inc bc
	inc bc
	inc bc
	inc bc
	ld hl, wMobileRx + 2
	call MobileChecksum
	ld a, [hli]
	cp d
	jr nz, .bad
	ld a, [hli]
	cp e
	jr nz, .bad
	ld a, [hli]
	cp MOBILE_DEVICE
	jr nz, .bad
	ld a, [hl]
	and a
	ret z
.bad
	scf
	ret

MobileConsumeResponse:
	ld a, [wMobileStep]
	ld e, a
	ld d, 0
	ld hl, MobileResponseSizes
	add hl, de
	ld a, [hl]
	cp $ff
	jp z, .data
	ld hl, wMobileRx + 5
	cp [hl]
	jp nz, .bad
	ld a, [wMobileStep]
	cp 3
	jp nz, .advance
	ld a, [wMobileRx + 6]
	cp 2
	jp nc, .bad
	ld [wMobileConnection], a
	jp .advance
.data
	ld a, [wMobileMode]
	and a
	jp nz, MobileAppendLobbyData
	ld a, [wMobileRx + 5]
	and a
	jp z, .bad
	dec a
	ld c, a
	ld a, [wMobileAppCount]
	ld e, a
	add c
	jp c, .bad
	cp MOBILE_FRAME_SIZE + 1
	jp nc, .bad
	ld [wMobileAppCount], a
	ld a, [wMobileRx + 6]
	ld hl, wMobileConnection
	cp [hl]
	jp nz, .bad
	ld d, 0
	ld hl, wMobileApp
	add hl, de
	ld d, h
	ld e, l
	ld hl, wMobileRx + 7
	ld a, c
	and a
	jp z, .assembled
.copy
	ld a, [hli]
	ld [de], a
	inc de
	dec c
	jp nz, .copy
.assembled
	ld a, [wMobileMode]
	and a
	jp nz, MobileConsumeFrame
	ld a, [wMobileAppCount]
	cp 8
	jp c, .incomplete
	ld hl, wMobileApp
	ld de, MobileReplyHeader
	ld b, 4
.header
	ld a, [de]
	inc de
	cp [hl]
	jp nz, .bad
	inc hl
	dec b
	jp nz, .header
	ld a, [wMobileSequence + 1]
	cp [hl]
	jp nz, .bad
	inc hl
	ld a, [wMobileSequence]
	cp [hl]
	jp nz, .bad
	inc hl
	ld a, [hli]
	and a
	jp nz, .bad
	ld a, [hli]
	cp 4
	jp nz, .bad
	ld a, [wMobileAppCount]
	cp 12
	jp c, .incomplete
	jp nz, .bad
	ld de, MobileToken
	ld b, 4
.token
	ld a, [de]
	inc de
	cp [hl]
	jp nz, .bad
	inc hl
	dec b
	jp nz, .token
	ld hl, wMobileRoundTrips
	inc [hl]
.advance
	ld hl, wMobileStep
	inc [hl]
	ld a, [hl]
	cp 9
	jp nz, .ok
	ld hl, wMobileSuccessCount
	inc [hl]
	call MobileRelease
.ok
	and a
	ret
.incomplete
	and a
	ret
.bad
	scf
	ret

MobileCommands:
	db $10, 8
	dw MobileNintendo
	db $12, 6
	dw MobileDial
	db $21, 10
	dw MobilePPP
	db $23, 6
	dw MobileEndpoint
	db $15, 13
	dw MobilePing
	db $24, 1
	dw MobilePing
	db $22, 0
	dw MobilePing
	db $13, 0
	dw MobilePing
	db $11, 0
	dw MobilePing
MobileCommandsEnd:
ASSERT (MobileCommandsEnd - MobileCommands) % 4 == 0
MobileResponseSizes:
	db 8, 0, 12, 1, $ff, 1, 0, 0, 0
MobileNintendo:
	db $4e, $49, $4e, $54, $45, $4e, $44, $4f
MobileDial:
	db 0, $23, $39, $36, $37, $37 ; blue adapter + #9677 ISP
MobilePPP:
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; empty ID/password, no DNS request
MobileEndpoint:
	db 127, 0, 0, 1, HIGH(MOBILE_SERVER_PORT), LOW(MOBILE_SERVER_PORT)
MobilePing:
	db 0 ; replaced by TCP connection ID
MobileRequestVector::
	db $50, $43, 1, 1, 0, 1, 0, 4
MobileToken:
	db $54, $45, $53, $54 ; TEST
MobileRequestVectorEnd::
ASSERT MobileRequestVectorEnd - MobileRequestVector == 12
MobileReplyHeader:
	db $50, $43, 1, 2
INCLUDE "mobile_adapter/rom/session.asm"
INCLUDE "mobile_adapter/rom/virtual_actors.asm"
INCLUDE "mobile_adapter/rom/menu.asm"
MobileCodeEnd::
ASSERT MobileCodeEnd - MobileCodeStart <= 8192
INCLUDE "mobile_adapter/rom/native_instance.asm"
INCLUDE "mobile_adapter/rom/native_palettes.asm"
INCLUDE "mobile_adapter/rom/graphics_ownership.asm"

INCLUDE "mobile_adapter/rom/virtual_renderer.asm"
mobile_virtual_assets

SECTION "Mobile Compatibility", ROMX, BANK[MOBILE_ROM_BANK + 5]
MobileCompatibilityHash::
if DEF(MOBILE_TEST_SCENARIOS)
	INCBIN "mobile_adapter/build-mobile-test/compatibility.sha256"
elif DEF(FAITHFUL)
	INCBIN "mobile_adapter/build-faithful-mobile/compatibility.sha256"
else
	INCBIN "mobile_adapter/build-mobile/compatibility.sha256"
endc
MobileCompatibilityHashEnd::
ASSERT MobileCompatibilityHashEnd - MobileCompatibilityHash == 32
