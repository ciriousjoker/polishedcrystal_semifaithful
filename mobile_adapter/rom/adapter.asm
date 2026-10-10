SECTION "Mobile Adapter", ROMX, BANK[MOBILE_ROM_BANK]
MobileCodeStart::

; Foreground only: DelayFrame has completed its VBlank wait. Preserve all
; registers and WBK. This routine never waits, calls DelayFrame, or reenters.
; Milestone 1 developer control: Select+B in safe overworld toggles the proof.
MobileTick::
	push af
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
	ld a, [wMobileOwner]
	and a
	jr nz, .key
	ld hl, wMobileCooldown
	ld a, [hl]
	and a
	jr z, .key
	dec [hl]
.key
	ldh a, [hJoypadDown]
	and PAD_SELECT | PAD_B
	cp PAD_SELECT | PAD_B
	jr z, .pressed
	xor a
	ld [wMobileKeyLatch], a
	jr .pump
.pressed
	ld a, [wMobileKeyLatch]
	and a
	jr nz, .pump
	inc a
	ld [wMobileKeyLatch], a
	ld a, [wMobileOwner]
	and a
	jr z, .begin
	ld a, 2
	ld [wMobileKeyLatch], a
	ld a, MOBILE_ERROR_CANCEL
	call MobileFail
	call MobileConsumeKey
	jp .return
.begin
	ld a, [wMobileCooldown]
	and a
	jr nz, .pump
	ld a, [wMapStatus]
	cp MAPSTATUS_HANDLE
	jr nz, .pump
	ldh a, [hInMenu]
	and a
	jr nz, .pump
	ld a, [wScriptRunning]
	and a
	jr nz, .pump
	call MobileBegin
.pump
	ld a, [wMobileKeyLatch]
	cp 2
	call z, MobileConsumeKey
	ld a, [wMobileOwner]
	and a
	jp z, .return
; Existing link/printer state must not share the serial owner.
	ld a, [wLinkMode]
	and a
	jr nz, .busy
	ld a, [wPrinterConnectionOpen]
	bit PRINTER_CONNECTION_OPEN, a
	jr z, .deadline
.busy
	ld a, MOBILE_ERROR_BUSY
	call MobileFail
	jp .return
.deadline
	ld hl, wMobileAttemptTicks
	call MobileDecrement
	jp z, .timeout
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	call MobileCheckGuards
	jp nz, .packet_error
	ld a, [wMobileWire]
	and a
	jr z, .prepare
	ld hl, wMobileDeadline
	dec [hl]
	jp z, .timeout
	ld a, [wMobileWire]
	cp MOBILE_WIRE_TX_DONE
	jr z, .request_done
	cp MOBILE_WIRE_DONE
	jr z, .response_done
	cp MOBILE_WIRE_BAD
	jp z, .packet_error
	ld a, [wMobileTransferActive]
	and a
	jp nz, .return
	ld a, MOBILE_BURST_BYTES
	ld [wMobileBurst], a
	call MobileClock
	jp .return
.prepare
	call MobilePrepareCommand
	jp .return
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
	ld a, MOBILE_BURST_BYTES
	ld [wMobileBurst], a
	call MobileClock
	jr .return
.response_done
	call MobileValidateResponse
	jr c, .packet_error
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
	ld a, [wMobileOwner]
	and a
	jr z, .return
	xor a
	ld [wMobileWire], a
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
	pop af
	ldh [rWBK], a
	pop hl
	pop de
	pop bc
	pop af
	ret

; Consume an accepted developer chord so Select does not start an ordinary
; registered-item script. Other buttons and unsafe-state input are untouched.
MobileConsumeKey:
	ldh a, [hJoypadDown]
	and ~(PAD_SELECT | PAD_B)
	ldh [hJoypadDown], a
	ldh a, [hJoypadPressed]
	and ~(PAD_SELECT | PAD_B)
	ldh [hJoypadPressed], a
	ret

; Cable/printer foreground entry points yield the serial port before touching
; their registers. This also covers paths that do not wait for another frame.
MobileYieldSerial::
	push af
	push bc
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
	ld a, [wLinkMode]
	and a
	ret nz
	ld a, [wPrinterConnectionOpen]
	bit PRINTER_CONNECTION_OPEN, a
	ret nz
	ldh a, [rSC]
	bit 7, a
	ret nz
	ld [wMobileSavedSC], a
	ldh a, [rSB]
	ld [wMobileSavedSB], a
	ldh a, [rIE]
	and IE_SERIAL
	ld [wMobileSavedSerialIE], a
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

MobileFail:
	ld [wMobileError], a
	ld a, MOBILE_COOLDOWN_TICKS
	ld [wMobileCooldown], a
; An aborted packet resets in libmobile after serial inactivity. Do not send
; a fresh START into a partial packet before this cooldown has elapsed.
MobileRelease:
	di
	xor a
	ldh [rSC], a
	ld [wMobileOwner], a
	ld [wMobileTransferActive], a
	ld [wMobileWire], a
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
	ei
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
	ld a, [hl]
	sub 1
	ld [hli], a
	ld a, [hl]
	sbc 0
	ld [hld], a
	or [hl]
	ret

; HL bytes, BC length -> DE unsigned 16-bit sum. Foreground only.
MobileChecksum:
	ld de, 0
.loop
	ld a, [hli]
	add e
	ld e, a
	adc d
	sub e
	ld d, a
	dec bc
	ld a, b
	or c
	jr nz, .loop
	ret

MobilePrepareCommand:
	ld a, [wMobileStep]
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
	ld a, [wMobileSequence + 1]
	ld [wMobileTx + 11], a
	ld a, [wMobileSequence]
	ld [wMobileTx + 12], a
.header
	pop bc
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
	ld a, MOBILE_BURST_BYTES
	ld [wMobileBurst], a
	jmp MobileClock

MobileSetRxPointer:
	ld a, l
	ld [wMobileRxPointer], a
	ld a, h
	ld [wMobileRxPointer + 1], a
	ret

; This banked IRQ does bounded byte work, never enables interrupts or waits,
; and restores the interrupted WRAM bank. Serial already saved all registers.
MobileSerialIRQ::
	ldh a, [rWBK]
	push af
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	xor a
	ld [wMobileTransferActive], a
	ld hl, wMobileIRQCount
	inc [hl]
	jr nz, .counted
	inc hl
	inc [hl]
.counted
	ldh a, [rSB]
	ld d, a
	ld a, [wMobileWire]
	cp MOBILE_WIRE_TX
	jr z, .tx
	cp MOBILE_WIRE_WAIT
	jr z, .waiting
	cp MOBILE_WIRE_RX
	jr z, .rx
	jr .bad
.waiting
	ld a, d
	cp $99
	jr nz, .return
	call MobileStoreRx
	jr c, .bad
	ld a, MOBILE_WIRE_RX
	ld [wMobileWire], a
	ld a, 5
	ld [wMobileHeaderLeft], a
	jr .next
.tx
	call MobileStoreRx
	jr c, .bad
	ld hl, wMobileRemaining
	call MobileDecrement
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
	call MobileStoreRx
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
	call MobileStoreRx
	jr c, .bad
	ld hl, wMobileRemaining
	call MobileDecrement
	jr nz, .next
	ld a, MOBILE_WIRE_DONE
	ld [wMobileWire], a
	jr .return
.next
	ld hl, wMobileBurst
	dec [hl]
	jr z, .return
	call MobileClock
	jr .return
.bad
	ld a, MOBILE_WIRE_BAD
	ld [wMobileWire], a
.return
	pop af
	ldh [rWBK], a
	ret

; D -> RX[ptr++], with a checked physical buffer end.
MobileStoreRx:
	ld a, [wMobileRxPointer + 1]
	ld h, a
	ld a, [wMobileRxPointer]
	ld l, a
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
	call MobileSetRxPointer
	and a
	ret
.overflow
	scf
	ret

; Start exactly one byte. Called with WBK=2; IRQ chains only within its budget.
MobileClock:
	ld a, [wMobileWire]
	cp MOBILE_WIRE_TX
	jr z, .tx
	ld b, $4b
	cp MOBILE_WIRE_RX
	jr nz, .send
	ld a, [wMobileHeaderLeft]
	and a
	jr nz, .send
	ld a, [wMobileRemaining + 1]
	and a
	jr nz, .send
	ld a, [wMobileRemaining]
	cp 2
	jr nz, .ack_end
	ld b, $80
	jr .send
.ack_end
	cp 1
	jr nz, .send
	ld b, 0
	jr .send
.tx
	ld a, [wMobileTxPointer + 1]
	ld h, a
	ld a, [wMobileTxPointer]
	ld l, a
	ld b, [hl]
	inc hl
	ld a, l
	ld [wMobileTxPointer], a
	ld a, h
	ld [wMobileTxPointer + 1], a
.send
	ld a, 1
	ld [wMobileTransferActive], a
	ld a, b
	ldh [rSB], a
	ld a, SC_START | SC_INTERNAL | SC_FAST
	ldh [rSC], a
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
MobileCodeEnd::
ASSERT MobileCodeEnd - MobileCodeStart <= 8192
