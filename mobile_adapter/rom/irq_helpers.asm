; Shared bounded serial primitives, emitted in ROM and protected WRAM.
MACRO mobile_serial_clock
	ldh a, [rLY]
	cp 136
	ret nc ; leave a guard window for the final byte ISR before VBlank
	ldh a, [hVBlankCounter]
	ld hl, wMobileLastFrame
	cp [hl]
	ret nz ; foreground must replenish the new frame before IRQ chaining resumes
	ld a, [wMobileBurst]
	and a
	ret z
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
ENDM

MACRO mobile_serial_decrement
	ld a, [hl]
	sub 1
	ld [hli], a
	ld a, [hl]
	sbc 0
	ld [hld], a
	or [hl]
	ret
ENDM
