; Exact int4 dot product, inputs 0..15, weights signed two's complement nibbles.
; Entry HL = input, BC = packed weights, DE = int16 bias, A = number of pairs.
; Caller guarantees sum fits int16. Destroys AF and preserves no flags.
; Table costs 512 bytes, aligned to a 256-byte boundary.
SECTION "Dot", ROM0[$100]
Dot:
    ldh [$ff80], a
.loop:
    ld a, [hli]
    swap a
    and $f0
    push hl
    ld l, a
    ld a, [bc]
    and $0f
    or l
    ld l, a
    ld h, HIGH(ProductLow)
    ld a, [hl]
    add e
    ld e, a
    inc h
    ld a, [hl]
    adc d
    ld d, a
    pop hl

    ld a, [hli]
    swap a
    and $f0
    push hl
    ld l, a
    ld a, [bc]
    inc bc
    swap a
    and $0f
    or l
    ld l, a
    ld h, HIGH(ProductLow)
    ld a, [hl]
    add e
    ld e, a
    inc h
    ld a, [hl]
    adc d
    ld d, a
    pop hl

    ldh a, [$ff80]
    dec a
    ldh [$ff80], a
    jr nz, .loop
    ret
SECTION "Products", ROM0, ALIGN[8]
ProductLow:
FOR x, 16
    FOR w, 16
        IF w < 8
            db LOW(x * w)
        ELSE
            db LOW(x * (w - 16))
        ENDC
    ENDR
ENDR
ProductHigh:
FOR x, 16
    FOR w, 16
        IF w < 8
            db HIGH(x * w)
        ELSE
            db HIGH(x * (w - 16))
        ENDC
    ENDR
ENDR
