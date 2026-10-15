; Standalone untrained integer network. Compile-time shape, ROM0 model.
; ABI: seed NetworkInput (0..15), NetworkLegal (little-endian bitmask).
; call NetworkEvaluate with interrupts disabled and a valid stack.
; Returns A and NetworkAction = slot or $ff. Clobbers all registers.
IF !DEF(NET_INPUTS)
 DEF NET_INPUTS EQU 64
ENDC
IF !DEF(NET_HIDDEN)
 DEF NET_HIDDEN EQU 48
ENDC
IF !DEF(NET_ACTIONS)
 DEF NET_ACTIONS EQU 12
ENDC
ASSERT NET_INPUTS > 0 && NET_INPUTS <= 136 && NET_INPUTS % 2 == 0
ASSERT NET_HIDDEN > 0 && NET_HIDDEN <= 136 && NET_HIDDEN % 2 == 0
ASSERT NET_ACTIONS > 0 && NET_ACTIONS <= 12
DEF NET_W1 EQU NET_INPUTS * NET_HIDDEN / 2
DEF NET_W2 EQU NET_HIDDEN * NET_ACTIONS / 2
DEF NET_BYTES EQU 16 + NET_W1 + NET_W2 + 2 * (NET_HIDDEN + NET_ACTIONS) + NET_HIDDEN
; Bias contract +/-16384; 136*15*8+16384=32704 <32768.
IF !DEF(NET_GAME)
SECTION "Network header", ROM0[$100]
    nop
    jp NetworkEvaluate
    ds $150 - @, 0
ENDC
IF DEF(NET_GAME)
 IF !DEF(NET_BANK)
  DEF NET_BANK EQU 127
 ENDC
 SECTION "Network runtime", ROMX, BANK[NET_BANK]
ELSE
 SECTION "Network runtime", ROM0[$150]
ENDC
NetworkEvaluate::
    ld bc, NetworkModel + 16
    ld hl, NetworkModel + 16 + NET_W1 + NET_W2
    ld a, l
    ld [NetworkBias], a
    ld a, h
    ld [NetworkBias+1], a
    ld hl, NetworkHidden
    ld a, l
    ld [NetworkDest], a
    ld a, h
    ld [NetworkDest+1], a
    ld hl, NetworkModel + NET_BYTES - NET_HIDDEN
    ld a, l
    ld [NetworkShift], a
    ld a, h
    ld [NetworkShift+1], a
    xor a
    ld [NetworkLayer], a
    ld a, NET_HIDDEN
    ld [NetworkRemaining], a
.neuron
    ld a, [NetworkBias]
    ld l, a
    ld a, [NetworkBias+1]
    ld h, a
    ld a, [hli]
    ld e, a
    ld a, [hli]
    ld d, a
    ld a, l
    ld [NetworkBias], a
    ld a, h
    ld [NetworkBias+1], a
    ld a, [NetworkLayer]
    and a
    ld hl, NetworkInput
    ld a, NET_INPUTS / 2
    jr z, .dot
    ld hl, NetworkHidden
    ld a, NET_HIDDEN / 2
.dot
    call NetworkDot
    ld a, [NetworkLayer]
    and a
    jr nz, .logit
    ld a, [NetworkShift]
    ld l, a
    ld a, [NetworkShift+1]
    ld h, a
    ld a, [hli]
    push af
    ld a, l
    ld [NetworkShift], a
    ld a, h
    ld [NetworkShift+1], a
    pop af
    and a
    jr z, .clip
.shift
    sra d
    rr e
    dec a
    jr nz, .shift
.clip
    bit 7, d
    jr nz, .zero
    ld a, d
    and a
    jr nz, .max
    ld a, e
    cp 16
    jr c, .hidden
.max
    ld a, 15
    jr .hidden
.zero
    xor a
.hidden
    ld e, a
.logit
    ld a, [NetworkDest]
    ld l, a
    ld a, [NetworkDest+1]
    ld h, a
    ld [hl], e
    inc hl
    ld a, [NetworkLayer]
    and a
    jr z, .stored
    ld [hl], d
    inc hl
.stored
    ld a, l
    ld [NetworkDest], a
    ld a, h
    ld [NetworkDest+1], a
    ld hl, NetworkRemaining
    dec [hl]
    jp nz, .neuron
    ld a, [NetworkLayer]
    and a
    jr nz, NetworkSelect
    inc a
    ld [NetworkLayer], a
    ld a, NET_ACTIONS
    ld [NetworkRemaining], a
    ld hl, NetworkLogits
    ld a, l
    ld [NetworkDest], a
    ld a, h
    ld [NetworkDest+1], a
    jp .neuron
NetworkSelect::
    ld a, $ff
    ld [NetworkAction], a
    xor a
    ld [NetworkIndex], a
    ld a, [NetworkLegal]
    ld [NetworkMask], a
    ld a, [NetworkLegal+1]
    ld [NetworkMask+1], a
    ld hl, NetworkLogits
.next
    ld a, [hli]
    ld e, a
    ld a, [hli]
    ld d, a
    push hl
    ld hl, NetworkMask+1
    srl [hl]
    dec hl
    rr [hl]
    jr nc, .skip
    ld a, [NetworkAction]
    cp $ff
    jr z, .best
    ld a, [NetworkBest+1]
    xor $80
    ld h, a
    ld a, d
    xor $80
    cp h
    jr c, .skip
    jr nz, .best
    ld a, [NetworkBest]
    ld h, a
    ld a, e
    cp h
    jr c, .skip
    jr z, .skip
.best
    ld a, e
    ld [NetworkBest], a
    ld a, d
    ld [NetworkBest+1], a
    ld a, [NetworkIndex]
    ld [NetworkAction], a
.skip
    pop hl
    ld a, [NetworkIndex]
    inc a
    ld [NetworkIndex], a
    cp NET_ACTIONS
    jr nz, .next
    ld a, [NetworkAction]
    ret
NetworkDot:
    ldh [NetworkPairs], a
.loop
FOR nibble, 2
    ld a, [hli]
    swap a
    and $f0
    push hl
    ld l, a
    ld a, [bc]
IF nibble == 1
    inc bc
    swap a
ENDC
    and $0f
    or l
    ld l, a
    ld h, HIGH(NetworkProductLow)
    ld a, [hl]
    add e
    ld e, a
    inc h
    ld a, [hl]
    adc d
    ld d, a
    pop hl
ENDR
    ldh a, [NetworkPairs]
    dec a
    ldh [NetworkPairs], a
    jr nz, .loop
    ret
NetworkTimingProbe::
REPT 16
    nop
ENDR
    ret
NetworkCodeEnd::
IF DEF(NET_GAME)
 SECTION "Network products", ROMX, BANK[NET_BANK], ALIGN[8]
ELSE
 SECTION "Network products", ROM0, ALIGN[8]
ENDC
NetworkProductLow::
FOR x, 16
 FOR w, 16
  db LOW(x * (w - 16 * (w >= 8)))
 ENDR
ENDR
NetworkProductHigh::
FOR x, 16
 FOR w, 16
  db HIGH(x * (w - 16 * (w >= 8)))
 ENDR
ENDR
IF DEF(NET_GAME)
 SECTION "Network model", ROMX, BANK[NET_BANK]
ELSE
 SECTION "Network model", ROM0
ENDC
NetworkModel::
    INCBIN "network_runtime_model.bin"
NetworkModelEnd::
ASSERT NetworkModelEnd - NetworkModel == NET_BYTES
IF DEF(NET_GAME)
 SECTION "Network scratch", WRAM0
ELSE
 SECTION "Network scratch", WRAM0[$c000]
ENDC
NetworkInput:: ds NET_INPUTS
NetworkHidden:: ds NET_HIDDEN
NetworkLogits:: ds 2 * NET_ACTIONS
NetworkLegal:: ds 2
NetworkAction:: ds 1
NetworkBias: ds 2
NetworkDest: ds 2
NetworkShift: ds 2
NetworkLayer: ds 1
NetworkRemaining: ds 1
NetworkIndex: ds 1
NetworkMask: ds 2
NetworkBest: ds 2
NetworkScratchEnd::
IF DEF(NET_GAME)
 SECTION "Network pair counter", HRAM
ELSE
 SECTION "Network pair counter", HRAM[$ff80]
ENDC
NetworkPairs: ds 1
