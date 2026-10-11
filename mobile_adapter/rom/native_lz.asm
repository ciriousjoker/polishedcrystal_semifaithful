; Resumable native LZ decoder. Only private avatar caches are writable output.
; One pump emits at most eight bytes. Native VBlank mode is never changed.
; Per-peer state is copied to this private scratch by the upload scheduler.
MobileNativeLZStart:
; B:DE compressed source, HL output cache. Fresh state has already been cleared.
	ld a, b
	ld [wMobileLZBank], a
	ld a, e
	ld [wMobileLZInput], a
	ld a, d
	ld [wMobileLZInput + 1], a
	ld a, l
	ld [wMobileLZOutput], a
	ld [wMobileLZBase], a
	ld a, h
	ld [wMobileLZOutput + 1], a
	ld [wMobileLZBase + 1], a
	ret

MobileNativeLZPump:
; Carry: terminator accepted. NZ: bounded work remains. Z: invalid local asset.
	ld a, 8
	ld [wMobileLZBudget], a
.again
	ld a, [wMobileLZLeft]
	ld b, a
	ld a, [wMobileLZLeft + 1]
	or b
	jr nz, .emit
	call .command
	ret c
	ret z
.emit
; Reject overlong output before reading or writing any further cache byte.
	ld a, [wMobileLZBase]
	ld e, a
	ld a, [wMobileLZBase + 1]
	ld d, a
	ld a, [wMobileLZOutput]
	sub e
	ld e, a
	ld a, [wMobileLZOutput + 1]
	sbc d
	jr z, .room
	cp HIGH(24 tiles)
	jp nz, .invalid
	ld a, e
	cp LOW(24 tiles)
	jp nc, .invalid
.room
	call .byte
	ret z
	ld b, a
	ld a, [wMobileLZOutput]
	ld l, a
	ld a, [wMobileLZOutput + 1]
	ld h, a
	ld [hl], b
	inc hl
	ld a, l
	ld [wMobileLZOutput], a
	ld a, h
	ld [wMobileLZOutput + 1], a
	ld hl, wMobileLZLeft
	call MobileDecrement
	ld hl, wMobileLZBudget
	dec [hl]
	jr nz, .again
	ld a, 1
	and a
	ret
.invalid
	xor a
	ret
.complete
	scf
	ret
.read
; ROM reads restore its bank before returning. Scratch/cache stay in bank 2.
	ld a, [wMobileLZInput]
	ld l, a
	ld a, [wMobileLZInput + 1]
	ld h, a
	ld a, [wMobileLZBank]
	call GetFarByte
	push af
	inc hl
	ld a, l
	ld [wMobileLZInput], a
	ld a, h
	ld [wMobileLZInput + 1], a
	pop af
	ret
.command
	call .read
	cp $ff
	jr z, .complete
	ld b, a
	and $e0
	cp $e0
	jr z, .long
	ld a, b
	and $1f
	inc a
	ld c, a
	ld a, b
	rlca
	rlca
	rlca
	and 7
	ld b, 0
	jr .length
.long
	ld a, b
	cp $fc
	jr nc, .packed
	and 3
	push af
	ld a, b
	rrca
	rrca
	and 7
	push af
	call .read
	ld c, a
	pop af
	pop de ; high length byte is in D
	ld b, d
	inc bc
	jr .length
.packed
	sub $fc
	add 7
	push af
	call .read
	ld c, a
	ld b, 0
	inc bc
	pop af
	ld [wMobileLZHalf], a ; reset below, command retained in A
.length
	ld [wMobileLZCommand], a
; This ROM's LZ variant encodes repeat count minus two and alternate count
; minus three. Other base and packed commands encode count minus one.
	cp 1
	jr z, .repeat_count
	cp 2
	jr nz, .count_ready
	inc bc
.repeat_count
	inc bc
.count_ready
	ld a, c
	ld [wMobileLZLeft], a
	ld a, b
	ld [wMobileLZLeft + 1], a
	xor a
	ld [wMobileLZHalf], a
	ld a, [wMobileLZCommand]
	cp 1
	jr z, .repeat
	cp 2
	jr z, .alternate
	cp 4
	jr c, .ready
	cp 7
	jr nc, .ready
; Rewrite command: absolute offset or -(low seven bits + 1) from current output.
	call .read
	bit 7, a
	jr nz, .relative
	push af
	call .read
	ld c, a
	pop af
	ld b, a
	ld a, [wMobileLZBase]
	ld l, a
	ld a, [wMobileLZBase + 1]
	ld h, a
	add hl, bc
	jr .copy_pointer
.relative
	and $7f
	inc a
	ld b, a
	ld a, [wMobileLZOutput]
	sub b
	ld l, a
	ld a, [wMobileLZOutput + 1]
	sbc 0
	ld h, a
.copy_pointer
	ld a, l
	ld [wMobileLZCopy], a
	ld a, h
	ld [wMobileLZCopy + 1], a
	jr .ready
.alternate
	call .read
	ld [wMobileLZFirst], a
	call .read
	ld [wMobileLZSecond], a
	jr .ready
.repeat
	call .read
	ld [wMobileLZFirst], a
.ready
	ld a, 1
	and a
	ret
.byte
	ld a, [wMobileLZCommand]
	and a
	jr z, .literal
	cp 1
	jr z, .repeated
	cp 2
	jr z, .alternated
	cp 3
	jr z, .zero
	cp 7
	jp nc, .nibble
; Only already decoded bytes in this cache can be referenced.
	ld a, [wMobileLZCopy]
	ld l, a
	ld a, [wMobileLZCopy + 1]
	ld h, a
	ld a, [wMobileLZBase]
	ld e, a
	ld a, [wMobileLZBase + 1]
	ld d, a
	ld a, l
	sub e
	ld a, h
	sbc d
	jp c, .invalid
	ld a, [wMobileLZOutput]
	ld e, a
	ld a, [wMobileLZOutput + 1]
	ld d, a
	ld a, l
	sub e
	ld a, h
	sbc d
	jp nc, .invalid
	ld c, [hl]
	ld a, [wMobileLZCommand]
	cp 6
	jr z, .reverse
	inc hl
	jr .copy_next
.reverse
	dec hl
.copy_next
	ld a, l
	ld [wMobileLZCopy], a
	ld a, h
	ld [wMobileLZCopy + 1], a
	ld a, [wMobileLZCommand]
	cp 5
	ld a, c
	jr nz, .value
; Native LZ bit reversal, applied to a single bounded output byte.
	rlca
	rlca
	xor c
	and $aa
	xor c
	ld c, a
	swap c
	xor c
	and $33
	xor c
	rrca
	jr .value
.literal
	call .read
	jr .value
.repeated
	ld a, [wMobileLZFirst]
	jr .value
.alternated
	ld a, [wMobileLZHalf]
	xor 1
	ld [wMobileLZHalf], a
	ld a, [wMobileLZFirst]
	jr nz, .value
	ld a, [wMobileLZSecond]
	jr .value
.zero
	xor a
	jr .value
.nibble
	ld a, [wMobileLZHalf]
	xor 1
	ld [wMobileLZHalf], a
	jr z, .low
	call .read
	ld [wMobileLZPacked], a
	swap a
	jr .unpack
.low
	ld a, [wMobileLZPacked]
.unpack
	and $f
	ld c, a
	ld a, [wMobileLZCommand]
	cp 9
	ld a, c
	jr z, .value
	ld a, [wMobileLZCommand]
	cp 7
	ld a, c
	jr nz, .lookup
	swap a
	jr .value
.lookup
	ld b, 0
	ld hl, .table
	add hl, bc
	ld a, [hl]
.value
; Preserve the byte while returning NZ even for a decoded zero byte.
	push af
	ld a, 1
	and a
	pop bc
	ld a, b
	ret
.table
	db $00, $ff, $01, $02, $03, $fe, $80, $07
	db $c0, $7f, $04, $0f, $1f, $3f, $08, $fc
