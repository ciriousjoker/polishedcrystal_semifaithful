; Standalone mapper diagnostic. No Polished Crystal gameplay or save writes.
SECTION "MBC30 probe sentinel zero", ROM0[$0000]
	db $f0
SECTION "MBC30 probe header", ROM0[$0100]
	jp Main
	ds $0150 - @, 0
Main:
	di
	cp $11 ; CGB boot register, checked by the common probe runner
	ld a, 0
	jr nz, .model_ready
	inc a
.model_ready
	ldh [hCGB], a
	xor a
	ldh [hCrashCode], a ; fixture marker; bank-1 control proves execution
	ld sp, $d000
.wait_vblank
	ldh a, [$ff44]
	cp 144
	jr c, .wait_vblank
	xor a
	ldh [$ff40], a
	ldh [$ff4f], a
	ldh [$ffff], a
	ld a, 1
	ldh [$ff70], a
	ld [$2000], a
	ld a, [$4000]
	ld [wMobileMBC30Bank01], a
	ld a, $80
	ld [$2000], a
	ld a, [$4000]
	ld [wMobileMBC30Bank80], a
	ld a, $ff
	ld [$2000], a
	ld a, [$4000]
	ld [wMobileMBC30BankFF], a
	ld hl, Tile
	ld de, $8000
	ld b, 16
.tile
	ld a, [hli]
	ld [de], a
	inc de
	dec b
	jr nz, .tile
	ld hl, $9800
	ld bc, 1024
.map
	xor a
	ld [hli], a
	dec bc
	ld a, b
	or c
	jr nz, .map
	ld a, $80
	ldh [$ff68], a
	ld hl, Palette
	ld b, 8
.palette
	ld a, [hli]
	ldh [$ff69], a
	dec b
	jr nz, .palette
	ld a, $e4
	ldh [$ff47], a
	ld a, $91
	ldh [$ff40], a
.forever
	halt
	jr .forever
Tile:
	rept 8
	db $55, 0
	endr
Palette:
	dw $7fff, $0000, $001f, $03e0
SECTION "MBC30 probe lower bank", ROMX[$4000], BANK[$01]
	db $31
SECTION "MBC30 probe last ordinary bank", ROMX[$4000], BANK[$7f]
	db $7f
SECTION "MBC30 probe first upper bank", ROMX[$4000], BANK[$80]
	db $a5
SECTION "MBC30 probe last upper bank", ROMX[$4000], BANK[$ff]
	db $5a
SECTION "MBC30 probe observations", WRAM0[$c100]
wMobileMBC30Bank01:: db
wMobileMBC30Bank80:: db
wMobileMBC30BankFF:: db
SECTION "MBC30 probe runner markers", HRAM[$ff80]
hCGB:: db
hCrashCode:: db
