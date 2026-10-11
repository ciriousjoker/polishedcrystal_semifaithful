; Map fingerprints are derived in bounded idle jobs, never in the game frame.
SECTION "Mobile Native Map Fingerprint", ROMX, BANK[MOBILE_ROM_BANK]
MobileNativeInstanceStart::
MobileNativeInstanceSlice:
; Enter/leave WBK=1. Carry means the complete coherent fingerprint is ready.
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileHashActive]
	and a
	jr nz, .resume
	ld a, 1
	ldh [rWBK], a
	ld a, [wMapWidth]
	ld e, a
	add 6
	ld c, a
	ld b, 0
	ld hl, wOverworldMapBlocks + 3
	add hl, bc
	add hl, bc
	add hl, bc
	ld a, [wMapHeight]
	ld d, a
	ld a, [wMapTileset]
	ld b, a
	ld c, a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, e
	ld [wMobileHashWidth], a
	ld [wMobileHashColumns], a
	ld a, d
	ld [wMobileHashRows], a
	ld a, 1
	ld [wMobileHashActive], a
	jr .run
.resume
	ld a, [wMobileHashPointer]
	ld l, a
	ld a, [wMobileHashPointer + 1]
	ld h, a
	ld a, [wMobileHashB]
	ld b, a
	ld a, [wMobileHashC]
	ld c, a
	ld a, [wMobileHashColumns]
	ld e, a
.run
	ld d, 16
.cell
	ld a, [hli]
	add c
	ld c, a
	add b
	ld b, a
	dec e
	jr nz, .credit
	ld a, [wMobileHashWidth]
	ld e, a
	push bc
	ld bc, 6
	add hl, bc
	pop bc
	push hl
	ld hl, wMobileHashRows
	dec [hl]
	pop hl
	jr z, .complete
.credit
	dec d
	jr nz, .cell
	ld a, l
	ld [wMobileHashPointer], a
	ld a, h
	ld [wMobileHashPointer + 1], a
	ld a, e
	ld [wMobileHashColumns], a
	ld a, b
	ld [wMobileHashB], a
	ld a, c
	ld [wMobileHashC], a
	ld a, 1
	ldh [rWBK], a
	and a
	ret
.complete
	xor a
	ld [wMobileHashActive], a
	ld a, b
	ld [wMobileInstanceKey], a
	ld a, c
	ld [wMobileInstanceKey + 1], a
	ld a, 1
	ldh [rWBK], a
	scf
	ret

MobileNativeInvalidateHash:
; Preserve the caller's bank, including an in-progress decoder or packet job.
	ldh a, [rWBK]
	push af
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	xor a
	ld [wMobileHashActive], a
	pop af
	ldh [rWBK], a
	ret
MobileNativeInstanceEnd::
ASSERT MobileNativeInstanceEnd - MobileNativeInstanceStart <= 256
