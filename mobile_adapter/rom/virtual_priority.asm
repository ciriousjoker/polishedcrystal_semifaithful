; Rotate only the temporary draw order, then restore it byte for byte. Native
; objects, scripts, map events and persistent state are never leased or edited.
SECTION "Mobile Native Priority Sharing", ROMX, BANK[MOBILE_ROM_BANK + 2]


MobileVirtualPriorityDispatch:
	jp c, MobileVirtualRestorePriorities
MobileVirtualRotatePriorities:
	push bc
	push de
	push hl
	ldh a, [rWBK]
	push af
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wObjectPriorities]
	and $f
	jr nz, .done
; At least eleven ordinary four-quad candidates fill native OAM. Sparse scenes
; retain their original order. Actual visible pressure is checked in idle time.
	ld hl, wObjectPriorities
	ld b, 0
.count
	ld a, [hli]
	and a
	jr z, .counted
	inc b
	ld a, b
	cp NUM_OBJECT_STRUCTS
	jr c, .count
.counted
	ld a, b
	cp OAM_COUNT / 4 + 1
	jr c, .done
	dec a
	ld b, a
	ld a, [wMobileVirtualPriorityCursor]
	cp b
	jr c, .cursor
	xor a
.cursor
	ld c, a
	inc a
	cp b
	jr c, .next_cursor
	xor a
.next_cursor
	ld [wMobileVirtualPriorityCursor], a
	ld a, b
	add LOW(wMobileVirtualPriorityBackup + 1)
	ld [wMobileVirtualPriorityEnd], a
	push bc
	ld hl, wObjectPriorities
	ld de, wMobileVirtualPriorityBackup
	ld bc, NUM_OBJECT_STRUCTS
	rst CopyBytes
	pop bc
	ld hl, wMobileVirtualPriorityBackup + 1
	ld a, l
	add c
	ld l, a
	ld de, wObjectPriorities + 1
.copy
	ld a, [hli]
	ld [de], a
	inc de
	ld a, [wMobileVirtualPriorityEnd]
	cp l
	jr nz, .within
	ld l, LOW(wMobileVirtualPriorityBackup + 1)
.within
	dec b
	jr nz, .copy
	ld a, 1
	ld [wMobileVirtualBaseReady], a ; foreground restoration token only
.done
	pop af
	ldh [rWBK], a
	pop hl
	pop de
	pop bc
	ld a, BANK(Facings)
	jp Bankswitch

MobileVirtualRestorePriorities:
	ld a, [wMobileVirtualBaseReady]
	and a
	jr z, .done
	push bc
	push de
	push hl
	ldh a, [rWBK]
	push af
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld hl, wMobileVirtualPriorityBackup
	ld de, wObjectPriorities
	ld bc, NUM_OBJECT_STRUCTS
	rst CopyBytes
	xor a
	ld [wMobileVirtualBaseReady], a
	pop af
	ldh [rWBK], a
	pop hl
	pop de
	pop bc
.done
	ld a, BANK(Facings)
	jp Bankswitch
