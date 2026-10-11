; Native saves include runtime object records. Project the original leased
; records into the ordinary save copy without changing live game memory.
SECTION "Mobile Native Save Isolation", ROMX
MobileNativeSaveCodeStart::

MobileNativeCaptureFree::
; Bank 1, BC native object. Capture even an empty slot's exact original bytes.
	push bc
	push de
	push hl
	farcall MobileNativeCaptureOriginal
	push bc
	farcall MobileNativeBackupPointer
	pop de
	ld b, OBJECT_LENGTH
.copy
	ld a, 1
	ldh [rWBK], a
	ld a, [de]
	inc de
	ld c, a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld [hl], c
	inc hl
	dec b
	jr nz, .copy
	ld a, 1
	ldh [rWBK], a
	pop hl
	pop de
	pop bc
	ret

MobileNativeRestoreFree::
; Caller already verified current ownership. Restore the exact empty record.
	push bc
	push de
	push hl
	push bc
	ld a, [wMobileBudgetIndex]
	farcall MobileNativeOriginalPointer
	pop de
	ld b, OBJECT_LENGTH
.copy
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [hli]
	ld c, a
	ld a, 1
	ldh [rWBK], a
	ld a, c
	ld [de], a
	inc de
	dec b
	jr nz, .copy
	pop hl
	pop de
	pop bc
	ret

MobileNativeSaveObjects::
; DE is the end of the native wPlayerData copy in the currently open SRAM bank.
; Both primary and backup call here before native checksum generation.
	push af
	push bc
	push de
	push hl
	ldh a, [rWBK]
	push af
	ld hl, -(wPlayerDataEnd - wObjectStructs)
	add hl, de
	ld d, h
	ld e, l
	ld b, MOBILE_PEERS
	ld c, 0
.peer
	push bc
	push de
	ld hl, wMobileNativeOriginals
	ld de, OBJECT_LENGTH
	ld a, c
.backup
	and a
	jr z, .backup_ready
	add hl, de
	dec a
	jr .backup
.backup_ready
	push hl
	ld h, 0
	ld l, c
	ld de, wMobileSavedFacings
	add hl, de
	ld a, [hl]
	and a
	jr z, .skip
	cp NUM_OBJECT_STRUCTS
	jr nc, .skip
; Calculate a bounded native-object offset and verify it is still our lease.
	ld l, a
	ld h, 0
	add hl, hl
	ld b, h
	ld c, l
rept 4
	add hl, hl
endr
	add hl, bc
	push hl
	ld bc, wObjectStructs + OBJECT_MAP_OBJECT_INDEX
	add hl, bc
	ld a, 1
	ldh [rWBK], a
	ld a, [hl]
	cp TEMP_OBJECT
	jr nz, .not_owned
	ld bc, OBJECT_FLAGS2 - OBJECT_MAP_OBJECT_INDEX
	add hl, bc
	ld a, [hl]
	and HIGH_PRIORITY | FROZEN
	cp HIGH_PRIORITY | FROZEN
	jr nz, .not_owned
	pop hl ; bounded object offset
	pop de ; original bank-5 record
	pop bc ; SRAM object-array base
	add hl, bc
	push bc
	call SwapHLDE
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld bc, OBJECT_LENGTH
	rst CopyBytes
	pop de ; retain the same SRAM base for the next peer
	jr .next
.not_owned
	pop hl
.skip
	pop hl
	pop de
.next
	pop bc
	inc c
	dec b
	jr nz, .peer
	pop af
	ldh [rWBK], a
	pop hl
	pop de
	pop bc
	pop af
	ret

MobileNativePaletteGuard::
; Native palette scratch also lies inside wPlayerData. Preserve it across our
; ordinary allocator call, including failed allocations and cached palettes.
	push af
	ldh a, [rWBK]
	push af
	ld a, 1
	ldh [rWBK], a
	ld a, [wNeededPalIndex]
	push af
	ld a, [wNeededPalType]
	push af
	ld a, [wNeededObjPalGlow]
	push af
	ld a, [wPrevNeededObjPalGlow]
	push af
	ld a, [wNeededMonPalLight]
	push af
	ld hl, sp + 13
	ld a, [hl]
	farcall MobileNativePrivatePalette
	push af
	pop bc ; preserve returned palette and carry while restoring scratch
	pop af
	ld [wNeededMonPalLight], a
	pop af
	ld [wPrevNeededObjPalGlow], a
	pop af
	ld [wNeededObjPalGlow], a
	pop af
	ld [wNeededPalType], a
	pop af
	ld [wNeededPalIndex], a
	pop af
	ldh [rWBK], a
	pop af
	push bc
	pop af
	ret

MobileNativeSaveCodeEnd::
ASSERT MobileNativeSaveCodeEnd - MobileNativeSaveCodeStart <= 384
