; Feature-owned native tile snapshots. No native decompressor state is borrowed.
SECTION "Mobile Native Tile Backups", ROMX
MobileNativeTileCodeStart::

MobileNativeBackupTiles:
; Bank 2, HL peer. Carry consumes one upload job, even if HBlank is deferred.
	push hl
	ld hl, wMobileActorBackup1
	ld de, OBJECT_LENGTH + 4
	ld a, [wMobileBudgetIndex]
.record
	and a
	jr z, .record_ready
	add hl, de
	dec a
	jr .record
.record_ready
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [hl]
	ld b, a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	pop hl
	push hl
	ld a, b
	cp SPRITE_SAILBOAT
	jr nz, .private_tiles
; Its shared atlas is protected by the native slot limit. No private-slot
; graphics from this decoration need to be captured or reuploaded.
	ld bc, 53
	add hl, bc
	ld [hl], 6
	jr .ready
.private_tiles
	ld bc, 53
	add hl, bc
	ld a, [hl]
	cp 6
	jr nc, .ready
	push hl
	call MobileNativeRawChunk
	call SwapHLDE
	ld b, 16
	di
.burst
	ldh a, [rLY]
	cp 136
	jr nc, .deferred
	ldh a, [rLCDC]
	bit B_LCDC_ENABLE, a
	jr z, .read
.leave
	ldh a, [rSTAT]
	and STAT_MODE
	jr z, .leave
.enter
	ldh a, [rLY]
	cp 136
	jr nc, .deferred
	ldh a, [rSTAT]
	and STAT_MODE
	jr nz, .enter
.read
; Four unrolled byte copies fit even the shortest native HBlank.
rept 4
	ld a, [hli]
	ld [de], a
	inc de
endr
	dec b
	jr nz, .burst
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	pop hl
	inc [hl]
	jr .done
.deferred
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	pop hl ; retry this entire private chunk
.done
	xor a
	ldh [rVBK], a
	pop hl
	scf
	ei
	ret
.ready
	pop hl
	and a
	ret

MobileNativeRawChunk::
; A=0..5 chunk, current pool/slot resident. Return HL=VRAM, DE=bank-5 source.
; Captures and restores use the same standing/walking native-slot mapping.
	ld [wMobileAvatarTile], a
	ld hl, wMobileActorTiles1
	ld bc, 24 tiles
	ld a, [wMobileBudgetIndex]
.peer
	and a
	jr z, .chunk
	add hl, bc
	dec a
	jr .peer
.chunk
	ld a, [wMobileAvatarTile]
	ld bc, 4 tiles
.source
	and a
	jr z, .source_ready
	add hl, bc
	dec a
	jr .source
.source_ready
	push hl
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, wMobileSavedFacings
	add hl, de
	ld a, [hl]
	ld b, 0
	cp FIRST_VRAM1_OBJECT_STRUCT
	jr c, .tile
	inc b
	sub FIRST_VRAM1_OBJECT_STRUCT
.tile
	add a
	add a
	ld c, a
	add c
	add c
	ld l, a
	ld h, 0
rept 4
	add hl, hl
endr
	ld de, vTiles0
	add hl, de
	ld a, b
	ldh [rVBK], a
	ld a, [wMobileAvatarTile]
	cp 3
	jr c, .standing
	ld de, $800
	inc b
	dec b
	jr z, .walking
	ld de, $400
.walking
	add hl, de
	sub 3
.standing
	ld de, 4 tiles
.destination
	and a
	jr z, .destination_ready
	add hl, de
	dec a
	jr .destination
.destination_ready
	pop de
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ret

MobileNativeTileCodeEnd::
ASSERT MobileNativeTileCodeEnd - MobileNativeTileCodeStart <= 512
