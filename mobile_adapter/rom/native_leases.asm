; Native object/OAM leases. Native save copies project the original objects.
; Audited complete-dialogue candidates only. Story objects remain protected.
MobileNativeLeaseSlot:
; Bank 1 -> BC object, E native slot. Carry means no safe lease is available.
	call .slot_limit
	call .demand
	cp OAM_SIZE - 4 * OBJ_SIZE + 1
	jr nc, .reclaim
	ld bc, wObject1Struct
	ld e, 1
.free
	ld a, [bc]
	and a
	jr nz, .occupied
	farcall MobileNativeCaptureFree
	and a
	ret
.occupied
	ld hl, OBJECT_LENGTH
	add hl, bc
	ld b, h
	ld c, l
	inc e
	ld a, [wMobileNativeSlotLimit]
	cp e
	jr nz, .free
.reclaim
	farcall MobileNativeLoadLeasePolicy
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
; Selection and capture are separate bounded jobs. Revalidate a pending slot
; against the current map policy before touching any live native object.
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, 55
	add hl, bc
	ld a, [hl]
	ld [hl], 0
	and a
	jr z, .scan_candidates
	ld [wMobileLeaseBestSlot], a
	ld e, a
	ld a, [wMobileNativeSlotLimit]
	cp e
	jr z, .scan_candidates
	jr c, .scan_candidates
	ld a, e
	call MobileNativeObjectPointer
	ld a, 1
	ldh [rWBK], a
	call .approved
	jp c, .selected
.scan_candidates
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	xor a
	ld [wMobileLeaseBestSlot], a
	ld [wMobileLeaseBestScore], a
	ld a, 1
	ldh [rWBK], a
	ld bc, wObject1Struct
	ld e, 1
.candidate
	call .approved
	jp nc, .next
	push de
	ld hl, OBJECT_SPRITE_X
	add hl, bc
	ld a, [hli]
	sub SCREEN_WIDTH_PX / 2
	jr nc, .x_positive
	cpl
	inc a
.x_positive
	ld d, a
	ld a, [hl]
	sub SCREEN_HEIGHT_PX / 2
	jr nc, .y_positive
	cpl
	inc a
.y_positive
	add d
	jr c, .distant
	cp 127
	jr c, .distance
.distant
	ld a, 127
.distance
	ld d, a
	ld hl, OBJECT_FLAGS2
	add hl, bc
	bit OFF_SCREEN_F, [hl]
	jr z, .score
	set 7, d
.score
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileLeaseBestSlot]
	and a
	jr z, .better
	ld a, [wMobileLeaseBestScore]
	cp d
	jr nc, .scored
.better
	ld a, d
	ld [wMobileLeaseBestScore], a
	pop de
	ld a, e
	ld [wMobileLeaseBestSlot], a
	push de
.scored
	ld a, 1
	ldh [rWBK], a
	pop de
	jp .next
.select_candidate
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileLeaseBestSlot]
	and a
	jp z, .none
	push af
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, 55
	add hl, bc
	pop af
	ld [hl], a
	ld a, 1
	ldh [rWBK], a
	scf
	ret
.selected
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileLeaseBestSlot]
	and a
	jp z, .none
	ld e, a
	call MobileNativeObjectPointer
	ld a, 1
	ldh [rWBK], a
	farcall MobileNativeCaptureOriginal
	push de
	push bc
	call MobileNativeBackupPointer
	pop bc
	ld d, b
	ld e, c
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
; Preserve the effective camera anchor; object movement is frozen while leased.
	ld a, [wMobileCameraX]
	ld [hli], a
	ld a, [wMobileCameraY]
	ld [hli], a
	ld a, [wMobileOAMReadCursor]
	ld [hli], a
	ld a, [wMobileOAMWriteCursor]
	ld [hl], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	push hl
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
	ld bc, MOBILE_NATIVE_ROLE
	add hl, bc
	ld [hl], 1
	ld bc, 53 - MOBILE_NATIVE_ROLE
	add hl, bc
	ld [hl], 0 ; capture original native tiles before their replacement
	pop hl
	pop de
	ld a, e
	call MobileNativeObjectPointer
	ld a, 1
	ldh [rWBK], a
	and a
	ret
.next
	ld hl, OBJECT_LENGTH
	add hl, bc
	ld b, h
	ld c, l
	inc e
	ld a, [wMobileNativeSlotLimit]
	cp e
	jp nz, .candidate
	jp .select_candidate
.none
	scf
	ret
.slot_limit
; The last slot's graphics are shared by special actors. Protect the whole map's
; declared assets, including hidden actors that may appear later in a script.
	ld a, NUM_OBJECT_STRUCTS
	ld [wMobileNativeSlotLimit], a
	ld hl, wMapObjects + MAPOBJECT_SPRITE
	ld de, MAPOBJECT_LENGTH
	ld b, NUM_OBJECTS
.assets
	ld a, [hl]
	cp SPRITE_BIG_GYARADOS
	jr z, .special
	cp SPRITE_ALOLAN_EXEGGUTOR
	jr z, .special
	cp SPRITE_SAILBOAT
	jr z, .special
	cp SPRITE_VARS
	jr nc, .special ; variable sprites cannot prove a fixed graphics allocation
	add hl, de
	dec b
	jr nz, .assets
	ret
.special
	ld a, NUM_OBJECT_STRUCTS - 1
	ld [wMobileNativeSlotLimit], a
	ret
.approved
; No script or flag state is interpreted as permission. The reviewed table wins.
	push de
	ld a, [bc]
	cp SPRITE_SAILBOAT
	jr nz, .ordinary_candidate
; Snowtop's reviewed shared-tile windows must remain intact during panning.
; Their off-screen records can sleep while normal exploration uses their slots.
	ld a, [wPanningAroundTinyMap]
	and a
	jr nz, .protected
	ld hl, OBJECT_FLAGS2
	add hl, bc
	bit OFF_SCREEN_F, [hl]
	jr z, .protected
.ordinary_candidate
	ld hl, OBJECT_MAP_OBJECT_INDEX
	add hl, bc
	ld a, [hl]
	cp NUM_OBJECTS
	jr nc, .protected
	ld d, a
	and 7
	ld e, a
	ld a, d
	srl a
	srl a
	srl a
	ld l, a
	ld h, 0
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	push de
	ld de, wMobileLeaseMask
	add hl, de
	ld a, [hl]
	pop de
.mask_bit
	and a
	jr z, .protected
	inc e
	dec e
	jr z, .mask_ready
	dec e
	srl a
	jr .mask_bit
.mask_ready
	and 1
	jr z, .protected
	ld a, 1
	ldh [rWBK], a
	pop de
	scf
	ret
.protected
	ld a, 1
	ldh [rWBK], a
	pop de
	and a
	ret

.demand
; Reserve four quads for every leased actor, including uploads not yet visible.
	ld bc, wObjectStructs
	ld d, NUM_OBJECT_STRUCTS
	ld e, 0
.count
	ld a, [bc]
	and a
	jr z, .advance
	ld hl, OBJECT_MAP_OBJECT_INDEX
	add hl, bc
	ld a, [hl]
	cp TEMP_OBJECT
	jr z, .avatar
	ld hl, OBJECT_FACING
	add hl, bc
	ld a, [hl]
	cp NUM_FACINGS
	jr nc, .advance
	push bc
	push de
	ld e, a
	ld d, 0
	ld hl, MobileNativeFacingCounts
	add hl, de
	ld a, [hl]
	pop de
	pop bc
	jr .add
.avatar
	ld a, 4 * OBJ_SIZE
.add
	add e
	jr c, .full
	ld e, a
.advance
	ld hl, OBJECT_LENGTH
	add hl, bc
	ld b, h
	ld c, l
	dec d
	jr nz, .count
	ld a, e
	ret
.full
	ld a, 255
	ret

MobileNativeBackupPointer::
; Current pool index -> bank-5 backup HL. No native storage is relocated.
	ld hl, wMobileActorBackup1
	ld de, OBJECT_LENGTH + 4
	ld a, [wMobileBudgetIndex]
.loop
	and a
	ret z
	add hl, de
	dec a
	jr .loop

MobileNativeOwnSlot:
; Bank 2, A slot. Carry confirms an extant frozen lease, A retains the slot.
; A script may delete/reuse a native object while the adapter is suspended.
	push de
	ld e, a
	and a
	jr z, .lost
	cp NUM_OBJECT_STRUCTS
	jr nc, .lost
	call MobileNativeObjectPointer
	ld a, 1
	ldh [rWBK], a
	ld hl, OBJECT_MAP_OBJECT_INDEX
	add hl, bc
	ld a, [hl]
	cp TEMP_OBJECT
	jr nz, .lost
	ld hl, OBJECT_FLAGS2
	add hl, bc
	ld a, [hl]
	and HIGH_PRIORITY | FROZEN
	cp HIGH_PRIORITY | FROZEN
	jr nz, .lost
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, e
	pop de
	scf
	ret
.lost
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, e
	pop de
	and a
	ret

MobileNativeReleaseLease:
; Bank 2, HL peer. Reload a reclaimed NPC through the same bounded upload stages.
	push hl
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, wMobileSavedFacings
	add hl, de
	ld a, [hl]
	call MobileNativeOwnSlot
	jp nc, .clear_slot
	pop hl
	push hl
	ld bc, MOBILE_NATIVE_ROLE
	add hl, bc
	ld a, [hl]
	and a
	jp z, .free
	cp 2
	jr z, .restoring
	ld [hl], 2
	push hl
	ld bc, 53 - MOBILE_NATIVE_ROLE
	add hl, bc
	ld a, [hl]
	cp 6
	pop hl
	jr nc, .captured
	dec hl
	ld [hl], 7 ; no replacement was uploaded; original VRAM is intact
	jp .hide
.captured
	push hl
	call MobileNativeBackupPointer
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [hl]
	ld b, a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	pop hl
	dec hl ; stage
	ld [hl], 0
	dec hl ; sprite
	ld [hl], b
	inc hl
	inc hl
	inc hl
	ld [hl], 0 ; restart decoder for the original NPC graphics
	jp .hide
.restoring
	dec hl
	ld a, [hl]
	cp 7
	jp nz, .hide
	push hl
	call MobileNativeBackupPointer
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	push hl
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, wMobileSavedFacings
	add hl, de
	ld a, [hl]
	call MobileNativeObjectPointer
	ld d, b
	ld e, c
	pop hl
	push hl ; original camera/BG anchor stays in bank 5
	ld a, [wMobileBudgetIndex]
	farcall MobileNativeOriginalPointer
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
	ld bc, OBJECT_LENGTH
	add hl, bc
	push de
; Screen coordinates follow camera scrolling that happened while the NPC slept.
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [hli]
	ld b, a
	ld a, [hli]
	ld c, a
	ld a, [hli]
	add b
	ld b, a
	ld a, [hl]
	add c
	ld c, a
	ld a, [wMobileCameraX]
	ld d, a
	ld a, [wMobileOAMReadCursor]
	add d
	ld d, a
	ld a, b
	sub d
	ld b, a
	ld a, [wMobileCameraY]
	ld d, a
	ld a, [wMobileOAMWriteCursor]
	add d
	ld d, a
	ld a, c
	sub d
	ld c, a
	pop de
	ld hl, -OBJECT_LENGTH + OBJECT_SPRITE_X
	add hl, de
	ld a, 1
	ldh [rWBK], a
	ld a, [hl]
	add b
	ld [hli], a
	ld a, [hl]
	add c
	ld [hl], a
	ld hl, -OBJECT_LENGTH + OBJECT_FLAGS2
	add hl, de
; The normal visibility transition rebinds the original palette before drawing,
; including dual-color Pokemon icon palettes. No feature palette cache survives.
	set OFF_SCREEN_F, [hl]
	ld a, TRUE
	ldh [hCGBPalUpdate], a
; Native visibility flags are recalculated normally, retaining saved motion.
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	pop hl
	inc hl
	ld [hl], 0
	jr .clear_slot
.hide
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, wMobileSavedFacings
	add hl, de
	ld a, [hl]
	call MobileNativeObjectPointer
	ld hl, OBJECT_FLAGS1
	add hl, bc
	ld a, 1
	ldh [rWBK], a
	set INVISIBLE_F, [hl]
	ld bc, OBJECT_FACING - OBJECT_FLAGS1
	add hl, bc
	ld [hl], STANDING
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	pop hl
	ret
.free
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, wMobileSavedFacings
	add hl, de
	ld a, [hl]
	and a
	jr z, .clear_slot
	call MobileNativeObjectPointer
	ld a, 1
	ldh [rWBK], a
	farcall MobileNativeRestoreFree
	ld a, TRUE
	ldh [hCGBPalUpdate], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
.clear_slot
	farcall MobileNativeClearPaletteSource
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, wMobileSavedFacings
	add hl, de
	ld [hl], 0
	pop hl
	push hl
	ld bc, 33
	add hl, bc
	xor a
	ld [hli], a ; sprite
	ld [hli], a ; upload stage
	ld [hli], a ; backup ownership
	ld [hl], a ; decoder bank
	pop hl
	ret

MobileNativeFacingCounts:
	db 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 20, 20, 20, 20, 16, 8, 56, 64, 16, 16, 16, 16, 16, 16, 8, 8, 8, 8, 4, 4, 4, 64, 64, 16, 16, 8, 8, 8, 8, 32, 32, 28, 28, 28, 28, 8, 8, 8, 8, 24, 24, 36, 64, 64, 48, 48
ASSERT @ - MobileNativeFacingCounts == NUM_FACINGS

MobileNativeResetLeases:
	push af
	push bc
	push hl
	ld hl, wMobileNativePaletteOwners
	ld bc, NUM_OBJECT_STRUCTS
	xor a
	rst ByteFill
	pop hl
	pop bc
	pop af
; Called before native map setup. Old temporary slots must not survive a
; connection-map transition or become associated with a different peer.
	ldh a, [rWBK]
	push af
	ld de, wMobileSavedFacings
	ld a, MOBILE_PEERS
.loop
	push af
	ld a, [de]
	and a
	jr z, .next
	cp NUM_OBJECT_STRUCTS
	jr nc, .next
	call MobileNativeObjectPointer
	ld a, 1
	ldh [rWBK], a
	ld hl, OBJECT_MAP_OBJECT_INDEX
	add hl, bc
	ld a, [hl]
	cp TEMP_OBJECT
	jr nz, .next
	ld h, b
	ld l, c
	push de
	ld bc, OBJECT_LENGTH
	xor a
	rst ByteFill
	pop de
.next
	inc de
	pop af
	dec a
	jr nz, .loop
	pop af
	ldh [rWBK], a
	ret
