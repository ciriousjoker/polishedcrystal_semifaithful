; Targeted native active-OBJ prewrite guards. Every entry preserves native ABI.
; Decrementing zero-count loops keep their native 256-iteration semantics.
SECTION "Mobile Native Palette Write Guards", ROMX, BANK[MOBILE_ROM_BANK]
MobilePalettePrewriteCopyPals::
 push af
 push bc
 push de
 push hl
 ld b, 0
 ld a, c
 and a
 jr nz, .count
 inc b
.count
 rept 3
 sla c
 rl b
 endr
 jr MobilePaletteGuardCurrentBank

MobilePalettePrewriteFade::
 push af
 push bc
 push de
 push hl
 ld b, 0
 ld c, d
 ld a, c
 and a
 jr nz, .count
 inc b
.count
 sla c
 rl b
 jr MobilePaletteGuardCurrentBank

MobilePalettePrewriteLoad::
 push af
 push bc
 push de
 push hl
 ld b, 0
 ld a, c
 and a
 jr nz, .count
 inc b
.count
 ld h, d
 ld l, e
 jr MobilePaletteGuardCurrentBank

MobilePalettePrewriteWhite::
 push af
 push bc
 push de
 push hl
 ld h, d
 ld l, e
 ld bc, 8
 jr MobilePaletteGuardFixedBank

MobilePalettePrewriteFixedCopy::
 push af
 push bc
 push de
 push hl
 ld h, d
 ld l, e
 jr MobilePaletteGuardFixedBank

MobilePalettePrewriteBlack::
 push af
 push bc
 push de
 push hl
 ld hl, wOBPals2
 ld bc, 64
 jr MobilePaletteGuardFixedBank

MobilePalettePrewriteClear::
 push af
 push bc
 push de
 push hl
 ld hl, wBGPals2
 ld bc, 128
 jr MobilePaletteGuardFixedBank

MobilePalettePrewriteObjectHandler::
 push af
 push bc
 push de
 push hl
 ldh a, [rWBK]
 push af
 ld a, BANK(wPalFadeDelayFrames)
 ldh [rWBK], a
 ld a, [wPalFadeDelayFrames]
 and a
 jr nz, .active
 ld a, BANK(wPalFlags)
 ldh [rWBK], a
 ld a, [wPalFlags]
 and NO_DYN_PAL_APPLY
 jr nz, .restore
.active
 ld h, d
 ld l, e
 ld bc, wOBPals2 - wOBPals1
 add hl, bc
 ld bc, 8
 call MobilePaletteGuardRange
.restore
 pop af
 ldh [rWBK], a
 jr MobilePaletteGuardReturn

MobilePaletteGuardCurrentBank:
 ldh a, [rWBK]
 and 7
 cp BANK(wOBPals2)
 jr nz, MobilePaletteGuardReturn
MobilePaletteGuardFixedBank:
 ldh a, [rWBK]
 push af
 call MobilePaletteGuardRange
 pop af
 ldh [rWBK], a
MobilePaletteGuardReturn:
 pop hl
 pop de
 pop bc
 pop af
 ret

MobilePaletteGuardRange:
 ; Does [HL, HL+BC) overlap DDC0..DDFF? A BG+OBJ span crosses the bound.
 ld a, h
 cp HIGH(wOBPals2 + 64)
 ret nc
 add hl, bc
 jr c, .intersects
 ld a, h
 cp HIGH(wOBPals2)
 ret c
 jr nz, .intersects
 ld a, l
 cp LOW(wOBPals2)
 ret c
 ret z
.intersects
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ; Preserve normal producer auth, immutable completions and physical raw owners.
 call MobileRejectNormalPresentation
 ld a, (1 << MOBILE_PEERS) - 1
 ld hl, wMobilePrivatePalDirty
 or [hl]
 ld [hl], a
 ret
MobilePaletteWriteGuardsEnd::
