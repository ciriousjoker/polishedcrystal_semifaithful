; Exact policy-pilot-16-v1 extraction from active battle RAM.
; The generated include derives every address from the frozen linked game SYM.
; FeatureExtract: A=perspective 0/1; caller guarantees positive MaxHP.
; FeatureEvaluate: A=perspective 0/1; validates the narrow neutral 1v1 profile,
; builds the two-move PP mask and calls the unchanged NetworkEvaluate.
; Interrupts must be disabled, bank 127 selected, and stack valid.
INCLUDE "feature_runtime_addresses.inc"
SECTION "Feature runtime", ROMX, BANK[127]
FeatureEvaluate::
    cp 2
    jp nc, FeatureReject
    ld [FeatureSide], a
    ld hl, FeatureGuards
.guards
    ld a, [hli]
    ld e, a
    ld a, [hli]
    ld d, a
    or e
    jr z, .living
    ld a, [de]
    cp [hl]
    jp nz, FeatureReject
    inc hl
    jr .guards
.living
    ld hl, FX_wBattleMonHP
    call FeatureNonzeroWord
    jp z, FeatureReject
    ld hl, FX_wEnemyMonHP
    call FeatureNonzeroWord
    jp z, FeatureReject
    ld hl, FX_wBattleMonMaxHP
    call FeatureNonzeroWord
    jp z, FeatureReject
    ld hl, FX_wEnemyMonMaxHP
    call FeatureNonzeroWord
    jp z, FeatureReject
    ld hl, FX_wBattleMonPP
    ld a, [FeatureSide]
    and a
    jr z, .mask
    ld hl, FX_wEnemyMonPP
.mask
    ld a, [hli]
    and 63
    ld b, 0
    jr z, .second
    inc b
.second
    ld a, [hl]
    and 63
    jr z, .masked
    set 1, b
.masked
    ld a, b
    ld [NetworkLegal], a
    xor a
    ld [NetworkLegal+1], a
    ld a, b
    and a
    jr z, FeatureEmpty
    xor a
    ld [FeatureStatus], a
    ld a, [FeatureSide]
    call FeatureExtract
    jp NetworkEvaluate
FeatureEmpty:
    ld a, 2
    jr FeatureFailure
FeatureReject:
    ld a, 1
FeatureFailure:
    ld [FeatureStatus], a
    xor a
    ld [NetworkLegal], a
    ld [NetworkLegal+1], a
    ld hl, NetworkInput
    ld b, 16 + 16 + 24
.clear
    ld [hli], a
    dec b
    jr nz, .clear
    ld a, $ff
    ld [NetworkAction], a
    ret
FeatureNonzeroWord:
    ld a, [hli]
    or [hl]
    ret
FeatureExtract::
    cp 2
    jp nc, FeatureReject
    ld [FeatureSide], a
    ld hl, FX_wBattleMonMaxHP
    call FeatureNonzeroWord
    jp z, FeatureReject
    ld hl, FX_wEnemyMonMaxHP
    call FeatureNonzeroWord
    jp z, FeatureReject
    xor a
    ld [FeatureStatus], a
    ld a, [FeatureSide]
    ld hl, FeatureDescriptors0
    and a
    jr z, .table
    ld hl, FeatureDescriptors1
.table
    ld de, NetworkInput
    ld a, e
    ld [FeatureOutput], a
    ld a, d
    ld [FeatureOutput+1], a
    ld a, 16
    ld [FeatureRemaining], a
.next
    ld a, [hli]
    ld [FeatureKind], a
    ld a, [hli]
    ld e, a
    ld a, [hli]
    ld d, a
    push hl
    ld h, d
    ld l, e
    ld a, [FeatureKind]
    and a
    jr nz, .pp
    ld a, [hli]
    ld b, a
    ld c, [hl]
    jr .value
.pp
    ld a, [hl]
    and 63
    ld c, a
    ld b, 0
.value
    pop hl
    ld a, [hli]
    ld e, a
    ld a, [hli]
    ld d, a
    push hl
    ld h, d
    ld l, e
    ld a, [hli]
    ld e, [hl]
    ld d, a
    call FeatureRatio
    ld b, a
    ld a, [FeatureOutput]
    ld l, a
    ld a, [FeatureOutput+1]
    ld h, a
    ld a, b
    ld [hli], a
    ld a, l
    ld [FeatureOutput], a
    ld a, h
    ld [FeatureOutput+1], a
    pop hl
    ld a, [FeatureRemaining]
    dec a
    ld [FeatureRemaining], a
    jr nz, .next
    ret
; BC=numerator, DE=positive denominator. Returns min(15, floor(15*BC/DE)).
; Fifteen bounded additions avoid any multiply or 21-bit product. At each
; iteration remainder < denominator; a carry is the seventeenth sum bit.
FeatureRatio::
    ld a, d
    or e
    jp z, FeatureReject
    ld a, b
    cp d
    jr c, .fraction
    jr nz, .maximum
    ld a, c
    cp e
    jr c, .fraction
.maximum
    ld a, 15
    ret
.fraction
    xor a
    ld [FeatureQuotient], a
    ld a, 15
    ld [FeatureIterations], a
    ld hl, 0
.loop
    add hl, bc
    jr c, .subtract
    ld a, h
    cp d
    jr c, .continue
    jr nz, .subtract
    ld a, l
    cp e
    jr c, .continue
.subtract
    ld a, l
    sub e
    ld l, a
    ld a, h
    sbc d
    ld h, a
    ld a, [FeatureQuotient]
    inc a
    ld [FeatureQuotient], a
.continue
    ld a, [FeatureIterations]
    dec a
    ld [FeatureIterations], a
    jr nz, .loop
    ld a, [FeatureQuotient]
    ret
FeatureCodeEnd::
FeatureDenominator255: db 0, 255
FeatureDenominator63: db 0, 63
INCLUDE "feature_runtime_tables.inc"
FeatureROMEnd::
SECTION "Feature scratch", WRAM0
FeatureSide: ds 1
FeatureStatus:: ds 1
FeatureOutput: ds 2
FeatureRemaining: ds 1
FeatureKind: ds 1
FeatureQuotient: ds 1
FeatureIterations: ds 1
FeatureScratchEnd::
