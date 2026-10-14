; Serialized normal producer source. Ordinary refresh preserves identity/history.
SECTION "Mobile Retained Normal Source", ROMX, BANK[MOBILE_ROM_BANK]
; Revoke immediately before the original native fill/copy, including equal bytes.
; Setup before these entries does not modify the captured source.
MobileClearNormalSourceBytes::
 call MobileInvalidateNormalSource
 jp ByteFill

MobileRestoreNormalSourceBytes::
 call MobileInvalidateNormalSource
 jp CopyBytes

MobileInvalidateNormalSource::
 push af
 push bc
 push de
 push hl
 ldh a, [rWBK]
 push af
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 call MobileRejectNormalSource
 pop af
 ldh [rWBK], a
 pop hl
 pop de
 pop bc
 pop af
 ret

MobileRejectNormalSource:
 call MobileRejectNormalPresentation
 xor a
 ld [wMobileStableProducerActive], a
 ld [wMobileNativeCaptureActive], a
 ld hl, wMobileVirtualDescriptors + 6
 ld de, 8
 ld b, NUM_OBJECT_STRUCTS
.clear
 ld [hl], 0
 add hl, de
 dec b
 jr nz, .clear
 ret

MobileRejectNormalPresentation:
 xor a
 ld [wMobileStablePendingValid], a
 ld [wMobileStablePendingNative], a
 ld [wMobileStablePendingNative + 1], a
 ld [wMobileStablePendingPrivate], a
 ld [wMobileStablePendingPrivate + 1], a
 ld [wMobileStableNativeCandidateValid], a
 ld [wMobileStableSourceValid], a
 ld [wMobileStableActorViewValid], a
 ld [wMobileStableCapacityValid], a
 ld a, [wMobileStableFinalDecision]
 cp $82
 jr z, .chosen_output
 xor a
 ld [wMobileStableFinalDecision], a
.chosen_output
 xor a
 ld [wMobileVirtualPreviousOAM], a
 ld [wMobileVirtualPublished], a
 ld [wMobileVirtualStage], a
 ld [wMobileVirtualCursor], a
 ret

MobileBeginNormalAttempt::
 push af
 push bc
 push de
 push hl
 ld a, [wStateFlags]
 ld c, a
 ldh a, [rWBK]
 push af
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 push bc
 call MobileRejectNormalSource
 ld hl, wMobileStableOwners
 ld bc, OAM_COUNT
 ld a, $ff
 rst ByteFill
 pop bc
 ld a, c
 bit SPRITE_UPDATES_DISABLED_F, a
 jr z, .done
 ldh a, [hInMenu]
 and a
 jr nz, .done
 ld a, [wMobileLocalValid]
 and a
 jr z, .done
 ld a, 1
 ld [wMobileStableProducerActive], a
 ld [wMobileNativeCaptureActive], a
.done
 pop af
 ldh [rWBK], a
 pop hl
 pop de
 pop bc
 pop af
 ret

; Called at weather entry before its first native raw/palette mutation.
; Preserve an authorization created by this normal attempt, never create one.
MobileBeginWeatherSource::
 push af
 push bc
 push de
 push hl
 ldh a, [rWBK]
 push af
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 call MobileRejectNormalPresentation
 pop af
 ldh [rWBK], a
 pop hl
 pop de
 pop bc
 pop af
 ret

; Close after final weather timer increment and before the destructive limiter.
MobileCaptureNormalSource::
 push af
 push bc
 push de
 push hl
 ldh a, [rWBK]
 push af
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 call MobileRejectNormalPresentation
 ld a, [wMobileStableFinalDecision]
 cp $82
 jr nz, .unlocked
 xor a
 ld [wMobileStableProducerActive], a
 ld [wMobileNativeCaptureActive], a
 jp .done
.unlocked
 ld a, [wMobileStableProducerActive]
 cp 1
 jp nz, .done
 xor a
 ld [wMobileStableProducerActive], a
 ld a, [wMobileNativeCaptureActive]
 cp 1
 jp nz, .done
 xor a
 ld [wMobileNativeCaptureActive], a
 ld hl, wShadowOAM
 ld de, wMobileStableOriginalYs
 ld b, OAM_COUNT
.ys
 ld a, [hli]
 ld [de], a
 inc de
 inc hl
 inc hl
 inc hl
 dec b
 jr nz, .ys
 ld hl, wShadowOAM + 1
 ld de, wMobileStableRawXTileFlags
 ld b, OAM_COUNT
.triples
 rept 3
 ld a, [hli]
 ld [de], a
 inc de
 endr
 inc hl
 dec b
 jr nz, .triples
 ld hl, wOBPals2
 ld de, wMobileStableNativeRGB
 ld bc, 64
 rst CopyBytes
 ld a, BANK(wMapGroup)
 ldh [rWBK], a
 ld a, [wMapGroup]
 ld [wMobileVirtualMapGroup], a
 ld a, [wMapNumber]
 ld [wMobileVirtualMapNumber], a
 ; Freeze the same physical world/view projection used by the native picture.
 ; Later simulation motion cannot move private peers against old native OAM.
 call MobileStepProgress
 ld c, d
 ld b, 0
 ld a, [wXCoord]
 call MobileWorldPixel
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, l
 ld [wMobileStableCameraX], a
 ld a, h
 ld [wMobileStableCameraX + 1], a
 ld a, BANK(wYCoord)
 ldh [rWBK], a
 call MobileStepProgress
 ld c, e
 ld b, 0
 ld a, [wYCoord]
 call MobileWorldPixel
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, l
 ld [wMobileStableCameraY], a
 ld a, h
 ld [wMobileStableCameraY + 1], a
 ld a, 1
 ld [wMobileStableActorViewValid], a
 ; This is source truth, not proof that the post-limiter native candidate fits.
 ld [wMobileStableSourceValid], a
 call MobileVirtualBeginRetainedComposition
.done
 pop af
 ldh [rWBK], a
 pop hl
 pop de
 pop bc
 pop af
 ret

; A is a native slot. Revoke before the earliest association store.
MobileRetireNativeSlot::
 push af
 push bc
 push de
 push hl
 cp NUM_OBJECT_STRUCTS
 jr nc, .done
 ld c, a
 ldh a, [rWBK]
 push af
 ld a, 5
 ldh [rWBK], a
 push bc
 call MobileRejectNormalSource
 pop bc
 ld b, 0
 ld hl, wMobileStableBindingValid
 add hl, bc
 ld a, [hl]
 and a
 jr z, .restore ; repeated deletion does not retire twice
 ld [hl], 0
 ld hl, wMobileStablePhases
 add hl, bc
 ld [hl], 0
 ld hl, wMobileStableOutputOffsets
 add hl, bc
 ld [hl], $ff
 ld a, c
 and 7
 ld b, a
 inc b
 ld a, 1
.mask
 dec b
 jr z, .mask_ready
 add a
 jr .mask
.mask_ready
 cpl
 ld d, a ; inverse slot bit
 ld a, c
 cp 8
 ld e, 0
 jr c, .low
 inc e
.low
 ld hl, MobileRetireMasks
 ld b, 11
.loop
 ld a, [hli]
 push hl
 ld l, a
 ld h, HIGH(wMobileStableEligible)
 ld a, e
 add l
 ld l, a
 ld a, [hl]
 and d
 ld [hl], a
 pop hl
 dec b
 jr nz, .loop
 ; Retired is an event bit, independent of valid and source proof.
 ld hl, wMobileStableRetired
 ld a, e
 add l
 ld l, a
 ld a, d
 cpl
 or [hl]
 ld [hl], a
.restore
 pop af
 ldh [rWBK], a
.done
 pop hl
 pop de
 pop bc
 pop af
 ret
; BC points to the native structure being destructively cleared.
MobileRetireNativeObject::
 push af
 push de
 push hl
 ld hl, wObjectStructs
 ld de, OBJECT_LENGTH
 xor a
.find
 push af
 ld a, h
 cp b
 jr nz, .next
 ld a, l
 cp c
 jr nz, .next
 pop af
 call MobileRetireNativeSlot
 jr .done
.next
 pop af
 inc a
 cp NUM_OBJECT_STRUCTS
 jr nc, .done
 add hl, de
 jr .find
.done
 pop hl
 pop de
 pop af
 ret
; Temporary callers use the actual destination in DE, not a map-object index.
; Preserve the native entry ABI, including flags and every WBK bit.
MobileRetireNativeObjectDE::
 push bc
 ld b, d
 ld c, e
 call MobileRetireNativeObject
 pop bc
 ret

; InitTempObject already tail-calls across banks. Keep its lower-bank size and
; let the ordinary far-jump machinery restore both nested return banks.
MobileCopyTempObjectToObjectStruct::
 call MobileRetireNativeObjectDE
 farjp CopyTempObjectToObjectStruct

MobileRetireMasks:
 db LOW(wMobileStableEligible), LOW(wMobileStableContinuous)
 db LOW(wMobileStablePhaseMasks), LOW(wMobileStablePhaseMasks + 2)
 db LOW(wMobileStableLastActual), LOW(wMobileStableLastEligible)
 db LOW(wMobileStableDue), LOW(wMobileStableNativeComplete)
 db LOW(wMobileStablePendingNative), LOW(wMobileStablePendingPrivate)
 db LOW(wMobileStableRetired)
MobileOwnCompletedNativeTuple::
 push af
 push bc
 push de
 push hl
 ldh a, [rWBK]
 push af
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 ld a, [wMobileVirtualIndex]
 ld c, a
 add a
 add a
 add a
 add LOW(wMobileVirtualDescriptors + 6)
 ld l, a
 ld h, HIGH(wMobileVirtualDescriptors)
 ld a, [hli]
 and $1f
 ld b, a
 ld a, [hl]
 rrca
 rrca
 and $3f
 ld e, a
 ld d, 0
 ld hl, wMobileStableOwners
 add hl, de
.mark
 ld [hl], c
 inc hl
 dec b
 jr nz, .mark
 pop af
 ldh [rWBK], a
 pop hl
 pop de
 pop bc
 pop af
 ret

; HL names the native raw byte about to be replaced. Revoke ownership even
; for an equal-byte store. Weather authorization survives until its closure.
MobileUnknownNativeWrite::
 push af
 push bc
 push de
 push hl
 ld a, h
 cp HIGH(wShadowOAM)
 jr nz, .done
 ld a, l
 cp OAM_SIZE
 jr nc, .done
 rrca
 rrca
 and $3f
 ld e, a
 ld d, 0
 ldh a, [rWBK]
 push af
 ld a, MOBILE_BACKUP_BANK
 ldh [rWBK], a
 call MobileRejectNormalPresentation
 ld hl, wMobileStableOwners
 add hl, de
 ld [hl], $ff
 pop af
 ldh [rWBK], a
.done
 pop hl
 pop de
 pop bc
 pop af
 ret
MobileRetainedNormalSourceEnd::
ASSERT BANK(wOBPals2) == MOBILE_BACKUP_BANK
ASSERT wOBPals2 == $ddc0
