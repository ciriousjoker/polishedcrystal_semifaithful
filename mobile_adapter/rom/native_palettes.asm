; Keep native palette metadata and buffers independent of remote rendering.
SECTION "Mobile Native Palettes", ROMX, BANK[MOBILE_ROM_BANK]
MobileNativePaletteCodeStart::

MobileEmergencyInput::
	call MobileEmergencyKeys
	jp MobileNativePaletteFadeDirty

; VBlank already saved all registers and keeps IME disabled. Read physical
; buttons before any native input lock can filter them. No packet or cache data
; is inspected, and this routine never waits for the adapter or the server.
MobileEmergencyKeys:
	ldh a, [hCGB]
	cp 1
	ret nz
; The native frame owns the CPU after VBlank. A completed adapter byte may stay
; pending in IF until the next idle wait; never dispatch transport in game work.
	ld a, [wMobileOwner]
	and a
	jr z, .buttons
	ldh a, [rIE]
	and ~IE_SERIAL
	ldh [rIE], a
.buttons
	ldh a, [rJOYP]
	and $30
	push af
	ld a, JOYP_GET_BUTTONS
	ldh [rJOYP], a
rept 6
	ldh a, [rJOYP]
endr
	cpl
	and PAD_SELECT | PAD_B | PAD_A
	ld b, a
	pop af
	ldh [rJOYP], a
	ld a, b
	and PAD_SELECT | PAD_B
	cp PAD_SELECT | PAD_B
	jr z, .held
	xor a
	ld [wMobileEmergencyLatch], a
	ld [wMobileEmergencyPending], a
	ld [wMobileKeyLatch], a
	ret
.held
	ld a, [wMobileEmergencyLatch]
	and a
	jr z, .new_press
	ld a, [wMobileEmergencyPending]
	and a
	ret z
	jr .cancel
.new_press
	ld c, b
	inc a
	ld [wMobileEmergencyLatch], a
	ld a, [wMobileOwner]
	ld b, a
	ld a, [wMobileWanted]
	or b
	ld b, a
	ld a, [wMobileOnline]
	or b
	jr nz, .cancel
	xor a
	bit 0, c ; A adds the offline one-shot echo diagnostic
	jr nz, .mode
	inc a
.mode
	ld [wMobileMode], a
	ld a, 1
	ld [wMobileWanted], a
	ret
.cancel
	ld a, [wMobileWanted]
	and a
	jr z, .palette_restored
	ld a, TRUE
	ldh [hCGBPalUpdate], a
.palette_restored
	ld a, 1
	ld [wMobileEmergencyPending], a
	xor a
	ld [wMobileWanted], a
	ld [wMobilePlayerId], a
	ld [wMobilePlayerId + 1], a
	ld a, 2
	ld [wMobileKeyLatch], a
	ld a, MOBILE_ERROR_CANCEL
	ld [wMobileError], a
	ld a, MOBILE_COOLDOWN_TICKS
	ld [wMobileCooldown], a
	ld a, [wMobileOwner]
	and a
	jr z, .already_released
	farjp MobileReleaseNoIME
.already_released
	xor a
	ld [wMobileOnline], a
	ld [wMobilePeerCount], a
	ret

; Foreground only: DelayFrame has completed its VBlank wait. Preserve all
; registers and WBK. This routine never waits, calls DelayFrame, or reenters.
; Select+B toggles the lobby; adding A runs the one-shot transport diagnostic.


MobileNativeOriginalPointer::
; A bounded pool index -> unbanked original record, preserving BC/DE.
	push bc
	push de
	ld hl, wMobileNativeOriginals
	ld de, OBJECT_LENGTH
	cp MOBILE_PEERS
	jr nc, .done
.loop
	and a
	jr z, .done
	add hl, de
	dec a
	jr .loop
.done
	pop de
	pop bc
	ret

MobileNativeCaptureOriginal::
; Bank 1, BC native object. This view may be read by normal palette scans.
	push af
	push bc
	push de
	push hl
	ld d, 0 ; E is the verified native slot supplied by the lease allocator
	ld hl, wMobileNativePaletteOwners
	add hl, de
	ld a, [wMobileBudgetIndex]
	inc a
	ld [hl], a
	ld a, [wMobileBudgetIndex]
	call MobileNativeOriginalPointer
	ld d, h
	ld e, l
	ld h, b
	ld l, c
	ld bc, OBJECT_LENGTH
	rst CopyBytes
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	farcall MobileNativePeerPointer
	ld bc, 54
	add hl, bc
	ld [hl], 0
	inc hl
	ld [hl], 0 ; consume a pending lease when the original is captured
	ld a, 1
	ldh [rWBK], a
	pop hl
	pop de
	pop bc
	pop af
	ret

MobileNativeClearPaletteSource::
; Clear only the table entry still owned by this pool record.
	push af
	push bc
	push de
	push hl
	ld a, [wMobileBudgetIndex]
	ld c, a
	ld e, a
	ld d, 0
	ld hl, wMobileSavedFacings
	add hl, de
	ld a, [hl]
	and a
	jr z, .done
	cp NUM_OBJECT_STRUCTS
	jr nc, .done
	ld e, a
	ld hl, wMobileNativePaletteOwners
	add hl, de
	inc c
	ld a, [hl]
	cp c
	jr nz, .done
	ld [hl], 0
.done
	pop hl
	pop de
	pop bc
	pop af
	ret

MobileNativePrivatePalette::
; A base color, C previous hardware slot. Native save metadata is never written.
	ld [wMobilePrivatePalRequest], a
	ld a, c
	ld [wMobilePrivatePalSlot], a
	ldh a, [rWBK]
	push af
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, wMobileSavedFacings
	add hl, de
	ld a, [hl]
	ld [wMobilePrivatePalCurrent], a
; Reuse an exact ordinary color without changing its manager metadata.
	ld a, 1
	ldh [rWBK], a
	ld c, 0
.native
	ld a, c
	cp PAL_OW_WEATHER
	jr nc, .borrow
	call .bit
	ld b, a
	ld hl, wLoadedObjPal0
	ld e, c
	ld d, 0
	add hl, de
	ld a, [wMobilePrivatePalRequest]
	cp [hl]
	jr nz, .next
	ld a, [wLoadedObjPalType]
	and b
	jr nz, .next
	ld hl, wLoadedObjPalGlows
	add hl, de
	ld a, [hl]
	ld hl, wLoadedObjPalPrevGlows
	add hl, de
	or [hl]
	jr nz, .next
; A native color can already be borrowed by a different remote identity.
; Check its private owner before reusing it, without scanning every actor.
	push bc
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld hl, wMobilePrivatePalBindings
	add hl, de
	ld a, [hl]
	and a
	jr z, .native_available
	cp MOBILE_PEERS + 1
	jr nc, .native_available
	dec a
	ld e, a
	ld hl, wMobilePrivatePalIdentities
	add hl, de
	ld a, [wMobilePrivatePalRequest]
	cp [hl]
	jr z, .native_available
	ld a, 1
	ldh [rWBK], a
	pop bc
	jr .next
.native_available
	ld a, 1
	ldh [rWBK], a
	pop bc
	ld a, c
	ld [wMobilePrivatePalSlot], a
	call .copy_native
	jp .success
.next
	inc c
	jr .native
.borrow
	xor a
	ld [wMobilePrivatePalUsed], a
	ld [wMobilePrivatePalBlocked], a
	ld a, 1
	ldh [rWBK], a
	ld de, wObjectStructs
	lb bc, NUM_OBJECT_STRUCTS, 0
.objects
	ld a, [wMobilePrivatePalCurrent]
	cp c
	jr z, .advance
	ld a, [de]
	and a
	jr z, .advance
	ld hl, OBJECT_FLAGS1
	add hl, de
	bit INVISIBLE_F, [hl]
	jr nz, .advance
	inc hl
	bit OFF_SCREEN_F, [hl]
	jr nz, .advance
	ld hl, OBJECT_PALETTE
	add hl, de
	ld a, [hl]
	and OAM_PALETTE
	push bc
	ld c, a
	call .bit
	ld c, a
	ld a, [wMobilePrivatePalUsed]
	or c
	ld [wMobilePrivatePalUsed], a
	ld hl, OBJECT_MAP_OBJECT_INDEX
	add hl, de
	ld a, [hl]
	cp TEMP_OBJECT
	jr nz, .ordinary
	ld hl, OBJECT_FLAGS2
	add hl, de
	ld a, [hl]
	and HIGH_PRIORITY | FROZEN
	cp HIGH_PRIORITY | FROZEN
	jr nz, .ordinary
	ld hl, OBJECT_PAL_INDEX
	add hl, de
	ld a, [wMobilePrivatePalRequest]
	cp [hl]
	jr z, .ordinary
	ld a, [wMobilePrivatePalBlocked]
	or c
	ld [wMobilePrivatePalBlocked], a
.ordinary
	pop bc
.advance
	ld hl, OBJECT_LENGTH
	add hl, de
	ld d, h
	ld e, l
	inc c
	dec b
	jr nz, .objects
; Slots 6/7 belong to weather and menu/effect users. Other unused hardware
; colors are borrowed privately, while both ordinary palette buffers stay intact.
	ld a, [wMobilePrivatePalSlot]
	cp PAL_OW_WEATHER
	jr nc, .choose
	ld c, a
	call .bit
	ld b, a
	ld a, [wMobilePrivatePalUsed]
	and b
	jr z, .cached
.choose
	ld c, 0
.empty
	call .bit
	ld b, a
	ld a, [wMobilePrivatePalUsed]
	and b
	jr z, .selected
	inc c
	ld a, c
	cp PAL_OW_WEATHER
	jr c, .empty
	pop af
	ldh [rWBK], a
	scf
	ret
.selected
	ld a, c
	ld [wMobilePrivatePalSlot], a
	jr .generate
.cached
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	farcall MobileNativePeerPointer
	ld bc, 54
	add hl, bc
	ld a, [hl]
	cp 1
	jr nz, .generate
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, wMobilePrivatePalIdentities
	add hl, de
	ld a, [wMobilePrivatePalRequest]
	cp [hl]
	jr nz, .generate
	ld c, e
	call .bit
	ld b, a
	ld a, [wMobilePrivatePalDirty]
	and b
	jp z, .success
; Retain the complete cached color during a native fade. Its dirty bit remains
; set, so the next stable idle job refreshes the color without replaying steps.
	ld a, [wPalFadeDelayFrames]
	and a
	jp nz, .success
.generate
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
; CopyObjectSpritePalHandler can replay up to 30 fade steps. That variable work
; cannot be admitted to this bounded job. New private colors wait for stability.
	ld a, [wPalFadeDelayFrames]
	and a
	jr z, .stable_generation
	ld a, 3
	ld [wMobileDeferred], a
	pop af
	ldh [rWBK], a
	scf
	ret
.stable_generation
	call .native_pointer
	ld de, wMobilePrivatePalTemporary
	ld bc, 1 palettes
	rst CopyBytes
	call .native_pointer
	ld bc, wOBPals2 - wOBPals1
	add hl, bc
	ld bc, 1 palettes
	rst CopyBytes
	ldh a, [hCGBPalUpdate]
	ld [wMobilePrivatePalSavedUpdate], a
; The native constructor also refreshes palette endpoint and fade scratch state.
; Private color generation must leave those ordinary engine values untouched.
	assert BANK(wPalFadeStepValue) == MOBILE_BACKUP_BANK
	ld a, [wPalFadeStepValue]
	push af
	ld a, [wPalState]
	push af
	assert PALSTATE_SIZE == 4
	ld hl, wCurPalStates
rept PALSTATE_SIZE
	ld a, [hli]
	push af
endr
	ld a, 1
	ldh [rWBK], a
	ld a, [wMobilePrivatePalRequest]
	ld [wNeededPalIndex], a
	xor a
	ld [wNeededPalType], a
	ld [wNeededObjPalGlow], a
	ld [wPrevNeededObjPalGlow], a
	dec a
	ld [wNeededMonPalLight], a
	call .native_pointer
	ld d, h
	ld e, l
	ld a, [wPalFlags]
	push af
	and ~NO_DYN_PAL_APPLY
	ld [wPalFlags], a
	farcall CopyObjectSpritePalHandler
	pop af
	ld [wPalFlags], a
	ld hl, wCurPalStates + PALSTATE_SIZE - 1
rept PALSTATE_SIZE
	pop af
	ld [hld], a
endr
	pop af
	ld [wPalState], a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	pop af
	ld [wPalFadeStepValue], a
	call .copy_native
; Restore original target and active buffers, including a running fade.
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	call .native_pointer
	ld d, h
	ld e, l
	ld hl, wMobilePrivatePalTemporary
	ld bc, 1 palettes
	rst CopyBytes
	push hl
	call .native_pointer
	ld bc, wOBPals2 - wOBPals1
	add hl, bc
	ld d, h
	ld e, l
	pop hl
	ld bc, 1 palettes
	rst CopyBytes
	ld a, [wMobilePrivatePalSavedUpdate]
	ldh [hCGBPalUpdate], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	farcall MobileNativePeerPointer
	ld bc, 54
	add hl, bc
	ld [hl], 1
	call MobileNativeClearPaletteBinding
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobilePrivatePalSlot]
	ld e, a
	ld d, 0
	ld hl, wMobilePrivatePalBindings
	add hl, de
	ld a, [wMobileBudgetIndex]
	inc a
	ld [hl], a
	ld a, TRUE
	ldh [hCGBPalUpdate], a
	jr .success
.copy_native
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	call .color_pointer
	ld d, h
	ld e, l
	call .native_pointer
	ld bc, wOBPals2 - wOBPals1
	add hl, bc
	ld bc, 1 palettes
	rst CopyBytes
	ld a, [wMobileBudgetIndex]
	ld e, a
	ld d, 0
	ld hl, wMobilePrivatePalIdentities
	add hl, de
	ld a, [wMobilePrivatePalRequest]
	ld [hl], a
	ld c, e
	call .bit
	cpl
	ld hl, wMobilePrivatePalDirty
	and [hl]
	ld [hl], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	farcall MobileNativePeerPointer
	ld bc, 54
	add hl, bc
	ld [hl], 2 ; exact ordinary color: native uploads keep it current
	call MobileNativeClearPaletteBinding
	ret
.success
	pop af
	ldh [rWBK], a
	ld a, [wMobilePrivatePalSlot]
	and a
	ret
.native_pointer
	ld hl, wOBPals1
	ld a, [wMobilePrivatePalSlot]
	ld bc, 1 palettes
	rst AddNTimes
	ret
.color_pointer
	ld hl, wMobilePrivatePalColors
	ld a, [wMobileBudgetIndex]
	ld bc, 1 palettes
	rst AddNTimes
	ret
.bit
; C bounded 0..7; return the corresponding bit without changing BC/DE.
	push bc
	ld b, c
	inc b
	ld a, 1
.bit_loop
	dec b
	jr z, .bit_done
	add a
	jr .bit_loop
.bit_done
	pop bc
	ret

MobileNativePaletteFadeDirty:
	ld a, [wMobileOnline]
	and a
	ret z
	ldh a, [rWBK]
	push af
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wPalFadeDelayFrames]
	ld b, a
	ld a, [wMobilePrivatePalWasFading]
	or b
	jr z, .stable
	ld a, (1 << MOBILE_PEERS) - 1
	ld [wMobilePrivatePalDirty], a
.stable
	ld a, b
	ld [wMobilePrivatePalWasFading], a
	pop af
	ldh [rWBK], a
	ret

MobileNativeClearPaletteBinding:
; Preserve the caller's selected WRAM bank and invalidate only this pool entry.
	ldh a, [rWBK]
	push af
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	inc a
	ld c, a
	ld b, 8
	ld hl, wMobilePrivatePalBindings
.loop
	ld a, [hl]
	cp c
	jr nz, .next
	ld [hl], 0
	ld a, TRUE
	ldh [hCGBPalUpdate], a
.next
	inc hl
	dec b
	jr nz, .loop
	pop af
	ldh [rWBK], a
	ret

MobileNativePaletteNPCBound::
; Ordinary actors take priority over borrowed hardware colors. The palette
; scan's original-record views are unbanked; they are not visible game actors.
	push af
	ld a, d
	cp HIGH(STARTOF(WRAMX))
	jr c, .done
	push bc
	push de
	push hl
	ldh a, [rWBK]
	push af
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileOnline]
	and a
	jr z, .restore
	ld a, [wMobileWanted]
	and a
	jr z, .restore
	ld hl, sp + 9 ; original A, full ordinary OAM palette byte
	ld a, [hl]
	and OAM_PALETTE
	ld e, a
	ld d, 0
	ld hl, wMobilePrivatePalBindings
	add hl, de
	ld [hl], 0
.restore
	pop af
	ldh [rWBK], a
	pop hl
	pop de
	pop bc
.done
	pop af
	ret

MobileNativeUpdateOBPalettes::
; ForceUpdateCGBPals already selected bank 5 and enabled OCPS auto-increment.
; Use the native 64-byte upload, selecting private colors only for extant,
; visible leases. No ordinary palette buffer or save metadata is modified.
	push de
	ld d, 0
	lb bc, 8, LOW(rOBPD)
.palette
	push hl
	call .source
rept 1 palettes
	ld a, [hli]
	ldh [c], a
endr
	pop hl
rept 1 palettes
	inc hl
endr
	inc d
	dec b
	jr nz, .palette
	pop de
	ret
.source
	push af
	push bc
	push de
	ld a, [wMobileOnline]
	and a
	jr z, .ordinary
	ld a, [wMobileWanted]
	and a
	jr z, .ordinary
	push hl
	ld a, d
	ld e, a
	ld d, 0
	ld hl, wMobilePrivatePalBindings
	add hl, de
	ld a, [hl]
	and a
	jr z, .no_owner
	cp MOBILE_PEERS + 1
	jr nc, .no_owner
	dec a
	push af ; pool index
	ld e, a
	ld hl, wMobileSavedFacings
	add hl, de
	ld a, [hl]
	and a
	jr z, .not_visible
	cp NUM_OBJECT_STRUCTS
	jr nc, .not_visible
; slot * 34, bounded before pointer arithmetic
	ld l, a
	ld h, 0
	add hl, hl
	ld b, h
	ld c, l
rept 4
	add hl, hl
endr
	add hl, bc
	ld bc, wObjectStructs + OBJECT_MAP_OBJECT_INDEX
	add hl, bc
	ld a, 1
	ldh [rWBK], a
	ld a, [hl]
	cp TEMP_OBJECT
	jr nz, .not_visible
	ld bc, OBJECT_FLAGS1 - OBJECT_MAP_OBJECT_INDEX
	add hl, bc
	bit INVISIBLE_F, [hl]
	jr nz, .not_visible
	inc hl
	ld a, [hli]
	and HIGH_PRIORITY | FROZEN | OFF_SCREEN
	cp HIGH_PRIORITY | FROZEN
	jr nz, .not_visible
	ld a, [hl] ; OBJECT_PALETTE
	and OAM_PALETTE
	ld hl, sp + 5 ; saved D, the hardware color being uploaded
	cp [hl]
	jr nz, .not_visible
	pop af
	push af
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld hl, sp + 1
	ld a, [hl]
	farcall MobileNativePeerPointer
	ld de, 54
	add hl, de
	ld a, [hl]
	cp 1
	jr nz, .not_visible
	pop af
	pop hl ; discard ordinary source, return a private color pointer
	ld hl, wMobilePrivatePalColors
	ld bc, 1 palettes
	rst AddNTimes
	jr .restore
.not_visible
	pop af
.no_owner
	pop hl
.restore
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
.ordinary
	pop de
	pop bc
	pop af
	ret

MobileNativePaletteCodeEnd::
ASSERT MobileNativePaletteCodeEnd - MobileNativePaletteCodeStart <= 1536
