; Keep native palette metadata and buffers independent of remote rendering.
SECTION "Mobile Native Palettes", ROMX, BANK[MOBILE_ROM_BANK]
MobileNativePaletteCodeStart::

MobileEmergencyInput::
	call MobileEmergencyKeys
	call MobileNativePaletteFadeDirty
	jp MobileVirtualCommitPose

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
	jp MobileReleaseNoIME
.already_released
	xor a
	ld [wMobileOnline], a
	ld [wMobilePeerCount], a
	ret

; Foreground only: DelayFrame has completed its VBlank wait. Preserve all
; registers and WBK. This routine never waits, calls DelayFrame, or reenters.
; Select+B toggles the lobby; adding A runs the one-shot transport diagnostic.



MobileNativeClearPaletteSource::
	ret ; no ordinary palette or object lease exists in this renderer

MobileNativePrivatePalette::
; A base color, C previous hardware slot. Native save metadata is never written.
	ld [wMobilePrivatePalRequest], a
	ld a, c
	ld [wMobilePrivatePalSlot], a
	ldh a, [rWBK]
	push af
; The caller has already rejected every exact native source. Hardware color
; ownership belongs to the renderer; this routine only maintains a private copy.
	jr .borrow
.borrow
; This is a private color cache, not a native palette lease. Hardware ownership
; is decided from current draw groups by the compositor, so scanning thirteen
; native object records here cannot inform the decision and wastes idle time.
	ld a, 1
	ld [wMobilePrivatePalSlot], a
	jp .cached
.cached
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	call MobileNativePeerPointer
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
; Look up the current canonical avatar colors directly. This read-only native
; selector accounts for time, darkness and overcast. Avoid the general allocator,
; its target/active buffer backup, endpoint recalculation and color reapplication.
; The outer guard preserves native request scratch; only wPalState needs saving.
	ld a, 1
	ldh [rWBK], a
	ld a, [wPalState]
	push af
	ld a, CURR_PALSTATE
	ld [wPalState], a
	xor a
	ld [wNeededObjPalGlow], a
	ld a, [wMobilePrivatePalRequest]
	farcall LookupOBPalette
	pop af
	ld [wPalState], a
	push hl
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	call .color_pointer
	ld d, h
	ld e, l
	pop hl
	ld bc, 1 palettes
	ld a, BANK(LookupOBPalette)
	call FarCopyBytes
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
	call MobileNativePeerPointer
	ld bc, 54
	add hl, bc
	ld [hl], 1
	call MobileNativeClearPaletteBinding
	jr .success
.success
	pop af
	ldh [rWBK], a
	ld a, [wMobilePrivatePalSlot]
	and a
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

MobileNativeUpdateOBPalettes::
	ldh a, [hInMenu]
	and a
	jp nz, .virtual_ordinary
	ld a, [wMobileLocalValid]
	and a
	jp z, .virtual_ordinary
	ld a, [wMobileOnline]
	and a
	jp z, .virtual_ordinary
	ld a, [wMobileWanted]
	and a
	jp z, .virtual_ordinary
	push hl
	call MobileVirtualFrameValid
	pop hl
	jp z, .virtual_ordinary
	ld a, [wMobileVirtualHasPrivateColors]
	and a
	jp z, .virtual_ordinary
; Keep immutable private RGB and OAM together. Selecting eight bounded
; sources here avoids rebuilding all 64 native color bytes in idle time.
	ld c, LOW(rOBPD)
for hardware_color, 8
	ld hl, wOBPals2 + hardware_color palettes
	ld a, [wMobilePrivatePalBindings + hardware_color]
	and a
	jr z, .frame_color{d:hardware_color}
	cp MOBILE_PEERS + 1
	jr nc, .frame_color{d:hardware_color}
	dec a
	add a
	add a
	add a
	add LOW(wMobileVirtualFrameColors)
	ld l, a
	adc HIGH(wMobileVirtualFrameColors)
	sub l
	ld h, a
.frame_color{d:hardware_color}
rept 1 palettes
	ld a, [hli]
	ldh [c], a
endr
endr
	ld hl, wOBPals2 + 8 palettes
	ld b, 0
	ret
.virtual_ordinary
	lb bc, 4, LOW(rOBPD)
.virtual_upload
rept 2 palettes
	ld a, [hli]
	ldh [c], a
endr
	dec b
	jr nz, .virtual_upload
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
	call MobileNativePrivatePalette
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

MobileNativePaletteCodeEnd::
ASSERT MobileNativePaletteCodeEnd - MobileNativePaletteCodeStart <= 1536
