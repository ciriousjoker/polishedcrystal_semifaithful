MACRO mobile_actual_oam_dma_latch
; A is the final actual DMA page. All known callers run before interrupt EI or
; in LCD interrupt code, so both resident bytes belong to this transfer. An
; optional register is caller-owned scratch that is overwritten before DMA;
; without it, preserve all registers and flags for the direct menu DMA caller.
	if _NARG
	ld \1, a
	else
	push af
	endc
	ld [wMobileActualOAMPage], a
	ldh a, [hVBlankCounter]
	ld [wMobileActualOAMFrame], a
	if _NARG
	ld a, \1
	else
	pop af
	endc
ENDM

MACRO mobile_actual_native_oam_setup
; BC has the same final native values; B is scratch only before this load.
	ld a, HIGH(wShadowOAM)
	mobile_actual_oam_dma_latch b
	lb bc, 40 + 1, LOW(rDMA)
ENDM

MACRO mobile_virtual_capture_kind
; Classify physical world-object sprites without changing their simulation.
; Character and unknown sprites retain NPC priority. The renderer, not maps,
; owns this built-in distinction.
	ld a, [bc]
	cp SPRITE_BALL_CUT_TREE
	jr z, .ground\@
	cp SPRITE_BOULDER_ROCK
	jr z, .ground\@
	cp SPRITE_BLANK_FRUIT
	jr z, .ground\@
	xor a
	jr .kind\@
.ground\@
	ld a, $20
.kind\@
	ld [wMobileVirtualCaptureClass], a
ENDM

ASSERT LOW(wMobileVirtualDescriptors) + NUM_OBJECT_STRUCTS * 8 <= $100

MACRO mobile_virtual_capture
; Unified immutable eight-byte producer ABI. Explicit $ff overflow sentinel.
; Capture authorization is separate from composition.
 ld a, [wMobileNativeCaptureActive]
 cp 1
 jp nz, .capture_done\@
 push bc
 push hl
 ld a, [wMobileVirtualIndex]
 add a
 add a
 add a
 add LOW(wMobileVirtualDescriptors)
 ld l, a
 ld h, HIGH(wMobileVirtualDescriptors)
 ldh a, [hCurSpriteXPixel]
 ld [hli], a
 ldh a, [hCurSpriteYPixel]
 ld [hli], a
 ldh a, [hCurSpriteTile]
 ld [hli], a
 ld [hl], d
 inc hl
 inc hl ; bounded facingID stored at the native validation site
 ldh a, [hCurSpriteOAMFlags]
 ld [hli], a
 ldh a, [hUsedSpriteTile]
 and a
 jr z, .bad_count\@
 cp 32
 jr nc, .bad_count\@
 ld b, a
 cp 4
 jr nz, .class\@
 set 6, b
.class\@
 ld a, [wMobileVirtualCaptureClass]
 or b
 ld [hli], a ; pending, bit7 clear
 ld [hl], $ff
 if \1
; Native mobile_native_oam_bounds has already proved raw capacity.
 ldh a, [hUsedSpriteTile]
 add a
 add a
 sub OBJ_SIZE
 ld b, a
 ld a, c
 sub b
 ld [hl], a
 else
; Native overflow edge still requires immutable source count validation above.
 dec hl
 set 7, [hl]
 endc
 jr .restore\@
.bad_count\@
 ld [hl], 0
 inc hl
 ld [hl], $ff
.restore\@
 pop hl
 pop bc
.capture_done\@
 ldh a, [hUsedSpriteTile]
ENDM

MACRO mobile_virtual_complete_capture
 ld a, [wMobileNativeCaptureActive]
 cp 1
 jr nz, .done\@
 push hl
 ld a, [wMobileVirtualIndex]
 add a
 add a
 add a
 add LOW(wMobileVirtualDescriptors + 7)
 ld l, a
 ld h, HIGH(wMobileVirtualDescriptors)
 ld a, [hl]
 cp $ff
 jr z, .restore\@
 dec hl
 farcall MobileOwnCompletedNativeTuple
 set 7, [hl]
.restore\@
 pop hl
.done\@
ENDM

MACRO mobile_virtual_bin_coordinate
; Shift the conservative buckets with the current native scene. Clamp a clipped
; top edge instead of wrapping an unsigned coordinate into another counter.
	ld e, a
	ld a, [wMobileVirtualLines + 15]
	ld d, a
	ld a, e
	sub d
	jr nc, :+
	xor a
:
ENDM

; This resident integration avoids the general farcall's stack work
; for a validated rectangle. Carry selects private output, otherwise native.
; Both upper programs return via Bankswitch.
MACRO mobile_native_rectangle_call
; Keep the caller-bank preparation in addon ROMX. The resident bridge must
; execute the bank switch and following jump together, outside switched ROMX.
; All prefix instructions preserve carry, which selects the output page.
	ldh a, [hROMBank]
	ld [wMobileVirtualRectangleReturnBank], a
	ld a, BANK(MobileVirtualWriteRectangleDispatch)
	call MobileNativeRectangleTrampoline
ENDM

MACRO mobile_native_rectangle_trampoline
MobileNativeRectangleTrampoline::
	rst Bankswitch
	jp MobileVirtualWriteRectangleDispatch
ENDM

; Shared exact scheduler mathematics for ordinary and resource-only admission.
MACRO mobile_virtual_advance_class
; Start after the first accepted ordinal next frame, including skipped actors.
	ld a, [wMobileVirtualSelectedOrdinal]
	ld b, a
	ld a, [wMobileVirtualNPCCount]
	ld c, a
	ld a, b
	cp c
	jr nc, .ground
	ld hl, wMobileVirtualNativeNext
	ld a, [hl]
	cp 255
	ret nz
	ld a, [wMobileVirtualNativeRotation]
	add b
	jr .wrap
.ground
	sub c
	ld b, a
	ld hl, wMobileVirtualGroundNext
	ld a, [hl]
	cp 255
	ret nz
	ld a, [wMobileVirtualOrderCount]
	sub c
	ld c, a
	ld a, [wMobileVirtualGroundRotation]
	add b
.wrap
	cp c
	jr c, .ready
	sub c
	jr .wrap
.ready
	inc a
	ld [hl], a
	ret

ENDM

; Shared live peer validation, with bank-local pointer and row checker entries.
MACRO mobile_virtual_gather_peer
; Read current eligibility and pose without admitting or publishing a figure.
	ld b, a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	xor a
	ld [wMobileVirtualPeerGatherValid], a
	ld a, b
; A pool index, cache and colors validated in bounded background work.
	ld [wMobileBudgetIndex], a
	inc a
	ld [wMobileVirtualGroupOwner], a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, [wMobileBudgetIndex]
	call \1
	push hl
	ld bc, MOBILE_PEER_VISIBLE
	add hl, bc
	ld a, [hl]
	and a
	jr z, .initial_invalid
	ld bc, MOBILE_PEER_DRAW_VALID - MOBILE_PEER_VISIBLE
	add hl, bc
	ld a, [hl]
	and a
	jr z, .initial_invalid
	pop hl
	jr .initial_valid
.initial_invalid
	pop hl
	ret
.row_unavailable
	pop hl
	ret
.initial_valid
; Revalidate map/status at drawing time. A turn or script can postpone the
; projection job while a new network record already describes another map.
	push hl
	ld a, [wMobileVirtualMapGroup]
	ld b, a
	ld a, [wMobileVirtualMapNumber]
	ld c, a
	ld de, 6
	add hl, de
	ld a, [hli]
	cp b
	jr nz, .ineligible
	ld a, [hli]
	cp c
	jr nz, .ineligible
	ld a, [wMobileInstanceKey]
	cp [hl]
	jr nz, .ineligible
	inc hl
	ld a, [wMobileInstanceKey + 1]
	cp [hl]
	jr nz, .ineligible
	ld de, 8
	add hl, de
	ld a, [hl]
	cp 1
	jr nz, .ineligible
	inc hl
	ld a, [hl]
	ld b, a
	ld [wMobileAvatarTile], a
	ld de, MOBILE_PEER_LOADED_SPRITE - 18
	add hl, de
	ld a, [hl]
	cp b
	jr nz, .ineligible
; The committed pose may retain its previous movement mode while new graphics
; prepare. Its own uploaded tiles, facing and color remain authoritative.
	pop hl
	jr .eligible
.ineligible
	pop hl
	ret
.eligible
; Collect the committed pose in one bank-2 view. Subsequent admission only
; touches private bank-5 presentation data; no repeated peer lookup is needed.
	ld a, 1
	ldh [rWBK], a
	ld a, [wScriptRunning]
	ld c, a
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	ld a, c
	cp $ff
	jr nz, .outside_menu
	push hl
	ld bc, MOBILE_PEER_SCREEN_X
	add hl, bc
	ld a, [hl]
	pop hl
	cp 9 * TILE_WIDTH
	ret nc
.outside_menu
	ld bc, MOBILE_PEER_SCREEN_X
	add hl, bc
	ld a, [hli]
	ld [wMobileOAMWriteCursor], a
	ld a, [hl]
	ld [wMobileOAMDemand], a
if _NARG >= 3
; A retained aligned plan already proves row geometry; fresh NativeView must
; still validate it before publication. Keep this committed peer pointer live.
else
	push hl
	ld [wMobileVirtualRectangleTop], a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, [wMobileVirtualStage]
	cp 3
	jr z, .row_available ; only the bounded local prefix precedes the urgent peer
	ld a, [wMobileVirtualFastAdmission]
	cp 2
	jr nz, .row_available
	ld a, [wMobileVirtualForceFull]
	and a
	jr nz, .row_available
	call \2
	jp c, .row_unavailable
.row_available
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
	pop hl
endc
	ld bc, MOBILE_PEER_NATIVE_STAGE - MOBILE_PEER_SCREEN_Y
	add hl, bc
	ld a, [hl]
	cp 7
	ret nz
	ld bc, MOBILE_PEER_NATIVE_FACING - MOBILE_PEER_NATIVE_STAGE
	add hl, bc
	ld a, [hli]
	ld [wMobileVirtualFrame], a
	inc hl
	ld a, [hl]
	and a
	ret z
	ld [wMobileVirtualCacheHit], a
	ld a, [wMobileBudgetIndex]
	ld e, a
	inc a
	ld b, a
	ld d, 0
	ld hl, wMobileVirtualWindowOwners
	add hl, de
	ld a, [hl]
	cp b
	ret nz
	ld a, e
	ld [wMobileVirtualWindow], a
	ld a, $ff
	ld [wMobileAvatarPalette], a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, 1
	ld [wMobileVirtualPeerGatherValid], a
	ret

ENDM

; Shared resource policies for ordinary rendering and plan-only construction.
MACRO mobile_virtual_build_order
; Cache only stable membership and class. Positions, poses, colors and row
; admission always come from this frame's immutable native descriptors.
	ld a, 255
	ld [wMobileVirtualNativeNext], a
	ld [wMobileVirtualGroundNext], a
	ld a, [wMobileVirtualOrderValid]
	ld b, a
for native_slot, 1, NUM_OBJECT_STRUCTS
	ld a, [wMobileVirtualDescriptors + native_slot * 8 + 6]
	and $3f
	ld c, a
	ld a, [wMobileVirtualMembership + native_slot - 1]
	xor c
	jr z, .same{d:native_slot}
	ld b, 0
	ld a, c
	ld [wMobileVirtualMembership + native_slot - 1], a
.same{d:native_slot}
endr
	ld a, b
	cp 1
	jr z, .rotate
	xor a
	ld [wMobileVirtualOrderCount], a
	ld [wMobileVirtualClass], a
.class
	ld b, 1
	ld c, NUM_OBJECT_STRUCTS - 1
	ld hl, wMobileVirtualMembership
.scan
	ld a, [hli]
	ld e, a
	and $1f
	jr z, .next
	ld a, [wMobileVirtualClass]
	xor e
	and $20
	jr nz, .next
	push hl
	ld a, [wMobileVirtualOrderCount]
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualOrderBase
	add hl, de
	ld [hl], b
	ld hl, wMobileVirtualOrderCount
	inc [hl]
	pop hl
.next
	inc b
	dec c
	jr nz, .scan
	ld a, [wMobileVirtualClass]
	and a
	jr nz, .complete
	ld a, [wMobileVirtualOrderCount]
	ld [wMobileVirtualNPCCount], a
	ld a, $20
	ld [wMobileVirtualClass], a
	jr .class
.complete
	ld a, 255
	ld [wMobileVirtualOrderNativeRotation], a
	ld [wMobileVirtualOrderGroundRotation], a
	ld a, 1
	ld [wMobileVirtualOrderValid], a
.rotate
; An incomplete transaction does not advance fairness rotations. Retain its
; membership-only ordering; positions, quads and admission remain frame-local.
	ld a, [wMobileVirtualNativeRotation]
	ld hl, wMobileVirtualOrderNativeRotation
	cp [hl]
	jr nz, .rotate_changed
	ld a, [wMobileVirtualGroundRotation]
	ld hl, wMobileVirtualOrderGroundRotation
	cp [hl]
	ret z
.rotate_changed
	ld a, [wMobileVirtualNativeRotation]
	ld [wMobileVirtualOrderNativeRotation], a
	ld a, [wMobileVirtualGroundRotation]
	ld [wMobileVirtualOrderGroundRotation], a
	ld de, wMobileVirtualOrder
	ld hl, wMobileVirtualOrderBase
	ld a, [wMobileVirtualNPCCount]
	ld c, a
	ld a, [wMobileVirtualNativeRotation]
	call .copy_class
	ld a, [wMobileVirtualNPCCount]
	add LOW(wMobileVirtualOrderBase)
	ld l, a
	ld a, [wMobileVirtualNPCCount]
	ld b, a
	ld a, [wMobileVirtualOrderCount]
	sub b
	ld c, a
	ld a, [wMobileVirtualGroundRotation]
.copy_class
	ld b, a
	ld a, c
	and a
	ret z
	ld a, l
	ld [wMobileVirtualClass], a
	ld a, b
.modulo
	cp c
	jr c, .position
	sub c
	jr .modulo
.position
	add l
	ld b, a
	ld a, c
	add l
	ld l, b
	ld b, c
	ld c, a
.copy
	ld a, [hli]
	ld [de], a
	inc e
	ld a, l
	cp c
	jr nz, .next_copy
	ld a, [wMobileVirtualClass]
	ld l, a
.next_copy
	dec b
	jr nz, .copy
	ret
ENDM

MACRO mobile_virtual_choose_private_palette
; Reserve the local color and every exact avatar source before borrowing a
; currently unused hardware slot. Rotate loans across all eight native slots.
	ld a, [wMobileVirtualPaletteUsed]
	ld b, a
	ld a, [wMobileVirtualNativeAvatarColors]
	or b
	ld b, a
; Prefer a genuinely unused native hardware color. Borrowing a visible NPC's
; slot while a free one exists would create artificial palette contention.
	ld a, BANK(wUsedObjectPals)
	ldh [rWBK], a
	ld a, [wUsedObjectPals]
	ld c, a
	ld a, MOBILE_BACKUP_BANK
	ldh [rWBK], a
	ld a, c
	or b
	cp 255
	jr nz, .free_color_table
	ld a, b
	cp 255
	jr z, .color_full
	ld a, [wMobileVirtualPaletteRotation]
	and 7
	ld c, a
.color_probe
	ld e, c
	ld d, 0
	ld hl, \1
	add hl, de
	ld a, [hl]
	and b
	ld a, c
	jr z, .free_color_found
	inc a
	and 7
	ld c, a
	jr .color_probe
.free_color_table
	ld e, a
	ld d, 0
	ld hl, \2
	add hl, de
	ld a, [hl]
	cp 8
	jr nc, .color_full
.free_color_found
	ld [wMobileAvatarPalette], a
	and a
	ret
.color_full
	scf
	ret
ENDM

MACRO mobile_virtual_free_palette_table
	DEF free_color = 255
for palette_mask, 1 << 8
	REDEF free_color = 255
	for virtual_color, 8
		if !(palette_mask & (1 << virtual_color)) && free_color == 255
			REDEF free_color = virtual_color
		endc
	endr
	db free_color
endr
	PURGE free_color
ENDM

MACRO mobile_virtual_admit_aligned_rectangle
; Exact only under AlignedScene's proof. Check both touched bins before
; changing either. The clipped bottom bin never consumes invisible scanlines.
	ld a, [wMobileVirtualRectangleCount]
	cp OAM_COUNT / 4
	jr nc, .rejected
	ld a, [wMobileVirtualRectangleTop]
	ld c, a
	and a
	jr z, .record
	cp SCREEN_HEIGHT_PX + OAM_Y_OFS
	jr nc, .record
	ld a, [wMobileVirtualLines + 15]
	ld b, a
	ld a, c
	sub b
	jr nc, .coordinate
	xor a
.coordinate
	ld b, a
	swap a
	and $f
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualLines + 16
	add hl, de
	ld a, [hl]
	cp 5
	jr nc, .rejected
	ld a, e
	cp 9
	jr nc, .one_bin
	ld a, b
	and $f
	jr z, .one_bin
	inc hl
	ld a, [hl]
	cp 5
	jr nc, .rejected
	inc [hl]
	dec hl
.one_bin
	inc [hl]
.record
	ld a, [wMobileVirtualRectangleCount]
	ld e, a
	ld d, 0
	ld hl, wMobileVirtualLines
	add hl, de
	ld [hl], c
	ld hl, wMobileVirtualRectangleCount
	inc [hl]
	and a
	ret
.rejected
	scf
	ret
ENDM

; Canonical private avatar emission shared by ordinary and cached-plan paths.
MACRO mobile_virtual_write_peer_quads
; DE screen origin, B mirrored tile base, C complete palette/bank/flip flags.
; A rectangle's output can be written once after admission, with no scratch copy.
for quad, 4
	ld a, d
if quad >= 2
	add 8
endc
	ld [hli], a
	ld a, e
if quad & 1
	add 8
endc
	ld [hli], a
	ld a, b
if quad & 1
	xor 1
endc
if quad >= 2
	add 2
endc
; Logical third-window tile ordinals map to distinct physical banks. Flip
; swaps columns before this mapping, preserving each quadrant's own pixels.
	cp $6d
	jr c, .ordinary_tile{d:quad}
	cp $70
	jr nc, .ordinary_tile{d:quad}
	cp $6e
	jr c, .third_first{d:quad}
	ld a, $7f
	jr z, .third_tile{d:quad}
	ld a, $ff
	jr .third_tile{d:quad}
.third_first{d:quad}
	ld a, $3f
.third_tile{d:quad}
	ld [hli], a
	ld a, c
	or OAM_BANK1
	ld [hli], a
	jr .quad_done{d:quad}
.ordinary_tile{d:quad}
	ld [hli], a
	ld [hl], c
	inc hl
.quad_done{d:quad}
endr
	ret

ENDM

; Commit identical palette ownership and one-shot DMA tokens in either bank.
MACRO mobile_virtual_publish_page
	ld a, [wMobileStableSourceValid]
	cp 1
	jp nz, \1
	ld b, 141
	ld a, [wMobileVirtualLength]
	cp OAM_SIZE
	jr nz, .publish_deadline
	ld b, 142 ; a full page needs no unused-Y suffix stores
.publish_deadline
	ldh a, [rLY]
	cp b
	ret nc ; no commit-visible state changes without the complete return window
	ld a, [wMobileVirtualLength]
	cp OAM_SIZE + 1
	jp nc, \1
	and OBJ_SIZE - 1
	jp nz, \1
; Publish the already complete private DMA page, never another full OAM copy.
; The one-shot frame token keeps menus, stale states and new scenes native.
	ld hl, wMobileVirtualBindings
	xor a
rept 8
	or [hl]
	inc hl
endr
	ld [wMobileVirtualHasPrivateColors], a
	and a
	jr z, .colors_ready
	ld a, TRUE
	ldh [hCGBPalUpdate], a
.colors_ready
	ld a, [wMobileVirtualLength]
	rrca ; two-byte entry for each four-byte OAM quad
	ld e, a
	ld d, 0
	ld hl, .suffix_table
	add hl, de
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld a, 160
	jp hl
; Each omitted Y costs sixteen clocks, independent of the suffix length.
for quad, OAM_COUNT
.suffix{d:quad}
	ld [wMobileVirtualOutput + quad * OBJ_SIZE], a
endr
.suffix40
	xor a
for hardware_color, 8
 ld a, [wMobileVirtualBindings + hardware_color]
 and a
 jr nz, .peer_source{d:hardware_color}
 ld a, hardware_color
 jr .source_ready{d:hardware_color}
.peer_source{d:hardware_color}
 add 7
.source_ready{d:hardware_color}
 ld [wMobilePrivatePalBindings + hardware_color], a
endr
	xor a
	ld [wMobileVirtualStage], a
	ldh a, [hVBlankCounter]
	ld [wMobileVirtualHeaderOAM], a
	ld a, 1
	ld [wMobileStablePendingValid], a
	ld [wMobileVirtualPreviousOAM], a
	ld [wMobileVirtualPublished], a
	ret

.suffix_table
for quad, OAM_COUNT + 1
	dw .suffix{d:quad}
endr

ENDM
