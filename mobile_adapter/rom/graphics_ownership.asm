; Native tile transfers always own their destinations. Invalidate only private
; avatar presentation when a transfer overlaps one of its physical VRAM slots.
SECTION "Mobile Native Graphics Ownership", ROMX, BANK[MOBILE_ROM_BANK + 5]

MACRO mobile_graphics_overlap
; DE = native start, HL = exclusive native end. A completed transfer cannot
; overlap [start,end) unless both strict interval comparisons hold.
	ld a, d
	cp HIGH(\2)
	jr c, .upper_ok\@
	jp nz, \3
	ld a, e
	cp LOW(\2)
	jp nc, \3
.upper_ok\@
	ld a, h
	cp HIGH(\1)
	jp c, \3
	jp nz, .invalidate
	ld a, l
	cp LOW(\1)
	jp c, \3
	jp nz, .invalidate
ENDM

MobileNativeGraphicsRequest::
; Native WriteVCopyRegistersToHRAM: HL destination, C output tile count.
; Preserve every native register, selector and flag, including offline calls.
	push af
	push bc
	push de
	push hl
	ldh a, [rWBK]
	push af
	ld a, [wMobileWanted]
	and a
	jp z, .restore
	ld a, c
	and a
	jp z, .restore
	ld d, h
	ld e, l
	ld h, 0
	ld l, c
rept 4
	add hl, hl
endr
	add hl, de
	jr nc, .end_ready
	ld hl, $ffff
.end_ready
	ldh a, [rVBK]
	and 1
	jr nz, .bank_one
	mobile_graphics_overlap $8640, $86d0, .restore
	jp .restore
.bank_one
	mobile_graphics_overlap $83f0, $8400, .second_slot
.second_slot
	mobile_graphics_overlap $87f0, $8800, .third_slot
.third_slot
	mobile_graphics_overlap $8ff0, $9000, .restore
	jp .restore
.invalidate
	ld a, 1
	ld [wMobileNativeGraphicsFence], a
	ld a, [wMobileVirtualHasPrivateColors]
	and a
	jr z, .colors_ready
	ld a, TRUE
	ldh [hCGBPalUpdate], a
.colors_ready
	xor a
	ld [wMobileVirtualStage], a
	ld [wMobileVirtualPreviousOAM], a
	ld [wMobileVirtualPublished], a
	ld [wMobileVirtualHasPrivateColors], a
	ld [wMobileVirtualQueuedPeer], a
	ld [wMobileVirtualWindowOwners], a
	ld [wMobileVirtualWindowOwners + 1], a
	ld [wMobileVirtualWindowOwners + 2], a
.restore
	pop af
	ldh [rWBK], a
	pop hl
	pop de
	pop bc
	pop af
	ret

MobileNativeGraphicsRelease::
; Scripts, menus and native animation structs retain their temporary graphics.
; No wall-clock timeout can hand those resources back while the game uses them.
	ldh a, [hInMenu]
	and a
	jr nz, .held
	ldh a, [rWBK]
	push af
	ld a, 1
	ldh [rWBK], a
	ld a, [wScriptRunning]
	and a
	jr nz, .restore_held
	ld a, [wPanningAroundTinyMap]
	and a
	jr nz, .restore_held
for actor, 1, NUM_SPRITE_ANIM_STRUCTS + 1
	ld a, [wSpriteAnim{d:actor} + SPRITEANIMSTRUCT_INDEX]
	and a
	jr nz, .restore_held
endr
	xor a
	ld [wMobileNativeGraphicsFence], a
	pop af
	ldh [rWBK], a
	xor a
	ret
.restore_held
	pop af
	ldh [rWBK], a
.held
	ld a, 1
	and a
	ret
