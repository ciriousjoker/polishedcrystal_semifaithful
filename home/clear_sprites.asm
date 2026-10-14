ClearSprites::
; Erase OAM data
	ld hl, wShadowOAM
	ld bc, wShadowOAMEnd - wShadowOAM
	xor a
if DEF(MOBILE_ADAPTER)
	farcall MobileClearNormalSourceBytes
else
	rst ByteFill
endc
	ret

ClearNormalSprites::
	ldh a, [hUsedOAMIndex]
	ld c, a
	; a = OAM_SIZE - a
	cpl
	add OAM_SIZE + 1
	ld h, HIGH(wShadowOAM)
	ld l, a
	xor a
	ld b, a
if DEF(MOBILE_ADAPTER)
	farcall MobileClearNormalSourceBytes
else
	rst ByteFill
endc
	ret

HideSprites::
; Set all OAM y-positions to 160 to hide them offscreen
	ld hl, wShadowOAM
	ld b, OAM_COUNT
HideSpritesInRange::
if DEF(MOBILE_ADAPTER)
	farcall MobileInvalidateNormalSource
endc
	ld de, OBJ_SIZE
	ld a, OAM_YCOORD_HIDDEN
.loop
	ld [hl], a
	add hl, de
	dec b
	jr nz, .loop
	ret

HidePlayerSprite::
; Set player sprite to 160 to hide it offscreen
	ld h, HIGH(wShadowOAM)
	ld a, [wPlayerCurrentOAMSlot]
	ld l, a
if DEF(MOBILE_ADAPTER)
	ld b, 4
	jr HideSpritesInRange
else
	ld de, OBJ_SIZE
	ld b, 4
	ld a, OAM_YCOORD_HIDDEN
.loop
	ld [hl], a
	add hl, de
	dec b
	jr nz, .loop
	ret
endc


FadeToMenu_BackupSprites::
	call FadeToMenu
BackupSprites::
; Copy wShadowOAM to wShadowOAMBackup
	ldh a, [rWBK]
	push af
	ld a, BANK(wShadowOAMBackup)
	ldh [rWBK], a
	ld hl, wShadowOAM
	ld de, wShadowOAMBackup
	ld bc, wShadowOAMEnd - wShadowOAM
	rst CopyBytes
	pop af
	ldh [rWBK], a
	ret

RestoreSprites::
	; Copy wShadowOAMBackup to wShadowOAM
	ldh a, [rWBK]
	push af
	ld a, BANK(wShadowOAMBackup)
	ldh [rWBK], a
	ld hl, wShadowOAMBackup
	ld de, wShadowOAM
	ld bc, wShadowOAMEnd - wShadowOAM
if DEF(MOBILE_ADAPTER)
	farcall MobileRestoreNormalSourceBytes
else
	rst CopyBytes
endc
	pop af
	ldh [rWBK], a
	ret
