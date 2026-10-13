PUSHS
SECTION "Mobile Adapter Menu", ROMX, BANK[MOBILE_ROM_BANK + 6]
; Explicit prototype lobby controls. Select+A opens this outside MobileTick.
MobileMenuInput::
	ldh a, [hJoypadDown]
	and PAD_SELECT | PAD_B
	cp PAD_SELECT | PAD_B
	call z, MobileMenuConsumeKey
	ldh a, [hJoypadDown]
	and PAD_SELECT | PAD_A | PAD_B
	cp PAD_SELECT | PAD_A
	ret nz
	ldh a, [hJoyPressed]
	and PAD_SELECT | PAD_A
	ret z
	ld a, [wScriptRunning]
	and a
	ret nz
	ldh a, [hInMenu]
	and a
	ret nz
	ld a, [wPlayerStepFlags]
	bit PLAYERSTEP_CONTINUE_F, a
	ret nz
	ld a, [wMobileMenuState]
	and a
	ret nz
	ld a, [wMobileDeferred]
	ld [wMobileMenuDeferred], a
	ld a, 1
	ld [wMobileMenuState], a
	call ClearJoypad
	call OpenText
.menu
	ld hl, .OfflineText
	ld a, [wMobileWanted]
	and a
	jr z, .status
	ld hl, .ConnectingText
	ld a, [wMobileOnline]
	and a
	jr z, .status
	ld hl, .OnlineText
.status
	call PrintText
	ld a, [wMobileOnline]
	and a
	jr z, .options
	ld a, [wMobileMenuDeferred]
	and a
	jr z, .options
	ld hl, .ProtectedText
	cp 2
	jr z, .reason
	ld hl, .CrowdedText
	cp 1
	jr z, .reason
	ld hl, .PausedText
.reason
	call PrintText
.options
	ld hl, .Header
	call LoadMenuHeader
	call VerticalMenu
	push af
	call ExitMenu
	pop af
	jp c, .close
	ld a, [wMenuCursorY]
	cp 1
	jr z, .connect
	cp 2
	jr z, .players
	cp 3
	jr nz, .close
	xor a
	ld [wMobileWanted], a
	jr .close
.connect
	ld a, 1
	ld [wMobileMode], a
	ld [wMobileWanted], a
	xor a
	ld [wMobileCooldown], a
	jr .close
.players
	ld a, [wMobilePlayerId]
	ld [wMobileLocalStaging], a
	ld a, [wMobilePlayerId + 1]
	ld [wMobileLocalStaging + 1], a
	xor a
	ld hl, wMobileLocalStaging + 2
	ld bc, 2 * MOBILE_PEERS
	rst ByteFill
	ld a, [wMobilePeerCount]
	and a
	jr z, .roster
	ld a, MOBILE_WRAM_BANK
	ldh [rWBK], a
for peer, 1, MOBILE_PEERS + 1
	ld a, [wMobilePeerCount]
	cp peer
	jr c, .bank
	ld a, [wMobilePeer{d:peer}Id]
	ld [wMobileLocalStaging + 2 * peer], a
	ld a, [wMobilePeer{d:peer}Id + 1]
	ld [wMobileLocalStaging + 2 * peer + 1], a
endr
.bank
	ld a, 1
	ldh [rWBK], a
.roster
	ld hl, .RosterText
	call PrintText
	call WaitButton
	jp .menu
.close
	call CloseText
	farcall MobileNativeInvalidateHash
	xor a
	ld [wMobileMenuState], a
	ld [wMobileLocalValid], a
	ld [wMobileGraphicsReady], a
	jmp ClearJoypad
.Header
	db MENU_BACKUP_TILES
	menu_coords 0, 0, 15, 9
	dw .Options
	db 1
.Options
	db STATICMENU_CURSOR | STATICMENU_WRAP
	db 4
	db "CONNECT@", "PLAYERS@", "DISCONNECT@", "CANCEL@"
.OfflineText
	text "Mobile: offline."
	line "Lobby: 1 (4 max)."
	done
.ConnectingText
	text "Mobile: connecting"
	line "or reconnecting."
	done
.OnlineText
	text "Mobile: online."
	line "Peers: "
	text_decimal wMobilePeerCount, 1, 1
	text "  Lobby: 1"
	done
.RosterText
	text "YOU: "
	text_decimal wMobileLocalStaging, 2, 5
	line "1: "
	text_decimal wMobileLocalStaging + 2, 2, 5
	text "  2: "
	text_decimal wMobileLocalStaging + 4, 2, 5
	para "3: "
	text_decimal wMobileLocalStaging + 6, 2, 5
	para "Same-map peers"
	line "appear nearby."
	done
.ProtectedText
	text "Avatars paused for"
	line "large map sprites."
	done
.CrowdedText
	text "Avatars paused to"
	line "keep required NPCs."
	done
.PausedText
	text "Avatars paused by"
	line "this game scene."
	done

MobileMenuConsumeKey:
	farcall MobileConsumeKey
	ret
POPS
