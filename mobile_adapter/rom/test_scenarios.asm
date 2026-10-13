; Optional test-only events, selected by declarative ROM configuration. All
; map/object initialization is executed by the game's ordinary loader.
if DEF(MOBILE_ADAPTER)
INCLUDE "mobile_adapter/build-mobile-test/acceptance-scenarios.asm"
else
INCLUDE "mobile_adapter/build-test/acceptance-scenarios.asm"
endc

AcceptanceSelectMap::
	ld a, -1
	ldh [hAcceptanceLighting], a
	ld a, [wMapGroup]
	cp GROUP_GOLDENROD_GAME_CORNER
	jr nz, .destination
	ld a, [wMapNumber]
	cp MAP_GOLDENROD_GAME_CORNER
	jr nz, .destination
	ld hl, AcceptanceScenarioID
	ld a, BANK(AcceptanceScenarioID)
	call GetFarByte
	cp (AcceptanceScenarioPointersEnd - AcceptanceScenarioPointers) / 2
	jr c, .valid
	xor a
.valid
	add a
	ld e, a
	ld d, 0
	ld hl, AcceptanceScenarioPointers
	add hl, de
	ld a, [hli]
	ld [wMapScriptsPointer], a
	ld a, [hl]
	ld [wMapScriptsPointer + 1], a
	ld a, BANK(AcceptanceScenarioPointers)
	ld [wMapScriptsBank], a
	ld hl, AcceptanceLighting
	ld a, BANK(AcceptanceLighting)
	call GetFarByte
	ldh [hAcceptanceLighting], a
	ret
.destination
	ld a, [wMapGroup]
	cp GROUP_PLAYERS_HOUSE_2F
	ret nz
	ld a, [wMapNumber]
	cp MAP_PLAYERS_HOUSE_2F
	ret nz
	ld a, BANK(AcceptanceEmptyDestination)
	ld [wMapScriptsBank], a
	ld a, LOW(AcceptanceEmptyDestination)
	ld [wMapScriptsPointer], a
	ld a, HIGH(AcceptanceEmptyDestination)
	ld [wMapScriptsPointer + 1], a
	ret

SECTION "Mobile Acceptance Lighting", HRAM
hAcceptanceLighting:: db
