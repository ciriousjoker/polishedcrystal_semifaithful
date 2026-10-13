; Included only with MOBILE_TEST_SCENARIOS, in the native intro-menu bank.
; Assembly writes happen during native initialization, never through a host RAM patch.
AcceptanceBoot::
	ld a, -1
	ldh [hAcceptanceLighting], a
	ld a, [wSaveFileExists]
	and a
	jmp nz, AcceptanceContinue
	xor a
	ldh [hBGMapMode], a
	call ResetWRAM_NotPlus
	call ResetWRAM
	farcall ClearSavedObjPals
	call NewGame_ClearTileMapEtc
	ld a, [AcceptanceAvatar]
	ld [wPlayerGender], a
	ld a, [AcceptanceTrainerID]
	ld [wPlayerID], a
	ld a, [AcceptanceTrainerID + 1]
	ld [wPlayerID + 1], a
	ld a, $12
	ld [wSecretID], a
	ld a, $34
	ld [wSecretID + 1], a
	ld hl, AcceptanceName
	ld de, wPlayerName
	ld bc, NAME_LENGTH
	rst CopyBytes
	xor a
	ld [wInitialOptions2], a ; native frame-driven clock, independent of host time
	ld [wStringBuffer2 + 2], a
	ld a, [AcceptanceHour]
	ld [wStringBuffer2 + 1], a
	call SetTimeOfDay
	farcall InitializeEvents
if DEF(MOBILE_ADAPTER)
	ld a, [AcceptanceInitialFault]
	and a
	jr z, .no_fault
	; Deliberate private-state fault initialization, never a host RAM patch.
	ld [wMobileOwner], a
	ld [wMobileWanted], a
	ld [wMobileStep], a
.no_fault
endc
	ld a, SPAWN_HOME
	ld [wDefaultSpawnpoint], a
	ld a, MAPSETUP_WARP
	ldh [hMapEntryMethod], a
	jmp FinishContinueFunction

AcceptanceContinue:
	farcall TryLoadSaveFile
	jr c, .failed
.loaded
	call Continue_FinishReset
	farcall ClearSavedObjPals
	farcall FixPlayerEVsAndStats
	farcall ClockContinue
	ld a, [AcceptanceScenarioLoad]
	and a
	jr z, .ordinary_continue
	ld a, [AcceptanceAvatar]
	ld [wPlayerGender], a
	ld a, [AcceptanceTrainerID]
	ld [wPlayerID], a
	ld a, [AcceptanceTrainerID + 1]
	ld [wPlayerID + 1], a
if DEF(MOBILE_ADAPTER)
	ld a, [AcceptanceInitialFault]
	and a
	jr z, .no_loaded_fault
	ld [wMobileOwner], a
	ld [wMobileWanted], a
	ld [wMobileStep], a
.no_loaded_fault
endc
	; The source-generated seed carries its producer's ordinary objects.
	; Reconstruct this scenario's objects through the native map loader.
	xor a
	ld hl, wObjectStructs
	ld bc, wObjectStructsEnd - wObjectStructs
	rst ByteFill
	ld hl, wMapObjects
	ld bc, wMapObjectsEnd - wMapObjects
	rst ByteFill
	ld a, SPAWN_HOME
	ld [wDefaultSpawnpoint], a
	ld a, MAPSETUP_WARP
	jr .enter
.ordinary_continue
	ld a, MAPSETUP_CONTINUE
.enter
	ldh [hMapEntryMethod], a
	jmp FinishContinueFunction
.failed
.wait
	call DelayFrame
	jr .wait

AcceptanceReadyMark::
	ret
AcceptanceDialogMark::
	ret

; Launcher-writable ROM data only. No host writes to CPU state or game RAM.
AcceptanceConfig::
AcceptanceScenarioID:: db 0
AcceptanceAvatar:: db 0
AcceptanceTrainerID:: dw $3039
AcceptanceHour:: db 12
AcceptanceInitialFault:: db 0
AcceptanceScenarioLoad:: db 0
AcceptanceInteractionFault:: db 0
AcceptanceLighting:: db -1
AcceptanceNPCOverride:: db 0
AcceptanceNPCPosition:: db 0, 0 ; native header Y/X coordinates (including +4)
AcceptanceSpawn::
	map_id GOLDENROD_GAME_CORNER
	db 3, 3 ; native spawn order X/Y, replaced by source-derived launcher data
AcceptanceControlPeers:: ds 9, 0 ; sprite/Y/X triples
AcceptanceConfigEnd::
AcceptanceDarkLighting:: db PALETTE_NITE | IN_DARKNESS
AcceptanceAutoLighting:: db PALETTE_AUTO
AcceptanceFigures:: db SPRITE_CHRIS, SPRITE_KRIS, SPRITE_CRYS, SPRITE_BETA
; Derived values are emitted by the same assembler as the current game.
AcceptanceObjectCount:: dw NUM_OBJECT_STRUCTS
AcceptanceObjectStride:: dw OBJECT_LENGTH
AcceptanceObjectSprite:: dw OBJECT_SPRITE
AcceptanceObjectMap:: dw OBJECT_MAP_OBJECT_INDEX
AcceptanceObjectFacing:: dw OBJECT_FACING
AcceptanceObjectPalette:: dw OBJECT_PALETTE
AcceptanceObjectMapX:: dw OBJECT_MAP_X
AcceptanceObjectMapY:: dw OBJECT_MAP_Y
AcceptanceSimulationFields::
	db OBJECT_SPRITE, OBJECT_MAP_OBJECT_INDEX, OBJECT_MOVEMENT_TYPE
	db OBJECT_WALKING, OBJECT_DIRECTION, OBJECT_STEP_TYPE, OBJECT_STEP_DURATION
	db OBJECT_ACTION, OBJECT_STEP_FRAME, OBJECT_MAP_X, OBJECT_MAP_Y
AcceptanceSimulationFieldsEnd::
AcceptanceName: db "TEST@"
	ds NAME_LENGTH - 5, 0

AcceptanceSave::
	farcall SaveGameData
	farcall SaveCurrentVersion
	ret

AcceptancePauseGameTimer::
	; Save tests compare equal external clock inputs. The ordinary world,
	; interrupts, animations, audio and network continue to advance normally.
	xor a
	ld [wGameTimerPaused], a
	ld hl, wGameTimeCap
	ld bc, wGameTimeFrames + 1 - wGameTimeCap
	rst ByteFill
	ret

AcceptanceLink::
	farcall LinkCommunications
	ret

AcceptanceApplyInteractionFault::
if DEF(MOBILE_ADAPTER)
	ld a, [AcceptanceInteractionFault]
	and a
	ret z
	ld [wMobileOwner], a
	ld [wMobileWanted], a
	ld [wMobileStep], a
endc
	ret

AcceptanceLoadSpawn::
	; Only the explicit bootstrap HOME spawn is configurable. Ordinary warps
	; use SPAWN_N_A and must pass through the unmodified native loader.
	ld a, [wDefaultSpawnpoint]
	cp SPAWN_HOME
	jr z, .selected
	and a
	ret
.selected
	push hl
	push de
	ld hl, AcceptanceSpawn
	ld de, wMapGroup
	ld a, [hli]
	ld [wMapGroup], a
	ld a, [hli]
	ld [wMapNumber], a
	ld a, [hli]
	ld [wXCoord], a
	ld a, [hli]
	ld [wYCoord], a
	pop de
	pop hl
	scf
	ret

AcceptanceApplyObjectConfig::
	ld a, [AcceptanceNPCOverride]
	and a
	jr z, .controls
	ld a, [AcceptanceNPCPosition]
	ld [wMap1Object + MAPOBJECT_Y_COORD], a
	ld a, [AcceptanceNPCPosition + 1]
	ld [wMap1Object + MAPOBJECT_X_COORD], a
.controls
if !DEF(MOBILE_ADAPTER)
	ld a, [AcceptanceScenarioID]
	cp (AcceptanceScenarioPointersEnd - AcceptanceScenarioPointers) / 2
	jr c, .valid_scenario
	xor a
.valid_scenario
	ld e, a
	ld d, 0
	ld hl, AcceptanceScenarioControls
	add hl, de
	ld a, BANK(AcceptanceScenarioControls)
	call GetFarByte
	and a
	ret z
	ld hl, AcceptanceScenarioCounts
	add hl, de
	ld a, BANK(AcceptanceScenarioCounts)
	call GetFarByte
	ld hl, wMap1Object
	ld bc, MAPOBJECT_LENGTH
	rst AddNTimes
	inc hl ; sprite/Y/X are the first three public object-header fields
	ld de, AcceptanceControlPeers
	ld b, 3
.loop
	push bc
	push hl
	ld c, 3
.fields
	ld a, [de]
	inc de
	ld [hli], a
	dec c
	jr nz, .fields
	pop hl
	ld bc, MAPOBJECT_LENGTH
	add hl, bc
	pop bc
	dec b
	jr nz, .loop
endc
	ret
