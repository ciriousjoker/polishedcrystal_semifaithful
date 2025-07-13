DoMultiplayerStep::
	ld a, BANK(.Script_MonRecoveredFromPoison)
	ld hl, .Script_MonRecoveredFromPoison
	call CallScript
	scf
	ret

.Script_MonRecoveredFromPoison:
	; opentext
	; callasm .CheckWhitedOut
	; iffalsefwd .whiteout
	; closetext
	end
