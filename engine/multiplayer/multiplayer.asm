; Toggle master/slave status when start menu is opened
BecomeMultiplayerMaster::
	ld a, [wMultiplayerIsMaster]
	xor 1
	ld [wMultiplayerIsMaster], a
	
	; Initialize multiplayer system
	farcall MultiplayerInitialize
	
	; Show debug info about role change
	call OpenText
	ld a, [wMultiplayerIsMaster]
	and a
	ld hl, .BecameMasterText
	jr nz, .show_role
	ld hl, .BecameSlaveText
.show_role:
	call PrintText
	call CloseText
	ret

.BecameMasterText:
	text "DEBUG: Now MASTER"
	done

.BecameSlaveText:
	text "DEBUG: Now SLAVE"
	done

MultiplayerSendPlayerMovement::
	push hl
	push bc
	push de

	call SendPlayerMovementData

	call OpenText
	ld hl, .StepText
	call PrintText
	call CloseText

	pop de
	pop bc
	pop hl
	ret

.StepText:
	text "Step taken!"
	done