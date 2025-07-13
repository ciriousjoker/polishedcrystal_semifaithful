; Become master if not already master or a forced slave.
MultiplayerInitializeConnection::
	; If we are a forced slave, we cannot become master.
	ld a, [wMultiplayerIsForcedSlave]
	and a
	ret nz

	; If we are already master, do nothing.
	ld a, [wMultiplayerIsMaster]
	and a
	ret nz

	; Initialize multiplayer system
	farcall MultiplayerInitialize
	
	; We are becoming master.
	ld a, 1
	ld [wMultiplayerIsMaster], a

	; Send a package to the other player to inform them.
	; Prepare package
	ld hl, wMultiplayerTempPackage
	ld bc, MULTIPLAYER_MAX_PACKAGE_SIZE
	xor a
	rst ByteFill

	ld a, MULTIPLAYER_PKG_INIT
	ld [wMultiplayerTempPackage], a

	; Queue package
	ld hl, wMultiplayerTempPackage
	jp MultiplayerQueuePackage
