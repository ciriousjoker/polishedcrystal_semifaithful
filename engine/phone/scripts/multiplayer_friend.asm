INCLUDE "data/phone/text/multiplayer_friend.asm"

MultiplayerFriendScript:
	callasm MultiplayerStartPhoneCall
	farwritetext MultiplayerFriendPhoneOutgoing
	end

MultiplayerFriendCallScript:
	farwritetext MultiplayerFriendPhoneIncoming
	specialphonecall SPECIALCALL_NONE
	end

MultiplayerStartPhoneCall:
	ld hl, wMultiplayerTempPackage
	ld bc, MULTIPLAYER_MAX_PACKAGE_SIZE
	xor a
	rst ByteFill

	ld a, MULTIPLAYER_PKG_PHONECALL
	ld [wMultiplayerTempPackage], a

	ld hl, wMultiplayerTempPackage
	farcall MultiplayerQueuePackage
	ret
