; Multiplayer system RAM variables

SECTION "Multiplayer RAM", WRAM0

; Control variables
wMultiplayerIsMaster:: db          ; Flag: 1 if this cartridge is the master, 0 if slave
