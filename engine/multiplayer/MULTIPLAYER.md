# Multiplayer communication

This is used in the overworld and is completely separate from the official system used in the Pokemon Centers.

## Overview

Arbitrary packages of varying length can be sent using the stuff defined in `frame_multiplayer.asm`.
Just run `MultiplayerQueuePackage` to asynchronously queue a package to be sent.
Running `MultiplayerQueuePackage` again will overwrite the previously queued package,
unless it's already in the process of being delivered.

Incoming packages are processed in `frame_multiplayer.asm` and should be handled in `MultiplayerOnPackageReceived` if possible.

### Packages

#### Get player name

| Request | Response                                          |
| ------- | ------------------------------------------------- |
| `0x00`  | [`0x01`, `wPlayerName[0]`, ..., `wPlayerName[7]`] |
