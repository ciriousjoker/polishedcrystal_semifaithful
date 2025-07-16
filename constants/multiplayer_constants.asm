; Multiplayer protocol constants

DEF MULTIPLAYER_MAX_PACKAGE_SIZE EQU 9
DEF MULTIPLAYER_IDLE_FRAMES EQU 1 ; 5 * 60fps -> ~5s

const_def
  ; Critical packages. Cannot be removed from the queue.
	const MULTIPLAYER_PKG_INIT          ; 00
	const MULTIPLAYER_PKG_NAME          ; 01
	const MULTIPLAYER_PKG_GIFT_ITEM     ; 02
  ; Anything below this line isn't critical and might be replaced by a newer package
  ; if the queue is full.
	const MULTIPLAYER_PKG_NOOP          ; 03
	const MULTIPLAYER_PKG_SEND_POSITION ; 04
	const MULTIPLAYER_PKG_PHONECALL     ; 05
