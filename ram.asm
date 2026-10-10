INCLUDE "macros/ram.asm"

INCLUDE "ram/vram.asm"
INCLUDE "ram/wram0.asm"
INCLUDE "ram/wramx.asm"
INCLUDE "ram/sram.asm"
INCLUDE "ram/oam.asm"
INCLUDE "ram/hram.asm"

if DEF(MOBILE_ADAPTER)
INCLUDE "mobile_adapter/rom/ram.asm"
endc
