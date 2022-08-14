org $be00

include_AUTO=FALSE

.rom_start

.ENTER_BASIC

incBeebLangHeader=0
WRCHV=0      \ User JMP $FFEE

include "Basic2.asm"
include "SBC_MOS.asm"

.rom_end

SAVE rom_start, rom_end
