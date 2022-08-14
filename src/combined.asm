org $be00

include_AUTO=FALSE
include_LOADSAVECHAIN=FALSE
include_OSCLI=FALSE
include_RANDOMACCESS=FALSE

.rom_start

.ENTER_BASIC

incBeebLangHeader=0
WRCHV=0      \ User JMP $FFEE

include "Basic2.asm"
include "SBC_MOS.asm"

.rom_end

SAVE rom_start, rom_end
