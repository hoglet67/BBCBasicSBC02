org $c000

include_LANG_HEADER=FALSE        \ 35 bytes
include_AUTO=TRUE                \ 54 bytes
include_LOADSAVECHAIN=TRUE       \ 103 bytes
include_OSCLI=TRUE               \ 34 bytes
                                 \ 14 byte shared
include_RANDOMACCESS=FALSE       \ 443 bytes

.rom_start

.ENTER_BASIC

WRCHV=0                          \ Use JMP $FFEE directly

include "Basic2.asm"
include "SBC_MOS.asm"

.rom_end

SAVE rom_start, rom_end
