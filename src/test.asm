org $8000

\MOS Entry Points:
OSCLI=&FFF7 :OSBYTE=&FFF4:OSWORD=&FFF1:OSWRCH=&FFEE
OSWRCR=&FFEC:OSNEWL=&FFE7:OSASCI=&FFE3:OSRDCH=&FFE0
OSFILE=&FFDD:OSARGS=&FFDA:OSBGET=&FFD7:OSBPUT=&FFD4
OSGBPB=&FFD1:OSFIND=&FFCE:BRKV=&202:WRCHV=&20E

incBeebLangHeader=1

.rom_start
include "Basic2.asm"
.rom_end

SAVE rom_start, rom_end