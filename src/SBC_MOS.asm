;; *************************************************************
;; Configuration
;; *************************************************************

USE_XON_XOFF = 1

;; *************************************************************
;; Memory
;; *************************************************************

ZP_START    = $00e0

pblock      = $00e0
pcopy       = $00e2
bufptr      = $00e2

ZP_TIME     = $00f0
ZP_RX_HEAD  = $00f4
ZP_RX_TAIL  = $00f5
ZP_TX_HEAD  = $00f6
ZP_TX_TAIL  = $00f7
ZP_XOFF     = $00f8
ZP_X        = $00fa
ZP_Y        = $00fb
ZP_ACC      = $00fc
ZP_ERRPTR   = $00fd
ZP_ESCFLAG  = $00ff

ZP_END      = $00ff


BRKV        = $0202

;; Rx Buffer is $7e00-$7eff
RX_BUFFER   = $7E00

;; Tx Buffer is $7f00-$7fff
TX_BUFFER   = $7F00

;; 6850 UART is at $a000
UART        = $A000

;; *************************************************************
;; OS API Implementation
;; *************************************************************

.nvosrdch
{
   STX ZP_X             ; Save X
.loop_empty
   LDX ZP_RX_HEAD       ; Is the Rx buffer empty
   CPX ZP_RX_TAIL
   BEQ loop_empty       ; Yes, loop back until a character arrives
   LDA ZP_ESCFLAG
   ROL A                ; C = bit7 of the escape flag
   LDA RX_BUFFER, X     ; Read the character from the Rx buffer
   INC ZP_RX_HEAD       ; Increment the Rx buffer head pointer
   LDX ZP_X             ; Restore X
   RTS
}


.nvoswrch
{
   STX   ZP_X           ; Save X
.loop_full
   LDX   ZP_TX_TAIL     ; Is there space in the Tx buffer for one more character?
   INX
   CPX   ZP_TX_HEAD
   BEQ   loop_full      ; No, then loop back and wait for characters to drain
   STA   TX_BUFFER,X
   STX   ZP_TX_TAIL     ; Save the updated tail pointer
   LDX   #$B5           ; Enable Tx interrupts to make sure buffer is serviced
   STX   UART
   LDX   ZP_X
}

.nvosfind
.nvosgbpb
.nvosbput
.nvosbget
.nvosargs
.nvosfile
.nvoscli
{
   RTS
}

;; *************************************************************
;; OSWORD
;; *************************************************************

.nvosword
{
   STX pblock
   STY pblock+1
   CMP #$01
   BCC osword_readline
   BEQ osword_readsysclk
   CMP #$02
   BEQ osword_writesysclk
   RTS
}

.osword_readsysclk
{
   LDY  #$03
.loop
   LDA ZP_TIME,Y
   STA (pblock), Y
   DEY
   BPL loop
   ;; Increment the clock each time it's read, as we have no other timer!
   INC ZP_TIME
   BNE done
   INC ZP_TIME+1
   BNE done
   INC ZP_TIME+2
   BNE done
   INC ZP_TIME+3
.done
   LDA #$01             ; preserve A
   LDY pblock+1         ; preserve Y
   RTS
}

.osword_writesysclk
{
   LDY #$03
.loop
   LDA (pblock), Y
   STA ZP_TIME,Y
   DEY
   BPL loop
   LDA #$02           ; preserve A
   LDY pblock+1       ; preserve Y
   RTS
}


;; On Entry: X points to the parameter block
;; 0: Buffer address
;; 2: Max line length
;; 3: Min ascii
;; 4: Max ascii

.osword_readline
{
   LDY #&04             ; Y=4
.ploop
   LDA (pblock),Y       ; copy parameter block to zero page
   STA pcopy,Y
   DEY
   BPL ploop
   BMI cloop2           ; branch always

.bell
   LDA #&07             ; bell character

.y0
   DEY                  ; decrement buffer index

.y1
   INY                  ; increment buffer index

.cloop1
   JSR OSWRCH           ; echo the character

.cloop2
   JSR OSRDCH           ; read character from input stream
   BCS exit_err         ; if carry set then illegal character or other error and exit
   CMP #&08             ; if character is not delete
   BNE not_del          ; process it
   CPY #&00             ; else is Y=0
   BEQ cloop2           ; and goto cloop2
   DEY                  ; decrement Y
   JSR OSWRCH           ; cursor left
   LDA #$20             ; space
   JSR OSWRCH
   LDA #$08             ; cursor left
   BNE cloop1           ; jump always

.not_del
   STA (bufptr),Y       ; store character in designated buffer
   CMP #&0D             ; is it CR?
   BEQ exit_ok          ; if so exit
   CPY pcopy+2          ; else check the line length
   BCS bell             ; if = or greater loop to ring bell
   CMP pcopy+3          ; check minimum character
   BCC y0               ; if less than minimum backspace
   CMP pcopy+4          ; check maximum character
   BEQ y1               ; if equal y1
   BCC y1               ; or less y1
   BCS y0               ; then y0

.exit_ok
   JSR OSNEWL           ; output CR/LF

.exit_err
   LDA ZP_ESCFLAG       ; A=ESCAPE FLAG
   ROL A                ; put bit 7 into carry
   LDA #&00             ; Preserve A
   RTS                  ; and exit routine
}


;; *************************************************************
;; OSBYTE
;; *************************************************************

.nvosbyte
{
   PHA
   CMP #$7C
   BNE not7c
   LDA #$00
   STA ZP_ESCFLAG
   PLA
   RTS
.not7c
   CMP #$7D
   BNE not7d
   LDA #$80
   STA ZP_ESCFLAG
   PLA
   RTS
.not7d
   CMP #$7E
   BNE not7e
   LDX #$00
   STX ZP_ESCFLAG
   STX ZP_RX_HEAD
   STX ZP_RX_TAIL
   DEX
   PLA
   RTS
.not7e
   CMP #$83
   BNE not83
   LDY #$08
   BNE exitx0
.not83
   CMP #$84
   BNE exit
   LDY #$7e
.exitx0
   LDX #$00
.exit
   PLA
   RTS
}

;; *************************************************************
;; IRQ Handler
;; *************************************************************

.irqbrk_handler
{
   STA ZP_ACC
   PLA
   PHA
   AND #$10
   BEQ irq_handler
   TXA                  ; could drop to save a few bytes
   PHA                  ; could drop to save a few bytes
   TSX
   LDA $103, X
   CLD
   SEC
   SBC #$01
   STA ZP_ERRPTR
   LDA $104, X
   SBC #$00
   STA ZP_ERRPTR+1
   PLA                  ; could drop to save a few bytes
   TAX                  ; could drop to save a few bytes
   LDA ZP_ACC
   CLI
   JMP (BRKV)
}

.nmi_handler
{
   RTI
}

.irq_handler
{
   TXA
   PHA
   LDA UART             ; Read UART status register
   AND #$01             ; Test bit 0 (RxFull)

   BEQ irq_tx           ; no, then go on to check for a transmit interrupt
   LDA UART+1           ; Read UART Rx Data (and clear interrupt)
   CMP #$1B             ; Test for escape
   BNE irq_noesc
   LDX #$80             ; Set the escape flag
   STX ZP_ESCFLAG
.irq_noesc
   LDX ZP_RX_TAIL       ; X = keyboard buffer tail index
   STA RX_BUFFER,X      ; store the character in the buffer
   INX                  ; increment the tail pointer
   CPX ZP_RX_HEAD       ; has it hit the head (buffer full?)
   BEQ irq_tx           ; yes, then drop characters
   STX ZP_RX_TAIL       ; no, then save the incremented tail pointer

.irq_tx
   LDA UART             ; Read UART status register
   AND #$02             ; Test bit 0 (TxEmpty)
   BEQ irq_exit         ; Not empty, so exit

;; Simple implementation of XON/XOFF to prevent receive buffer overflow
IF USE_XON_XOFF = 1
   LDA ZP_RX_TAIL       ; Determine if we need to send XON or XOFF
   SEC
   SBC ZP_RX_HEAD       ; Tail - Head gives the receive buffer occupancy
   EOR ZP_XOFF          ; In XOFF state, complement to give some hysterisis
   CMP #$C0             ; C=1 if occupancy >=75% (when in XON) or <25% (when in XOFF)
   BCC irq_tx_char      ; Nothing to do...
   LDA ZP_XOFF          ; toggle the XON/XOFF state
   EOR #$FF
   STA ZP_XOFF
   BNE send_xoff
   LDA #$11             ; 0x11 = XON character
   BNE send_a           ; Send XON
.send_xoff
   LDA #$13             ; 0x13 = XOFF character
   BNE send_a           ; Send XOFF
ENDIF

.irq_tx_char
   LDX ZP_TX_HEAD       ; Is the Tx buffer empty?
   CPX ZP_TX_TAIL
   BEQ irq_tx_done      ; Yes, then disable Tx interrupts and exit
   INX
   STX ZP_TX_HEAD
   LDA TX_BUFFER, X

.send_a
   STA UART+1

.irq_exit
   PLA
   TAX
   LDA ZP_ACC
   RTI

.irq_tx_done
   LDA #$95
   STA UART
   BNE irq_exit
}

.reset_msg
   EQUB $0D
   EQUS "SBC02 "
   EQUS GITVERSION
   EQUB $0A, $0D
IF USE_XON_XOFF = 1
   EQUB $11             ; XON
ENDIF
   EQUB $00

.default_brk_handler
   ;; Default BRK handler is reset code

.reset_handler
{
   ;; Initialize the stack
   CLD
   LDX #$FF
   TXS

   ;; Initialize Zero Page
   LDX #ZP_END - ZP_START + 1
   LDA #$00
.clrloop
   STA ZP_START,X
   DEX
   BPL clrloop

   ;; Initialize the SWI Handler
   LDA #<default_brk_handler
   STA BRKV
   LDA #>default_brk_handler
   STA BRKV+1

   ;; Initialize the UART
   ;; RX INT ENABLED, RTS LOW, TX INT DISABLED, 8N1, CLK/16
   LDA #$95
   STA UART

   ;; Enable interrupts
   CLI

   ;; Print the reset message
   LDX #0
.prloop
   LDA reset_msg, X
   BEQ done
   JSR OSASCI
   INX
.done
   ;; Enter Basic
   JMP ENTER_BASIC
}

;; *************************************************************
;; OS Interface
;; *************************************************************

;; Note, to save space nothing is vectored

   ORG $ffce
.OSFIND
   JMP nvosfind
.OSGBPB
   JMP nvosgbpb
.OSBPUT
   JMP nvosbput
.OSBGET
   JMP nvosbget
.OSARGS
   JMP nvosargs
.OSFILE
   JMP nvosfile
.OSRDCH
   JMP nvosrdch
.OSASCI
   CMP #$0D
   BNE OSWRCH
.OSNEWL
   LDA #$0A
   JSR OSWRCH
   LDA #$0D
.OSWRCH
   JMP nvoswrch
.OSWORD
   JMP nvosword
.OSBYTE
   JMP nvosbyte
.OSCLI
   JMP nvoscli

;; *************************************************************
;; Vectors
;; *************************************************************

   EQUW nmi_handler
   EQUW reset_handler
   EQUW irqbrk_handler
