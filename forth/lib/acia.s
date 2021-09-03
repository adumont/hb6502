; ACIA subroutines
; (C) Alex Dumont, 2021

; __acia_speed = 9600
__acia_speed = 115200

; ACIA $4200
.define   ACIA_BASE         $4200 ;
.define   ACIA_DATA  ACIA_BASE+$0 ;
.define   ACIA_STAT  ACIA_BASE+$1 ;
.define   ACIA_CMD   ACIA_BASE+$2 ;
.define   ACIA_CTRL  ACIA_BASE+$3 ;

acia_init:
	; ACIA setup

    stz ACIA_STAT       ; Programmed reset ACIA (data in A is "don't care")


; ACIA speed config

; 1.8432MHz oscillador needs to be connected to XTLI and RxC pins of the 6551. XTLO floating.
.if __acia_speed = 115200

    .out    "** ACIA configured at 115200 bauds"

    stz ACIA_CTRL       ; 16x EXTERNAL CLOCK, External Receiver Clock, 8 data word length, 1 stop bit

.elseif __acia_speed = 9600
   
    .out    "** ACIA configured at 9600 bauds"

    lda #%00011110      ; SBR: 9600, RCS: baud rate
    sta ACIA_CTRL

.else
    .fatal  "** Must define __acia_speed 115200 or 9600"
.endif

    lda #%00001011      ; set specific modes and functions:
                        ;   no parity, no echo, no Tx interrupt
                        ;   no Rx interrupt, enable Tx/Rx
    sta ACIA_CMD

    rts

putc:
acia_send_char:
    pha
:   lda ACIA_STAT       ; wait for TX empty
    and #%00010000      ; Bit 4: Transmitter Data Register Empty?
    beq :-              ; 0 - Not Empty, repeat
    pla                 ; restore char

    sta ACIA_DATA       ; send char
    rts

getc:
acia_receive_char:
:   lda ACIA_STAT       ; wait for RX full
    and #%00001000      ; Bit 3: Receiver Data Register Full?
    beq :-              ; 0 - Not Full, repeat
    lda ACIA_DATA       ; read char
    rts

