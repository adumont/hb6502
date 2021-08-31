; ACIA subroutines
; (C) Alex Dumont, 2021

; ACIA $4200
.define   ACIA_BASE         $4200 ;
.define   ACIA_DATA  ACIA_BASE+$0 ;
.define   ACIA_STAT  ACIA_BASE+$1 ;
.define   ACIA_CMD   ACIA_BASE+$2 ;
.define   ACIA_CTRL  ACIA_BASE+$3 ;

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

acia_init:
	; ACIA setup

    stz ACIA_STAT       ; Programmed reset ACIA (data in A is "don't care")

    ; stz ACIA_CTRL       ; 16x EXTERNAL CLOCK, External Receiver Clock, 8 data word length, 1 stop bit

    lda #%00011110      ; SBR: 9600, RCS: baud rate
    sta ACIA_CTRL

    lda #%00001011      ; set specific modes and functions:
                        ;   no parity, no echo, no Tx interrupt
                        ;   no Rx interrupt, enable Tx/Rx
    sta ACIA_CMD

    rts