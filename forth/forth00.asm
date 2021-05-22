; FORTH 
; Alex Dumont

; Help & Reference:
; [Bitwise, Day 35: Implementing Forth](https://www.youtube.com/watch?v=rlayTh3sjiw)
; [Moving Forth: Part 1](https://www.bradrodriguez.com/papers/moving1.htm)


; IP: Next Instruction Pointer (IP)-->W
; W : Address of the code to run
W	.= $FE		; 2 bytes, an address
IP	.= W -2
DTOP	.= IP-2

	*= $8000

RES_vec
   	CLD             ; clear decimal mode
    	LDX #$FF
    	TXS             ; set the stack pointer
    	LDX #DTOP
    	CLI

; This is a Direct Threaded Code based FORTH
NEXT: 	.MACRO
; (IP) --> W
	LDY #0
	LDA (IP),y
	STA W
	INY
	LDA (IP),y
	STA W+1
; IP+2 --> IP
	CLC
	LDA IP
	ADC #2		; A<-A+2
	STA IP
	BCC .skip
	INC IP+1
.skip:
; JMP (W)
	JMP (W)
	.ENDM

; Load the entry point of our main FORTH
; program and start execution (with NEXT)
	; Place forth_prog ADDR into IP register
	LDA #<forth_prog
	STA IP
	LDA #>forth_prog
	STA IP+1
	; run NEXT
	NEXT
	

; Implemented words:
; LIT, SWAP, DUP, TO_R (>R), FROM_R (<R)
; PUTC , GETC, FETCH (@), STORE (!)
; JUMP

; For now, this is the Entry POint of our
; FORTH program.
	
	*= $4000
forth_prog:	
;	.DW word1

	.DW do_LIT
	.DW $AABB

	.DW do_LIT
	.DW $2FF4

	.DW do_STORE

	.DW do_PUSH1
	.DW do_GETC
	.DW do_PLUS
	.DW do_PUTC
	; repeat
	.DW do_JUMP
	.DW forth_prog ; arg for JUMP
	
; Example of some COLON definitions:
; add to the program using:
;	.DW word1

; A word COLON definition
; MUST start with a LABEL followed
; by JMP do_COLON:
;
; do_LABEL:
;	JMP do_COLON
; and MUST end with:
;	.DW do_SEMI

word1:
	JMP do_COLON
	.DW do_PUSH1
	.DW do_DUP
	.DW do_DROP
	.DW word2
	.DW do_DROP
	.DW do_SEMI

word2:
	JMP do_COLON
	.DW do_LIT
	.DW $0002
	.DW do_LIT
	.DW $0103
	.DW do_PLUS
	.DW do_SWAP
	.DW do_SEMI
	

do_COLON: ; COLON aka ENTER
; push IP to Return Stack
	LDA IP+1	; HI
	PHA
	LDA IP		; LO
	PHA

; W+3 --> IP 
; (Code at W was a JMP, so 3 bytes)
	LDA W+1
	STA IP+1
	LDA W
	ADC #3
	STA IP
	BCC skip
	INC IP+1
skip:
	NEXT	


; SEMICOLON aka EXIT
do_SEMI:
; POP IP from Return Stack
	PLA
	STA IP
	PLA
	STA IP+1
; NEXT
	NEXT

do_SWAP:
	LDA 2,X
	LDY 4,X
	STY 2,X
	STA 4,X
	LDA 3,X
	LDY 5,X
	STY 3,X
	STA 5,X
	NEXT

do_DROP:
	INX
	INX
	NEXT

; FORTH Primitive words

do_PUSH0:
	STZ 0,x
	STZ 1,x
	DEX
	DEX
	NEXT

do_PUSH1:
	LDA #1
	STA 0,x
	STZ 1,x
	DEX
	DEX
	NEXT

; Push a literal word (2 bytes)
do_LIT:
; (IP) points to literal
; instead of next instruction ;)
	LDY #0
	LDA (IP),y
	STA 0,X
	INY
	LDA (IP),y
	STA 1,X
	DEX
	DEX
; Now advance IP
; IP+2 --> IP
	LDA IP
	ADC #2
	STA IP
	BCC skip1
	INC IP+1
skip1:
	NEXT

do_DUP:
	LDA 2,X
	STA 0,X
	LDA 3,X
	STA 1,X
	DEX
	DEX
	NEXT	

do_PLUS:
	CLC
	INX
	INX
	LDA 0,X
	ADC 2,X
	STA 2,X
	LDA 1,X
	ADC 3,X
	STA 3,X
	NEXT

do_PUTC: ; "c," emit a single char
	; char is on stack
	LDA 2,X
	INX
	INX
	JSR putc
	NEXT

do_GETC:
; get a single char from IO, leave on stack
	JSR getc ; leaves the char in A
	STA 0,X
	STZ 1,X
	DEX
	DEX
	NEXT

do_TO_R:
; >R: pop a cell (possibly an ADDR) from
; the stack and pushes it to the Return Stack
	INX
	INX
	LDA 1,X
	PHA
	LDA 0,X
	PHA
	NEXT

do_FROM_R:
; <R: pop a cell from the Return Stack
; and pushes it to the Stack
	PLA
	STA 0,X
	PLA
	STA 1,X
	DEX
	DEX
	NEXT

do_AT_R:
; @R : copy the cell from the Return Stack
; to the Stack
	PHX 	;\
	TSX	; \ 
	TXA	;  | put SP into Y
	TAY	; / (a bit involved...)
	PLX	;/ we mess A,Y, but don't care...
	LDA $0102,Y
	STA 0,X
	LDA $0103,Y
	STA 1,X
	DEX
	DEX
	NEXT

do_JUMP:
; (IP) points to literal address to jump to
; instead of next instruction ;)
	; we push the addr to the Return Stack
	LDY #1
	LDA (IP),y
	PHA
	DEY
	LDA (IP),y
	PHA
	; and jump to do_SEMI to handle the rest ;)
	JMP do_SEMI

	
do_JUMP_OLD: ; alternative way of jumping, no Return Stack
; (IP) points to literal address to jump to
; instead of next instruction ;)
	LDY #0
	LDA (IP),y
	PHA
	INY
	LDA (IP),y
	STA IP+1
	PLA
	STA IP
	NEXT

do_FETCH:
; @ ( ADDR -- value ) 
; We read the data at the address on the 
; stack and put the value on the stack
	; copy address from stack to W
	LDA 2,X	; LO
	STA W
	LDA 3,X	; HI
	STA W+1
	; Read data at (W) and save
	; in the TOS
	LDY #0
	LDA (W),y
	STA 2,X
	INY
	LDA (W),y
	STA 3,X
	NEXT

do_STORE:
; ! ( value ADDR -- )
	; copy the address to W
	LDA 2,X	; LO
	STA W
	LDA 3,X	; HI
	STA W+1
	; save the value to (W)
	; LO
	LDY #0
	LDA 4,X
	STA (W),y
	; HI
	INY
	LDA 5,X
	STA (W),y
	INX
	INX
	INX
	INX
	NEXT

; Kowalkski I/O routines
; to change for SBC

getc:
  LDA IO_AREA+4
  BEQ getc
  RTS

putc:
  STA IO_AREA+1
  RTS
	
msg	.BYTE "Monitor v0", 0

IRQ_vec
NMI_vec
	RTI


	*= $0200

BYTE	.DS 1
LEN 	.DS 1	; Length of CMD
CMD	.DS 16	; CMD string

; system vectors

    *=  $FFFA

    .word   NMI_vec     ; NMI vector
    .word   RES_vec     ; RESET vector
    .WORD   IRQ_vec     ; IRQ vector