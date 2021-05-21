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

	LDA #<lbl_test1
	STA IP
	LDA #>lbl_test1
	STA IP+1


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
	
	NEXT
	
	
	
	*= $4000
lbl_test1:	
	.DW forth_prog
	
	*= $4100
forth_prog:
	JMP do_COLON
	.DW do_PUSH1
	.DW do_DUP
	.DW do_DROP
	.DW other_word
	.DW do_DROP
	.DW do_SEMI

other_word:
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