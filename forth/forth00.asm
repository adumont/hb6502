; FORTH 
; Alex Dumont

; Help & Reference:
; [Bitwise, Day 35: Implementing Forth](https://www.youtube.com/watch?v=rlayTh3sjiw)
; [Moving Forth: Part 1](https://www.bradrodriguez.com/papers/moving1.htm)


; IP: Next Instruction Pointer (IP)-->W
; W : Address of the code to run
W	.= $FE		; 2 bytes, an address
IP	.= W -2
G1	.= IP-2		; general purpose register
G2	.= G1-2		; general purpose register
DTOP	.= G2-2

; Offset of the WORD name in the label
; 2 bytes after the Header's addr
HDR_OFFSET_STR .= 2	


	*= $8000

RES_vec
   	CLD             ; clear decimal mode
    	LDX #$FF
    	TXS             ; set the stack pointer
    	LDX #DTOP
    	CLI

; store the ADDR of the latest word to
; LATEST variable:

	LDA #<h_LATEST
	STA LATEST
	LDA #>h_LATEST
	STA LATEST+1

; This is a Direct Threaded Code based FORTH
	
; Load the entry point of our main FORTH
; program and start execution (with JMP NEXT)
	; Place forth_prog ADDR into IP register
	LDA #<forth_prog
	STA IP
	LDA #>forth_prog
	STA IP+1
	; run NEXT

NEXT:
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
	JMP (W)

	

; Implemented words:
; LIT, SWAP, DUP, TO_R (>R), FROM_R (<R)
; PUTC , GETC, FETCH (@), STORE (!)
; JUMP

; For now, this is the Entry POint of our
; FORTH program.
	
	;*= $4000
forth_prog:	
;	.DW word1

	.DW do_LIT
	.DW TEST_STR

	.DW do_FIND
	
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

h_COLON:
	.DW $0000
	.STR ":"
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
	JMP NEXT	


; SEMICOLON aka EXIT
h_SEMI:
	.DW h_COLON
	.STR ";"
do_SEMI:
; POP IP from Return Stack
	PLA
	STA IP
	PLA
	STA IP+1
; JMP NEXT
	JMP NEXT

h_SWAP:
	.DW h_SEMI
	.STR "SWAP"
do_SWAP:
	LDA 2,X
	LDY 4,X
	STY 2,X
	STA 4,X
	LDA 3,X
	LDY 5,X
	STY 3,X
	STA 5,X
	JMP NEXT

h_DROP:
	.DW h_SWAP
	.STR "DROP"
do_DROP:
	INX
	INX
	JMP NEXT

; FORTH Primitive words

h_PUSH0:
	.DW h_DROP
	.STR "PUSH0"
do_PUSH0:
	STZ 0,x
	STZ 1,x
	DEX
	DEX
	JMP NEXT

h_PUSH1:
	.DW h_PUSH0
	.STR "PUSH1"
do_PUSH1:
	LDA #1
	STA 0,x
	STZ 1,x
	DEX
	DEX
	JMP NEXT

; Push a literal word (2 bytes)
h_LIT:
	.DW h_PUSH1
	.STR "LIT"
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
	JMP NEXT

h_DUP:
	.DW h_LIT
	.STR "DUP"
do_DUP:
	LDA 2,X
	STA 0,X
	LDA 3,X
	STA 1,X
	DEX
	DEX
	JMP NEXT	

h_PLUS:
	.DW h_DUP
	.STR "+"
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
	JMP NEXT

h_PUTC:
	.DW h_PLUS
	.STR "PUTC"
do_PUTC: ; "c," emit a single char
	; char is on stack
	LDA 2,X
	INX
	INX
	JSR putc
	JMP NEXT

h_GETC:
	.DW h_PUTC
	.STR "GETC"
do_GETC:
; get a single char from IO, leave on stack
	JSR getc ; leaves the char in A
	STA 0,X
	STZ 1,X
	DEX
	DEX
	JMP NEXT

h_TO_R:
	.DW h_GETC
	.STR ">R"
do_TO_R:
; >R: pop a cell (possibly an ADDR) from
; the stack and pushes it to the Return Stack
	INX
	INX
	LDA 1,X
	PHA
	LDA 0,X
	PHA
	JMP NEXT

h_FROM_R:
	.DW h_TO_R
	.STR "<R"
do_FROM_R:
; <R: pop a cell from the Return Stack
; and pushes it to the Stack
	PLA
	STA 0,X
	PLA
	STA 1,X
	DEX
	DEX
	JMP NEXT

h_AT_R:
	.DW h_FROM_R
	.STR "@R"
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
	JMP NEXT

h_JUMP:
	.DW h_AT_R
	.STR "JUMP"
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
	JMP NEXT


h_FETCH:
	.DW h_JUMP
	.STR "@"
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
	JMP NEXT

h_STORE:
	.DW h_FETCH
	.STR "!"
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
	JMP NEXT

h_FIND:
	.DW h_STORE
	.STR "FIND"
do_FIND:
; ( ADDRi -- ADDRo )
; ADDRi: Address of a string
; ADDRo: Address of the header if Found
; or 0000 if not found

; Store the addr on the STACK in G2
; Addr to the counted string we look in
; the dictionary
	LDA 2,X	; LO
	STA G2
	LDA 3,X	; HI
	STA G2+1
; store LATEST in W
	CLC
	LDA LATEST
	STA W
	LDA LATEST+1
	STA W+1
nxt_word: ; previous word in the dictionary
; store W+2 in G1
	CLC
	LDA W
	ADC #HDR_OFFSET_STR
	STA G1
	LDA W+1
	ADC #0
	STA G1+1

	JSR STRCMP
	BEQ found
; not found: look for next word in
; dictionnary

	; W points to the previous entry
	; (W) -> W
	LDY #0
	LDA (W),Y
	STA 0,X ; we store it there temporarily
	INY
	LDA (W),Y
	STA W+1
	LDA 0,X
	STA W
	BNE nxt_word
	LDA W+1
	BNE nxt_word
	; here: not found :(
	INX
	INX
	JMP do_PUSH0
	
found:	; ADDR is W -> TOS
	SEC
	LDA W
	STA 2,X
	LDA W+1
	STA 3,X

	JMP NEXT

STRCMP:
; Clobbers: A, Y
; Input:
; - expects the addr of 2 counted STR
;   in registers G1 and G2
; Output:
; - Z flag set if both str equals
; - Z flag cleared if not equals
	; compare the length (1rst byte)
	LDY #0
	LDA (G1),Y	
	CMP (G2),Y
	;BNE not_same
	BNE strcmp_exit

	; here we know both str have
	; same length (in A)
	; now, we put Len in Y
	TAY	; Y=length
next_char:
	LDA (G1),Y
	CMP (G2),Y
	;BNE not_same
	BNE strcmp_exit
	DEY
	; BEQ same
	BNE next_char
strcmp_exit:
	RTS

not_same:
	CLC
	RTS
same:
	SEC
	RTS

; ALWAYS update the latest word's 
; header address
h_LATEST .= h_FIND


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


TEST_STR: .STR "JUMP"

	*= $0200

LATEST	.DS 2	; Store the latest ADDR of the Dictionary
CMD	.DS 16	; CMD string

; system vectors

    *=  $FFFA

    .word   NMI_vec     ; NMI vector
    .word   RES_vec     ; RESET vector
    .WORD   IRQ_vec     ; IRQ vector
