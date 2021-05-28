; FORTH 
; Alex Dumont

; Help & Reference:
; [Bitwise, Day 35: Implementing Forth](https://www.youtube.com/watch?v=rlayTh3sjiw)
; [Moving Forth: Part 1](https://www.bradrodriguez.com/papers/moving1.htm)


; IP: Next Instruction Pointer (IP)-->W
; W : Address of the code to run
W	.= $FE		; 2 bytes, an address
IP	.= W -2
G2	.= IP-2		; general purpose register
G1	.= G2-2		; general purpose register
DTOP	.= G1-2		; Stack TOP

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

	LDA #<USER_BASE
	STA DP
	LDA #>USER_BASE
	STA DP+1

; This is a Direct Threaded Code based FORTH
	
; Load the entry point of our main FORTH
; program and start execution (with JMP NEXT)
	; Place forth_prog ADDR into IP register
	LDA #<forth_prog
	STA IP
	LDA #>forth_prog
	STA IP+1
	BRA NEXT

; Lots of primitive words were ending with:
; 	DEX
;	DEX
;	JMP NEXT
; I have put this right here before NEXT, so now in the
; primitive word I jump to DEX2_NEXT, and DEX2_NEXT will
; fall through to NEXT. We save on ROM memory

DEX2_NEXT:
	DEX
	DEX
	; fall through NEXT

NEXT:
; (IP) --> W
	LDA (IP)
	STA W
	LDY #1
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

;	.DW do_DUP, do_PRINT, do_CRLF
	
	; Put Addr of a string
	.DW do_LIT
	.DW h_PRINT+3
		
	; Put LENGTH of the string
	.DW do_LIT, $0005
	
	.DW do_FIND
	

; do_CFA
	.DW do_1PLUS, do_1PLUS     ; 1+ 1+	; skip prev. word link
	.DW do_DUP, do_CFETCH, do_1PLUS, do_PLUS ; DUP c@ 1+ +	; add length
;	
	.DW do_DUP,do_PRINT, do_CRLF	; print
	
	.DW do_DUP

	.DW do_EXEC

	.DW do_DP
	.DW do_1PLUS, do_1PLUS     ; 1+ 1+
	.DW do_DP, do_STORE 	   ; DP !

	; Print 1234 5678 on output
	.DW do_LIT, $1234, do_PRINT   ; $1234
	.DW do_SPACE
	.DW do_LIT, $5678, do_PRINT
	.DW do_CRLF
	.DW do_LIT, $ABCD, do_PRINT

	.DW do_LIT, $1234	   ; $1234
	.DW do_DP, do_FETCH 	   ; DP @ (HERE)
	.DW do_STORE               ; !

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
	JMP DEX2_NEXT

h_PUSH1:
	.DW h_PUSH0
	.STR "PUSH1"
do_PUSH1:
	LDA #1
	STA 0,x
	STZ 1,x
	JMP DEX2_NEXT


; Push a literal word (2 bytes)
h_LIT:
	.DW h_PUSH1
	.STR "LIT"
do_LIT:
; (IP) points to literal
; instead of next instruction ;)
	LDA (IP)
	STA 0,X
	LDY #1
	LDA (IP),y
	STA 1,X
;	DEX
;	DEX
; Now advance IP
; IP+2 --> IP
	LDA IP
	ADC #2
	STA IP
	BCC .skip
	INC IP+1
.skip:
	JMP DEX2_NEXT

h_DUP:
	.DW h_LIT
	.STR "DUP"
do_DUP:
	LDA 2,X
	STA 0,X
	LDA 3,X
	STA 1,X
	JMP DEX2_NEXT
	

h_PLUS:
	.DW h_DUP
	.STR "+"
do_PLUS:
	CLC
	LDA 2,X
	ADC 4,X
	STA 4,X
	LDA 3,X
	ADC 5,X
	STA 5,X
	JMP do_DROP

h_1PLUS:
	.DW h_PLUS
	.STR "1+"
do_1PLUS:
	CLC
	INC 2,X
	BNE .skip
	INC 3,X
.skip:	JMP NEXT

h_PUTC:
	.DW h_1PLUS
	.STR "PUTC"
do_PUTC: ; "c," emit a single char
	; char is on stack
	LDA 2,X
	JSR putc
	JMP do_DROP

h_GETC:
	.DW h_PUTC
	.STR "GETC"
do_GETC:
; get a single char from IO, leave on stack
	JSR getc ; leaves the char in A
	STA 0,X
	STZ 1,X
	JMP DEX2_NEXT


h_TO_R:
	.DW h_GETC
	.STR ">R"
do_TO_R:
; >R: pop a cell (possibly an ADDR) from
; the stack and pushes it to the Return Stack
	LDA 3,X
	PHA
	LDA 2,X
	PHA
	JMP do_DROP

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
	JMP DEX2_NEXT


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
	JMP DEX2_NEXT


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
	LDA (IP)
	PHA
	; and jump to do_SEMI to handle the rest ;)
	JMP do_SEMI

	
do_JUMP_OLD: ; alternative way of jumping, no Return Stack
; (IP) points to literal address to jump to
; instead of next instruction ;)
	LDA (IP)
	PHA
	LDY #1
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
	LDA (W)
	STA 2,X
	LDY #1
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
	LDA 4,X
	STA (W)
	; HI
	LDY #1
	LDA 5,X
	STA (W),y
end_do_STORE:	
	INX
	INX
	;INX       ; INX INX NEXT is do_DROP
	;INX
	;JMP NEXT 	
	JMP do_DROP

h_CFETCH:
	.DW h_STORE
	.STR "c@"
do_CFETCH:
; c@ ( ADDR -- byte ) 
; We read the data at the address on the 
; stack and put the value on the stack
	; copy address from stack to W
	LDA 2,X	; LO
	STA W
	LDA 3,X	; HI
	STA W+1
	; Read data at (W) and save
	; in the TOS
	LDA (W)
	STA 2,X
	STz 3,X
	JMP NEXT

h_CSTORE:
	.DW h_CFETCH
	.STR "C!"
do_CSTORE:
; C! ( value ADDR -- )
	; copy the address to W
	LDA 2,X	; LO
	STA W
	LDA 3,X	; HI
	STA W+1
	; save the value to (W)
	; LO
	LDA 4,X
	STA (W)
	BRA end_do_STORE

h_FIND:
	.DW h_CSTORE
	.STR "FIND"
do_FIND:
; ( ADDRi LEN -- ADDRo )
; ADDRi: Address of a string
; LEN: Length of the string (LO byte only)
; ADDRo: Address of the header if Found
; or 0000 if not found

; Store the addr on the STACK in G2
	LDA 4,X	; LO
	STA G2
	LDA 5,X	; HI
	STA G2+1
; store LATEST in W
	LDA LATEST
	STA W
	LDA LATEST+1
	STA W+1
nxt_word:
; store W+2 in G1 (G1 points to the counted str)
	CLC
	LDA W
	ADC #HDR_OFFSET_STR
	STA G1
	LDA W+1
	ADC #0
	STA G1+1

; compare length
	LDA 2,X		; len on stack (1byte)
	CMP (G1)	; compare to 
	BNE advance_w	; not same length, advance to next word
; same length: compare str
	; G1+1 --> G1 (now points to STR, not length)
	CLC
	INC G1
	BNE .skip
	INC G1+1
.skip:	
	TAY		; we previously loaded LEN in A --> Y
	JSR STRCMP
	BEQ found

; not found: look for next word in
; dictionnary

advance_w:
	; W points to the previous entry
	; (W) -> W
	LDA (W)
	STA 0,X ; we store it there temporarily
	LDY #1
	LDA (W),Y
	STA W+1
	LDA 0,X
	STA W
	BNE nxt_word
	LDA W+1
	BNE nxt_word
	; here: not found :(, we put 00 on stack and exit
	INX
	INX
	JMP do_PUSH0 ; this will also exit (NEXT)
	
found:	; ADDR is W -> TOS
	LDA W
	STA 4,X
	LDA W+1
	STA 5,X
	JMP do_DROP

STRCMP:
; Clobbers: A, Y
; Input:
; - expects the addr of 2 counted STR
;   in registers G1 and G2
; Output:
; - Z flag set if both str equals
; - Z flag cleared if not equals
.next_char:
	DEY
	LDA (G1),Y
	CMP (G2),Y
	BNE .strcmp_exit
	CPY #0
	BNE .next_char
.strcmp_exit:
	RTS


h_DP:
	.DW h_FIND
	.STR "DP"
do_DP:
; DP ( -- addr )

; Alternative, as word definition:
;	JMP do_COLON
;	.DW do_LIT, CP, do_SEMI
	LDA #<DP
	STA 0,X
	LDA #>DP
	STA 1,X
	JMP DEX2_NEXT


; Print a WORD
h_PRINT:
	.DW h_DP
	.STR "PRINT"
do_PRINT:
	LDA 3,X
	JSR print_byte
	LDA 2,X
	JSR print_byte
	JMP do_DROP

h_SPACE:
	.DW h_PRINT
	.STR "SPACE"
do_SPACE:
	LDA #' '
	JSR putc
	JMP NEXT

h_CRLF:
	.DW h_SPACE
	.STR "CRLF"
do_CRLF:
	LDA #$0a ; CR
	JSR putc
	LDA #$0d ; LF
	JSR putc
	JMP NEXT

h_HERE:
	.DW h_CRLF
	.STR "HERE"
do_HERE:
; : HERE	DP @ ;
	JMP do_COLON
	.DW do_DP, do_FETCH, do_SEMI

h_ALLOT:
	.DW h_HERE
	.STR "ALLOT"
do_ALLOT:
; : ALLOT	HERE + DP ! ;
	JMP do_COLON
	.DW do_HERE, do_PLUS, do_DP, do_STORE
	.DW do_SEMI

h_CFA:
	.DW h_ALLOT
	.STR ">CFA"
do_CFA:
	JMP do_COLON
	; ( ADDR -- ADDR )
	; takes the dictionary pointer to a word
	; returns the codeword pointer
	.DW do_1PLUS, do_1PLUS  ; 1+ 1+	; skip prev. word link
	.DW do_DUP, do_CFETCH, do_1PLUS, do_PLUS ; DUP c@ 1+ +	; add length
	.DW do_SEMI

; Put Data Stack Pointer on the stack
h_DSP:
	.DW h_CFA
	.STR "DSP"
do_DSP:
	TXA
	STA 0,X
	STZ 1,X
	JMP DEX2_NEXT


; Put Data Stack Pointer on the stack
h_EXEC:
	.DW h_DSP
	.STR "EXEC"
do_EXEC:
	LDA 2,X
	STA W
	LDA 3,X
	STA W+1
	INX
	INX
	JMP (W)


;-----------------------------------------------------------------
; ALWAYS update the latest word's 
; header address h_*
h_LATEST .= h_EXEC



;-----------------------------------------------------------------
; I/O routines for Kowalkski simulator 
; Change for SBC

getc:
  LDA IO_AREA+4
  BEQ getc
  RTS

putc:
  STA IO_AREA+1
  RTS
	
msg	.BYTE "Monitor v0", 0


; Print routines

print_byte:
	PHA	; save A for 2nd nibble
	LSR	; here we shift right
	LSR	; to get HI nibble
	LSR
	LSR
	JSR nibble_value_to_asc
	JSR putc

	PLA
	AND #$0F ; LO nibble
	JSR nibble_value_to_asc
	JSR putc
	RTS

nibble_value_to_asc:
	CMP #$0A
	BCC skip3
	ADC #$66
skip3:
	EOR #$30
	RTS


; Interrupts routines

IRQ_vec
NMI_vec
	RTI
	
	
	
	


TEST_STR: .STR "HERE"

	*= $0200

LATEST	.DS 2	; Store the latest ADDR of the Dictionary
CMD	.DS 80	; CMD string
DP	.DS 2	; Data Pointer: Store the latest ADDR of next free space in RAM (HERE)

; Base of user memory area.
USER_BASE:

; system vectors

    *=  $FFFA

    .word   NMI_vec     ; NMI vector
    .word   RES_vec     ; RESET vector
    .WORD   IRQ_vec     ; IRQ vector