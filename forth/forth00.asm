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
BKSPACE .= $08
MAX_LEN .= 80		; Input Buffer MAX length

BOOTP	.= INP_IDX - 2	; 2 last bytes from the input buffer will 
			; be used at boot as pointer into the
			; bootstrap code

; Offset of the WORD name in the label
; 2 bytes after the Header's addr
HDR_OFFSET_STR .= 2	


	*= $8000

RES_vec
   	CLD             ; clear decimal mode
    	LDX #$FF
    	TXS             ; set the stack pointer
	STX MODE	; set MODE to FF
	STX BOOT	; set BOOT to FF
    	LDX #DTOP
    	CLI

; store the ADDR of the latest word to
; LATEST variable:

	LDA #<p_LATEST
	STA LATEST
	LDA #>p_LATEST
	STA LATEST+1

	LDA #<USER_BASE
	STA DP
	LDA #>USER_BASE
	STA DP+1

	; Initialize bootP (pointer into Bootstrap code)
	LDA #<BOOT_PRG
	STA BOOTP
	LDA #>BOOT_PRG
	STA BOOTP+1
	
; This is a Direct Threaded Code based FORTH
	
; Load the entry point of our main FORTH
; program and start execution (with JMP NEXT)
	; Place forth_prog ADDR into IP register
	LDA #<forth_prog
	STA IP
	LDA #>forth_prog
	STA IP+1

; Start FORTH
	BRA NEXT

; Lots of primitive words were ending with:
; 	DEX
;	DEX
;	JMP NEXT

; I have put this right here before NEXT, so now in the
; primitive word I jump to DEX2_NEXT, and DEX2_NEXT will
; fall through to NEXT. We save on ROM memory

; we don't add here INX INX jmp NEXT as this is
; like "jmp do_DROP"! --> use that.

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

;------------------------------------------------------
; For now, this is the Entry POint of our
; FORTH program.
	
forth_prog:

; set all IMMEDIATE flags in dictionnary
	.DW do_LIT, h_SEMICOLON, do_SETIMM

;	.DW do_DUP, do_PRINT, do_CRLF	; print

; Print version string
;	.DW do_LIT, VERS_STR
;	.DW do_COUNT, do_TYPE
	
; Restart Intepreter loop:
rsin:	.DW do_RSIN	; Reset Input
	
loop1:	.DW do_WORD	; ( addr len )
	.DW do_OVER, do_OVER	; 2DUP ( addr len addr len )

;	.DW do_OVER, do_OVER, do_TYPE	; DEBUG: Show TOKEN

	.DW do_FIND ; ( addr len hdr )
	.DW do_DUP  ; ( addr len hdr hdr )
	.DW do_0BR, numscan ; Not a word? Goto numscan

; Found a word! ( addr len hdr )
	.DW do_NROT, do_DROP, do_DROP ; ( hdr ) header of the word

	.DW do_LIT, MODE, do_CFETCH   ; ( hdr MODE ) 0: compile, 1 execute

	.DW do_0BR, compile ; Mode = 0 --> Compile
	; if not 0, Execute!

executeW:
	; ( hdr )
	.DW do_CFA  ; ( cfa )
	.DW do_EXEC ; ( )
	.DW do_JUMP, loop1

compile:
	; ( hdr )
	.DW do_DUP	; ( hdr hdr )
	.DW do_GETIMM	; ( hdr imm_flag )
	
	.DW do_0BR, executeW ; imm_flag=0 --> Immediate! (Execute)
	
	; otherwise, let's add to dictionary
	; ( hdr )
	.DW do_CFA  ; ( cfa )
	.DW do_COMMA ; ( )	; commit cfa to header
	
	.DW do_JUMP, loop1

numscan:
	.DW do_DROP ; ( addr len )
	.DW do_OVER, do_OVER ; 2DUP ( addr len addr len )
	.DW do_NUMBER ; ( addr len n ) n or garbage if ERROR

	.DW do_LIT, ERROR  ; ( addr len n Addr )
	.DW do_CFETCH	; ( addr len n Error )
	.DW do_0BR, cleanStack ; 0 -> no error => clean stack & loop

; Error: ( addr len n )
	.DW do_DROP ; ( addr len )
	.DW do_TYPE ; ( -- ) print the unknown word
	.DW do_LIT, WHAT_STR
	.DW do_COUNT, do_TYPE
	.DW do_JUMP, rsin ; reset input buffer

cleanStack:  ; ( addr len n )
	.DW do_NROT, do_DROP, do_DROP ; ( n )
	
	; here we have the number N on the stack.
	; are we in compilation mode?

	.DW do_LIT, MODE, do_CFETCH   ; ( n MODE ) 0: compile, 1 execute

	.DW do_0BR, commitN ; Mode = 0 --> CommitN to new word
	; if not 0, continue loop (N already on the stack)
	
	.DW do_JUMP, loop1

commitN:
	; ( n )
	; Add number to the word we are defining
	.DW do_LIT, do_LIT, do_COMMA	; first add "LIT" ( n )
	.DW do_COMMA			; add n to word (  )

	.DW do_JUMP, loop1

;------------------------------------------------------

	; below are other old tests...


	.DW do_LIT, BOOT_PRG ; Addr of "TEST_STR" string
	.DW do_DUP
	.DW do_CFETCH	     ; get length
	.DW do_SWAP
	.DW do_1PLUS	     ; Add 1 --> point to STR
	.DW do_LIT, INPUT
	.DW do_ROT	     ; ( src dst len )
	.DW do_CMOVE
loop:	.DW do_WORD
	.DW do_FIND
	.DW do_DUP, do_PRINT, do_CRLF
	.DW do_CFA
	.DW do_DUP, do_PRINT, do_CRLF

	; wait a key!
	.DW do_GETC, do_JUMP, loop
	



;	.DW word1

;	.DW do_DUP, do_PRINT, do_CRLF

	.DW do_PUSH1, do_EQZ
	.DW do_DUP, do_PRINT, do_CRLF	; print
	
	; Put Addr of a string
	.DW do_LIT
	.DW h_PRINT+3
		
	; Put LENGTH of the string
	.DW do_LIT, $0005
	
	.DW do_FIND
	


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
	.DW BOOT_PRG

	.DW do_FIND
	
	.DW do_LIT
	.DW $2FF4

	.DW do_STORE

	.DW do_PUSH1
	.DW do_GETC
	.DW do_PLUS
	.DW do_EMIT
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

;------------------------------------------------------

h_COLON:
	.DW $0000
	.STR "DOCOL"
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
	.STR "SEMI"
do_SEMI:
; POP IP from Return Stack
	PLA
	STA IP
	PLA
	STA IP+1
; JMP NEXT
	JMP NEXT

; RSIN ( -- )
; Reset Input buffer
; called on start-up, and each parse error
h_RSIN:
	.DW h_SEMI
	.STR "RSIN"
do_RSIN:
	STZ INP_LEN
	STZ INP_IDX
	JMP NEXT

h_SWAP:
	.DW h_RSIN
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

h_ROT:
	.DW h_SWAP
	.STR "ROT"
do_ROT:
; ( x y z -- y z x )
; X, stack 2 -> W
	LDA 6,X
	STA W
	LDA 7,X
	STA W+1
; Y, stack 1 -> stack 2
	LDA 4,X
	STA 6,X
	LDA 5,X
	STA 7,X
; Z, stack 0 -> stack 1
	LDA 2,X
	STA 4,X
	LDA 3,X
	STA 5,X
; W --> Stack 0
	LDA W
	STA 2,X
	LDA W+1
	STA 3,X
	JMP NEXT

h_NROT:
	.DW h_ROT
	.STR "-ROT"
do_NROT:
; ( x y z -- z x y )
; Stack 0 --> W
	LDA 2,X
	STA W
	LDA 3,X
	STA W+1
; Stack 1 --> Stack 0
	LDA 4,X
	STA 2,X
	LDA 5,X
	STA 3,X
; Stack 2 --> Stack 1
	LDA 6,X
	STA 4,X
	LDA 7,X
	STA 5,X
; W --> Stack 2
	LDA W
	STA 6,X
	LDA W+1
	STA 7,X
	JMP NEXT

h_OVER:
	.DW h_NROT
	.STR "OVER"
do_OVER:
; ( x y -- x y x )
	LDA 4,X
	STA 0,X
	LDA 5,X
	STA 1,X
	JMP DEX2_NEXT

h_DROP:
	.DW h_OVER
	.STR "DROP"
do_DROP:
	INX
	INX
	JMP NEXT

; Convenience BREAK word we can add in the code
; to force a BREAK
h_BREAK:
	.DW h_DROP
	.STR "BREAK"
do_BREAK:
	JMP NEXT	; set Breakpoint here!

		
h_PUSH0:
	.DW h_BREAK
	.STR "0"
do_PUSH0:
	STZ 0,x
	STZ 1,x
	JMP DEX2_NEXT

h_PUSH1:
	.DW h_PUSH0
	.STR "1"
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

h_MINUS:
	.DW h_PLUS
	.STR "-"
do_MINUS:
	SEC
	LDA 4,X
	SBC 2,X
	STA 4,X
	LDA 5,X
	SBC 3,X
	STA 5,X
	JMP do_DROP

h_1PLUS:
	.DW h_MINUS
	.STR "1+"
do_1PLUS:
	CLC
	INC 2,X
	BNE .skip
	INC 3,X
.skip:	JMP NEXT

h_EMIT:
	.DW h_1PLUS
	.STR "EMIT"
do_EMIT: ; EMIT emit a single char
	; char is on stack
	LDA 2,X
	JSR putc
	JMP do_DROP

h_GETC:
	.DW h_EMIT
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

; Branch to Label if 0 on stack
h_0BR:
	.DW h_AT_R
	.STR "0BR"
do_0BR:
	LDA 2,X
	ORA 3,X

	BNE .not0
	INX
	INX
	BRA do_JUMP	; 0?
.not0:	

; Now advance IP
; IP+2 --> IP
	LDA IP
	ADC #2
	STA IP
	BCC .skip
	INC IP+1
.skip:

	JMP do_DROP

h_JUMP:
	.DW h_0BR
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
end_do_STORE:		; used by CSTORE (below)
	INX
	INX
	;INX       ; INX INX NEXT is do_DROP
	;INX
	;JMP NEXT 	
	JMP do_DROP

h_CFETCH:
	.DW h_STORE
	.STR "C@"
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
.nxt_word:
; store W+2 in G1 (G1 points to the counted str)
	CLC
	LDA W
	ADC #HDR_OFFSET_STR
	STA G1
	LDA W+1		; replace with BCC skip / INC G1+1 ?
	ADC #0		;
	STA G1+1	;

; compare length
	LDA (G1)	; load current dictionay word's length
	AND #$1F		; remove flags (3 MSB)
	CMP 2,X		; compare to len on stack (1byte)
	BNE .advance_w	; not same length, advance to next word
; same length: compare str
	; G1+1 --> G1 (now points to STR, not length)
	CLC
	INC G1
	BNE .skip
	INC G1+1
.skip:	
	TAY		; we previously loaded LEN in A --> Y
	JSR STRCMP
	BEQ .found

; not found: look for next word in
; dictionnary

.advance_w:
	; W points to the previous entry
	; (W) -> W
	LDA (W)
	STA 0,X ; we store it there temporarily
	LDY #1
	LDA (W),Y
	STA W+1
	LDA 0,X
	STA W
	BNE .nxt_word
	LDA W+1
	BNE .nxt_word
	; here: not found :(, we put 00 on stack and exit
	STZ 4,x
	STZ 5,x
	JMP do_DROP
	
.found:	; ADDR is W -> TOS
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

; Print data on top of stack (in hex for now)
; ( n -- )
h_PRINT:
	.DW h_DP
	.STR "."
do_PRINT:
	LDA 3,X
	JSR print_byte
	LDA 2,X
	JSR print_byte
	LDA #' '
	JSR putc
	JMP do_DROP

; COUNT: ( addr -- addr+1 len )
; Converts a counted string, whose length is contained in
; the 1rst byte, into the form appropriate for TYPE, by
; leaving the address of the first character and the
; length on the stack.

h_COUNT:
	.DW h_PRINT
	.STR "COUNT"
do_COUNT:
	LDA 2,X
	STA W
	LDA 3,X
	STA W+1

	LDA (W)		; 1rst byte is length
	STA 0,X		; add on top of stack
	STZ 1,X

	INC 2,X		; addr++
	BNE .skip
	INC 3,X
.skip:
	JMP DEX2_NEXT


; Print a STRING	( addr len -- )
; addr --> pointer to 1rst char of string
; len  --> length of string (1 byte)
h_TYPE:
	.DW h_COUNT
	.STR "TYPE"
do_TYPE:
	LDA 2,X		; Length (one byte, max 256)
	BEQ .exit	; len = 0, exit
	
	STA G1		; we save length in G1
	LDY #0
	
	; save ADDR to STR
	LDA 4,X
	STA W
	LDA 5,X
	STA W+1
	
.loop:	LDA (W),y
	JSR putc

	INY
	CPY G1		; Y ?= Length --> end
	BNE .loop

.exit:	INX
	INX
	JMP do_DROP


; Print a space		( -- )
h_SPACE:
	.DW h_TYPE
	.STR "SPACE"
do_SPACE:
	LDA #' '
	JSR putc
	JMP NEXT

; Print a new line	( -- )
h_CRLF:
	.DW h_SPACE
	.STR "CRLF"
do_CRLF:
	JSR _crlf
	JMP NEXT

_crlf:
	LDA #$0a ; CR
	JSR putc
	LDA #$0d ; LF
	JSR putc
	RTS

; 0=, it's also equivalent to "logical NOT" (not a bitwise NOT)
; logical NOT --> use 0=
; 0<> --> use 0= 0=  (twice!)
h_EQZ:
	.DW h_CRLF
	.STR "0="
do_EQZ:
; ( n -- bool )
; TRUE  (FFFF) if n is 0000
; FALSE (0000) otherwise
	LDA 2,X
	ORA 3,X
	BEQ .true
.false:	STZ 2,X
	STZ 3,X
	JMP NEXT
.true:
	LDA #$FF
	STA 2,X
	STA 3,X
	JMP NEXT

h_AND:
	.DW h_EQZ
	.STR "AND"
do_AND:
; ( a b -- a&b ) bitwise AND
	LDA 2,X
	AND 4,X
	STA 4,X
	LDA 3,X
	AND 5,X
	STA 5,X
	JMP do_DROP

h_OR:
	.DW h_AND
	.STR "OR"
do_OR:
; ( a b -- a|b ) bitwise OR
	LDA 2,X
	ORA 4,X
	STA 4,X
	LDA 3,X
	ORA 5,X
	STA 5,X
	JMP do_DROP

h_NOT:
	.DW h_OR
	.STR "NOT"
do_NOT:
; ( a -- not(a) ) bitwise NOT
	LDA 2,X
	EOR #$FF
	STA 2,X
	LDA 3,X
	EOR #$FF
	STA 3,X
	JMP NEXT

h_GETIMM:
	.DW h_NOT
	.STR "GETIMM"
do_GETIMM:
; ( hdr -- imm_flag )
; BEWARE: Returns 0 if word IS indeed immediate
; Immediate flag is kept in MSB of str len
	JSR _getWordLen

	STZ 3,X		; clear HI

	BMI .isImm	; branch if IMMEDIATE flag set
; not set
	STA 2,X		; set LO to LEN (not 0 ie not immediate)
	JMP NEXT
.isImm:	
	STZ 2,X		; clear LO, and exit
	JMP NEXT

h_SETIMM:
	.DW h_GETIMM
	.STR "SETIMM"
do_SETIMM:
; ( hdr -- )
; takes a header to a word in dictionary
; and sets its Immediate flag
	JSR _getWordLen

	ORA #$80	; MSB set
	STA (W),Y	; LEN
		
	JMP do_DROP

_getWordLen:
; ( hdr -- hdr )
; store's WORD header's addr in W
; set Y to 2. 
; (W),Y point to LEN
; returns LEN in A
	LDA 2,X
	STA W
	LDA 3,X
	STA W+1
	
	LDY #2
	LDA (W),Y	; LEN
	RTS

h_HERE:
	.DW h_SETIMM
	.STR "HERE"
do_HERE:
; : HERE	DP @ ;
	JMP do_COLON
	.DW do_DP, do_FETCH, do_SEMI

h_COMMA:
	.DW h_HERE
	.STR ","
do_COMMA:
; ( XX -- ) save a word XX to HERE and advance
; HERE by 2
; : , HERE ! HERE 2 + DP ! ;
	JMP do_COLON
	.DW do_HERE, do_STORE
	.DW do_HERE, do_1PLUS, do_1PLUS
	.DW do_DP, do_STORE
	.DW do_SEMI

h_CCOMMA:
	.DW h_COMMA
	.STR "C,"
do_CCOMMA:
; ( C -- ) save a byte C to HERE and advance
; HERE by 1
; : , HERE ! HERE 2 + DP ! ;
	JMP do_COLON
	.DW do_HERE, do_CSTORE
	.DW do_HERE, do_1PLUS
	.DW do_DP, do_STORE
	.DW do_SEMI
	
h_CREATE:
	.DW h_CCOMMA
	.STR ":"
do_CREATE:
; get next TOKEN in INPUT and creates 
; a Header for a new word
	JMP do_COLON
	.DW do_HERE		; keep current HERE on stack
	.DW do_LATEST, do_FETCH, do_COMMA ; store value of LATEST in the Link of new Header
	.DW do_LATEST, do_STORE ; store "old HERE" in LATEST

	.DW do_WORD		; get next TOKEN in INPUT (new word's name)
	.DW do_DUP, do_CCOMMA	; store LEN in Header
	.DW do_DUP, do_NROT	; ( len addr len )
	.DW do_HERE, do_SWAP, do_CMOVE ; store name
	.DW do_ALLOT		; advance HERE by LEN
	
	.DW do_LIT, $004C, do_CCOMMA	; store a 4C (JMP)
	.DW do_LIT, do_COLON, do_COMMA	; store do_COLON's addr
	
	.DW do_PUSH0, do_LIT, MODE, do_CSTORE ; Enter Compilation mode
	
	.DW do_SEMI


h_SEMICOLON:		; IMMEDIATE
	.DW h_CREATE
	.STR ";"
do_SEMICOLON:
; Add's do_SEMI to header of word being defined
; and exits COMPILATION mode (1->MODE)
	JMP do_COLON
	.DW do_LIT, do_SEMI, do_COMMA	; commits do_SEMI addr
	
	.DW do_PUSH1, do_LIT, MODE, do_CSTORE ; Exits Compilation mode
	
	.DW do_SEMI

h_LATEST:
	.DW h_SEMICOLON
	.STR "LATEST"
do_LATEST:
; ( -- LATEST )
	LDA #<LATEST
	STA 0,X
	LDA #>LATEST
	STA 1,X
	JMP DEX2_NEXT

h_ALLOT:
	.DW h_LATEST
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
	.DW do_DUP, do_CFETCH 	; ( LEN ) with FLAG
	.DW do_LIT, $001F, do_AND ; ( LEN ) w/o FLAGs
	.DW do_1PLUS, do_PLUS ; DUP c@ 1+ +	; add length
	.DW do_SEMI

; Put Data Stack Pointer on the stack
h_SP:
	.DW h_CFA
	.STR "SP"
do_SP:
	TXA
	STA 0,X
	STZ 1,X
	JMP DEX2_NEXT

h_CMOVE:
	.DW h_SP
	.STR "CMOVE"
do_CMOVE:
; (src dst len -- )
; copy len bytes from src to dst
	; LEN --> Y
	LDY 2,X
	; put SRC in G1
	LDA 6,X
	STA G1
	LDA 7,X
	STA G1+1
	; put DST in G2
	LDA 4,X
	STA G2
	LDA 5,X
	STA G2+1
.loop:
	DEY
	LDA (G1),Y
	STA (G2),Y
	CPY #0
	BNE .loop
	; remove top 3 elements of the stack
	TXA
	CLC
	ADC #6
	TAX

	JMP NEXT

h_WORD:
	.DW h_CMOVE
	.STR "WORD"
do_WORD:
; Find next word in input buffer (and advance INP_IDX)
; ( -- ADDR LEN )
	; INPUT --> W
	LDA #<INPUT
	STA W
	LDA #>INPUT
	STA W+1
	; INP_IDX --> Y
	LDY INP_IDX
.next1:
	CPY INP_LEN	; reached end of input string?
	BEQ .eos

	LDA (W),Y	; load char at (W)+Y in A
	
	CMP #' '
	BNE .startW
	INY
	BRA .next1

.eos:	; refill input string
	JSR getline	
	BRA do_WORD	; and try again

; start of word
.startW:
	; First we store the ADDR on stack
	TYA
	STA G1	; we save Y in G1, temporarily
	CLC
	ADC W
	STA 0,X
	LDA W+1		;
	ADC #0		; replace with BCC skip / INC ?
	STA 1,X		;
	DEX
	DEX
.next2:
	INY

	CPY INP_LEN	; reached end of input string?
	BEQ .endW

	LDA (W),Y	; load char at (W)+Y in A
	
	CMP #' '
	BEQ .endW
	BRA .next2
.endW:
	; compute length
	SEC
	TYA
	SBC G1	; earlier we saved Y in G1 ;)
	STA 0,X
	STZ 1,X
	; Save Y in INP_IDX
	STY INP_IDX
	;
	JMP DEX2_NEXT

; Put Data Stack Pointer on the stack
h_EXEC:
	.DW h_WORD
	.STR "EXEC"
do_EXEC:
	LDA 2,X
	STA W
	LDA 3,X
	STA W+1
	INX
	INX
	JMP (W)

h_NUMBER:
	.DW h_EXEC
	.STR "NUMBER"
do_NUMBER:
; ( ADDR LEN -- VALUE )
; ADDR points to the string representing the value
; LEN length of the string representing the value

; on error (not a number), we set the ERROR variable
; VALUE on stack is left random (likely we'll drop it later)

; temp registers used
; W <- ADDR
	LDA 4,X
	STA W
	LDA 5,X
	STA W+1
; G1 <- LEN
	LDA 2,X
	STA G1
; G2 <-- 0000
	STZ G2
	STZ G2+1
; Y <-- 0, index
	LDY #0
; Reset error flag (no error by default)
	STZ ERROR

.next:
	LDA (W),Y
	JSR nibble_asc_to_value
	BCS .err
	
	ORA G2
	STA G2
	
	INY
	CPY G1	; Y = LEN ? --> end
	BEQ .go
; <<4
	ASL G2
	ROL G2+1
	ASL G2
	ROL G2+1
	ASL G2
	ROL G2+1
	ASL G2
	ROL G2+1
	BRA .next
.go:
; leave results G2 on the stack
	LDA G2
	STA 4,X
	LDA G2+1
	STA 5,X
	BRA .drop
.err:	
	LDA #1
	STA ERROR
.drop:	JMP do_DROP


h_INPUT:
	.DW h_NUMBER
	.STR "INPUT"
do_INPUT:
	JSR getline
	JMP NEXT

;-----------------------------------------------------------------
; ALWAYS update the latest word's 
; header address h_*
p_LATEST .= h_INPUT



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
	
; Input Buffer Routines

; Getline refills the INPUT buffer
; Bootstrapping mode BOOT<>0 then refill from BOOT_PRG
; If we are in user mode (BOOT=0) we refill from user input
getline:
	STZ INP_IDX	; reset Input index
	LDY #0

	LDA BOOT
	BNE boot_refill

.next:	JSR getc

	CMP #BKSPACE
	BEQ .bkspace

	CMP #$0D ; \n
	BEQ .finish

	CPY #MAX_LEN
	BEQ .maxlen

	STA INPUT,y	; save char to INPUT
	JSR putc	; echo char
	INY

	BRA .next
.maxlen:
	PHA
	LDA #BKSPACE	; send bckspace to erase last char
	JSR putc
	PLA		; restore new char
	STA INPUT-1,y	; save char to INPUT
	JSR putc
	BRA .next
.bkspace:
	CPY #0		; start of line?
	BEQ .next	; do nothing
	JSR putc	; echo char
	DEY		; else: Y--
	BRA .next
.finish:
	STY INP_LEN
	JSR _crlf
	RTS


; boot_refill will refill only one token (word) from BOOT_PRG
; into INPUT buffer. Not super efficient I guess...
boot_refill:
	; commented out as we come from getline
	; STZ INP_IDX	; reset Input index
	; LDY #0

	; commented out as we come from getline
	; W already contains INPUT
	; LDA #<INPUT
	; STA W
	; LDA #>INPUT
	; STA W+1

	LDA BOOTP
	STA G1
	LDA BOOTP+1
	STA G1+1

	DEY
.next:	INY
	LDA (G1),Y

	CMP #$20 ; space
	BEQ .next

	CMP #$0A
	BEQ .next

	CMP #$0D
	BEQ .next

; if we get here we've found a new WORD
	; add Y to G1
	TYA
	CLC
	ADC G1
	STA G1
	BCC .skip1
	INC G1+1
.skip1:
	LDY #0

.next2:
	LDA (G1),Y

	BEQ .eobc	; $00, end of boostrap code

	CMP #$20 ; space
	BEQ .endW

	CMP #$0A
	BEQ .endW

	CMP #$0D
	BEQ .endW

	; Add letter to INPUT
	STA (W),Y
	INY
	BRA .next2
.eobc:
	STZ BOOT	; clear BootStrap mode
.endW:
	; save word's length
	STY INP_LEN
	STZ INP_IDX

	; Advance W by Y -> BOOTP
	CLC
	TYA
	ADC G1
	STA BOOTP
	BCC .skip2
	INC G1+1
.skip2:
	LDA G1+1
	STA BOOTP+1

	RTS

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
	BCC .skip
	ADC #$66
.skip:
	EOR #$30
	RTS

nibble_asc_to_value:
; converts a char representing a hex-digit (nibble)
; into the corresponding hex value

; boundary check is it a digit?
	CMP #'0'
	BMI .err
	CMP #'F'+1
	BPL .err
	CMP #'9'+1
	BMI .conv
	CMP #'A'
	BPL .conv
.err:	; nibble wasn't valid, error
	SEC
	RTS
.conv:	; conversion happens here
	CMP #$41
	BMI .less
	SBC #$37
.less:
	AND #$0F
	CLC
	RTS

; Interrupts routines

IRQ_vec:
NMI_vec:
	RTI

VERS_STR: .STR "ALEX FORTH v0", $0A, $0D
WHAT_STR: .STR " ?", $0A, $0D

; Bootstrap code:
; At this point we can extend our forth in forth
; Must end with $00. That will exit BOOTstrap mode and
; enter the interpreter
BOOT_PRG:
	.DB " : ? @ . ; "
	.DB $00



	*= $0200

LATEST	.DS 2	; Store the latest ADDR of the Dictionary
MODE	.DS 1	; <>0 Execute, 0 compile
BOOT	.DS 1	; <>0 Boot, 0 not boot anymore
ERROR	.DS 1	; Error when converting number
INP_LEN .DS 1	;
INPUT	.DS 80	; CMD string (extend as needed, up to 256!)
INP_IDX .DS 1	; Index into the INPUT Buffer (for reading it with KEY)
DP	.DS 2	; Data Pointer: Store the latest ADDR of next free space in RAM (HERE)


; Base of user memory area.
USER_BASE:

; system vectors

    *=  $FFFA

    .word   NMI_vec     ; NMI vector
    .word   RES_vec     ; RESET vector
    .WORD   IRQ_vec     ; IRQ vector
