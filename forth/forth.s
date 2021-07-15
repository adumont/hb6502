; FORTH 
; Alex Dumont

; Help & Reference:
; [Bitwise, Day 35: Implementing Forth](https://www.youtube.com/watch?v=rlayTh3sjiw)
; [Moving Forth: Part 1](https://www.bradrodriguez.com/papers/moving1.htm)

.pc02 ; 65C02 mode
.debuginfo      +       ; Generate debug info
.feature string_escapes

__word_last .set 0
__word_0 = 0

; Macro to encode FORTH counted strings
; Answered at https://retrocomputing.stackexchange.com/a/20379/19750
.macro CString str, immflag
	.ifnblank immflag
		.byte :++  -  :+  +128
	.else
		; we diff the two unamed labels :++ and :+ --> the length
		.byte :++  -  :+
	.endif
:	.byte str
:
.endmacro

; macro for new word headers
; Answered at https://retrocomputing.stackexchange.com/a/20161/19750
.macro defword label, strname, immflag
	.ident (.sprintf("h_%s", label)):
	.ident (.sprintf("__word_%u", __word_last + 1)):

	.addr .ident(.sprintf("__word_%u", __word_last))
	CString strname, immflag

	__word_last .set __word_last + 1

	.ident (.sprintf("do_%s", label)):
.endmacro

; IP: Next Instruction Pointer (IP)-->W
; W : Address of the code to run
W	= $FE		; 2 bytes, an address
IP	= W -2
G2	= IP-2		; general purpose register
G1	= G2-2		; general purpose register
DTOP	= G1-2		; Stack TOP
BKSPACE = $08 ; BACKSPACE = CTRL+BCKSPACE in LINUX (Python)
MAX_LEN = 80		; Input Buffer MAX length

; Offset of the WORD name in the label
; 2 bytes after the Header's addr
HDR_OFFSET_STR = 2	

;	*= $8000
.segment  "CODE"

RES_vec:
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
	BCC @skip
	INC IP+1
@skip:
	JMP (W)

;------------------------------------------------------
; For now, this is the Entry POint of our
; FORTH program.
	
forth_prog:

; set all IMMEDIATE flags in dictionary
; we won't be able to do that in ROM, but Kowalski doesn't matter
	.ADDR do_LIT, h_SEMICOLON, do_SETIMM
	.ADDR do_LIT, h_LBRAC, do_SETIMM
	.ADDR do_LIT, h_SQUOT, do_SETIMM

;	.ADDR do_DUP, do_PRINT, do_CRLF	; print

; Print version string
	.ADDR do_LIT, VERS_STR
	.ADDR do_COUNT, do_TYPE

; test LITSTR

;	.ADDR do_LITSTR
;	.STR "FORTH"
;	.ADDR do_COUNT, do_TYPE
	
; Restart Intepreter loop:
rsin:	.ADDR do_RSIN	; Reset Input
	
loop1:	.ADDR do_WORD	; ( addr len )
	.ADDR do_OVER, do_OVER	; 2DUP ( addr len addr len )

;	.ADDR do_OVER, do_OVER, do_TYPE	; DEBUG: Show TOKEN

	.ADDR do_FIND ; ( addr len hdr )
	.ADDR do_DUP  ; ( addr len hdr hdr )
	.ADDR do_0BR, numscan ; Not a word? Goto numscan

; Found a word! ( addr len hdr )
	.ADDR do_NROT, do_DROP, do_DROP ; ( hdr ) header of the word

	.ADDR do_MODE, do_CFETCH   ; ( hdr MODE ) 0: compile, 1 execute

	.ADDR do_0BR, compile ; Mode = 0 --> Compile
	; if not 0, Execute!

executeW:
	; ( hdr )
	.ADDR do_CFA  ; ( cfa )
	.ADDR do_EXEC ; ( )
	.ADDR do_JUMP, loop1

compile:
	; ( hdr )
	.ADDR do_DUP	; ( hdr hdr )
	.ADDR do_GETIMM	; ( hdr imm_flag )
	
	.ADDR do_0BR, executeW ; imm_flag=0 --> Immediate! (Execute)
	
	; otherwise, let's add to dictionary
	; ( hdr )
	.ADDR do_CFA  ; ( cfa )
	.ADDR do_COMMA ; ( )	; commit cfa to header
	
	.ADDR do_JUMP, loop1

numscan:
	.ADDR do_DROP ; ( addr len )
	.ADDR do_OVER, do_OVER ; 2DUP ( addr len addr len )
	.ADDR do_NUMBER ; ( addr len n ) n or garbage if ERROR

	.ADDR do_LIT, ERROR  ; ( addr len n Addr )
	.ADDR do_CFETCH	; ( addr len n Error )
	.ADDR do_0BR, cleanStack ; 0 -> no error => clean stack & loop

; Error: ( addr len n )
	.ADDR do_DROP ; ( addr len )
	.ADDR do_TYPE ; ( -- ) print the unknown word
	.ADDR do_LIT, WHAT_STR
	.ADDR do_COUNT, do_TYPE


	; if we were in compilation mode, we have to cancel the last word
	; that was started (but is unfinished). we have to restore LATEST to its
	; previous value (which is in LATEST @ )
	.ADDR do_MODE, do_CFETCH   ; ( n MODE ) 0: compile, 1 execute
	.ADDR do_0BR, removeW ; Mode = 0 --> remove unfinished word

executeMode:
	; 1->MODE (back to Execute mode, cancel compilation mode)
	.ADDR do_LBRAC
	.ADDR do_JUMP, rsin ; reset input buffer

removeW:
	.ADDR do_LATEST, do_FETCH, do_FETCH ; LATEST @ @
	.ADDR do_LATEST, do_STORE           ; LATEST !
	.ADDR do_JUMP, executeMode ; back to execute mode and reset input buffer

cleanStack:  ; ( addr len n )
	.ADDR do_NROT, do_DROP, do_DROP ; ( n )
	
	; here we have the number N on the stack.
	; are we in compilation mode?

	.ADDR do_MODE, do_CFETCH   ; ( n MODE ) 0: compile, 1 execute

	.ADDR do_0BR, commitN ; Mode = 0 --> CommitN to new word
	; if not 0, continue loop (N already on the stack)
	
	.ADDR do_JUMP, loop1

commitN:
	; ( n )
	; Add number to the word we are defining
	.ADDR do_LIT, do_LIT, do_COMMA	; first add "LIT" ( n )
	.ADDR do_COMMA			; add n to word (  )

	.ADDR do_JUMP, loop1

;------------------------------------------------------

h_COLON:
	.ADDR $0000
	CString "DOCOL"
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
	.ADDR h_COLON
	CString "SEMI"
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
	.ADDR h_SEMI
	CString "RSIN"
do_RSIN:
	STZ INP_LEN
	STZ INP_IDX
	JMP NEXT

h_SWAP:
	.ADDR h_RSIN
	CString "SWAP"
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
	.ADDR h_SWAP
	CString "ROT"
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
	.ADDR h_ROT
	CString "-ROT"
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
	.ADDR h_NROT
	CString "OVER"
do_OVER:
; ( x y -- x y x )
	LDA 4,X
	STA 0,X
	LDA 5,X
	STA 1,X
	JMP DEX2_NEXT

h_DROP:
	.ADDR h_OVER
	CString "DROP"
do_DROP:
	INX
	INX
	JMP NEXT

; Convenience BREAK word we can add in the code
; to force a BREAK
h_BREAK:
	.ADDR h_DROP
	CString "BREAK"
do_BREAK:
	JMP NEXT	; set Breakpoint here!

		
h_PUSH0:
	.ADDR h_BREAK
	CString "0"
do_PUSH0:
	STZ 0,x
	STZ 1,x
	JMP DEX2_NEXT

h_PUSH1:
	.ADDR h_PUSH0
	CString "1"
do_PUSH1:
	LDA #1
	STA 0,x
	STZ 1,x
	JMP DEX2_NEXT

; Push a literal word (2 bytes)
h_LIT:
	.ADDR h_PUSH1
	CString "LIT"
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
	BCC @skip
	INC IP+1
@skip:
	JMP DEX2_NEXT

h_DUP:
	.ADDR h_LIT
	CString "DUP"
do_DUP:
	LDA 2,X
	STA 0,X
	LDA 3,X
	STA 1,X
	JMP DEX2_NEXT
	
h_PLUS:
	.ADDR h_DUP
	CString "+"
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
	.ADDR h_PLUS
	CString "-"
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
	.ADDR h_MINUS
	CString "1+"
do_1PLUS:
	CLC
	INC 2,X
	BNE @skip
	INC 3,X
@skip:	JMP NEXT

h_EMIT:
	.ADDR h_1PLUS
	CString "EMIT"
do_EMIT: ; EMIT emit a single char
	; char is on stack
	LDA 2,X
	JSR putc
	JMP do_DROP

h_GETC:
	.ADDR h_EMIT
	CString "GETC"
do_GETC:
; get a single char from IO, leave on stack
	JSR getc ; leaves the char in A
	STA 0,X
	STZ 1,X
	JMP DEX2_NEXT


h_TO_R:
	.ADDR h_GETC
	CString ">R"
do_TO_R:
; >R: pop a cell (possibly an ADDR) from
; the stack and pushes it to the Return Stack
	LDA 3,X
	PHA
	LDA 2,X
	PHA
	JMP do_DROP

h_FROM_R:
	.ADDR h_TO_R
	CString "<R"
do_FROM_R:
; <R: pop a cell from the Return Stack
; and pushes it to the Stack
	PLA
	STA 0,X
	PLA
	STA 1,X
	JMP DEX2_NEXT


h_AT_R:
	.ADDR h_FROM_R
	CString "@R"
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
	.ADDR h_AT_R
	CString "0BR"
do_0BR:
	LDA 2,X
	ORA 3,X

	BNE @not0
	INX
	INX
	BRA do_JUMP	; 0?
@not0:	

; Now advance IP
; IP+2 --> IP
	LDA IP
	ADC #2
	STA IP
	BCC @skip
	INC IP+1
@skip:

	JMP do_DROP

h_JUMP:
	.ADDR h_0BR
	CString "JUMP"
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
	.ADDR h_JUMP
	CString "@"
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
	.ADDR h_FETCH
	CString "!"
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
	.ADDR h_STORE
	CString "C@"
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
	.ADDR h_CFETCH
	CString "C!"
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
	.ADDR h_CSTORE
	CString "FIND"
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
@nxt_word:
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
	BNE @advance_w	; not same length, advance to next word
; same length: compare str
	; G1+1 --> G1 (now points to STR, not length)
	CLC
	INC G1
	BNE @skip
	INC G1+1
@skip:	
	TAY		; we previously loaded LEN in A --> Y
	JSR STRCMP
	BEQ @found

; not found: look for next word in
; dictionnary

@advance_w:
	; W points to the previous entry
	; (W) -> W
	LDA (W)
	STA 0,X ; we store it there temporarily
	LDY #1
	LDA (W),Y
	STA W+1
	LDA 0,X
	STA W
	BNE @nxt_word
	LDA W+1
	BNE @nxt_word
	; here: not found :(, we put 00 on stack and exit
	STZ 4,x
	STZ 5,x
	JMP do_DROP
	
@found:	; ADDR is W -> TOS
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
@next_char:
	DEY
	LDA (G1),Y
	CMP (G2),Y
	BNE @strcmp_exit
	CPY #0
	BNE @next_char
@strcmp_exit:
	RTS

h_DP:
	.ADDR h_FIND
	CString "DP"
do_DP:
; DP ( -- addr )

; Alternative, as word definition:
;	JMP do_COLON
;	.ADDR do_LIT, CP, do_SEMI
	LDA #<DP
	STA 0,X
	LDA #>DP
	STA 1,X
	JMP DEX2_NEXT

; Print data on top of stack (in hex for now)
; ( n -- )
h_PRINT:
	.ADDR h_DP
	CString "."
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
	.ADDR h_PRINT
	CString "COUNT"
do_COUNT:
	LDA 2,X
	STA W
	LDA 3,X
	STA W+1

	LDA (W)		; 1rst byte is length
	STA 0,X		; add on top of stack
	STZ 1,X

	INC 2,X		; addr++
	BNE @skip
	INC 3,X
@skip:
	JMP DEX2_NEXT


; Print a STRING	( addr len -- )
; addr --> pointer to 1rst char of string
; len  --> length of string (1 byte)
h_TYPE:
	.ADDR h_COUNT
	CString "TYPE"
do_TYPE:
	LDA 2,X		; Length (one byte, max 256)
	BEQ @exit	; len = 0, exit
	
	STA G1		; we save length in G1
	LDY #0
	
	; save ADDR to STR
	LDA 4,X
	STA W
	LDA 5,X
	STA W+1
	
@loop:	LDA (W),y
	JSR putc

	INY
	CPY G1		; Y ?= Length --> end
	BNE @loop

@exit:	INX
	INX
	JMP do_DROP


; Print a space		( -- )
h_SPACE:
	.ADDR h_TYPE
	CString "SPACE"
do_SPACE:
	LDA #' '
	JSR putc
	JMP NEXT

; Print a new line	( -- )
h_CRLF:
	.ADDR h_SPACE
	CString "CRLF"
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
	.ADDR h_CRLF
	CString "0="
do_EQZ:
; ( n -- bool )
; TRUE  (FFFF) if n is 0000
; FALSE (0000) otherwise
	LDA 2,X
	ORA 3,X
	BEQ @true
@false:	STZ 2,X
	STZ 3,X
	JMP NEXT
@true:
	LDA #$FF
	STA 2,X
	STA 3,X
	JMP NEXT

h_AND:
	.ADDR h_EQZ
	CString "AND"
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
	.ADDR h_AND
	CString "OR"
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
	.ADDR h_OR
	CString "NOT"
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
	.ADDR h_NOT
	CString "GETIMM"
do_GETIMM:
; ( hdr -- imm_flag )
; BEWARE: Returns 0 if word IS indeed immediate
; Immediate flag is kept in MSB of str len
	JSR _getWordLen

	STZ 3,X		; clear HI

	BMI @isImm	; branch if IMMEDIATE flag set
; not set
	STA 2,X		; set LO to LEN (not 0 ie not immediate)
	JMP NEXT
@isImm:	
	STZ 2,X		; clear LO, and exit
	JMP NEXT

h_SETIMM:
	.ADDR h_GETIMM
	CString "SETIMM"
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
; (W),Y point to LEN field (including Immediate flag)
; returns LEN in A
	LDA 2,X
	STA W
	LDA 3,X
	STA W+1
	
	LDY #2
	LDA (W),Y	; LEN
	RTS

h_HERE:
	.ADDR h_SETIMM
	CString "HERE"
do_HERE:
; : HERE	DP @ ;
	JMP do_COLON
	.ADDR do_DP, do_FETCH, do_SEMI

h_COMMA:
	.ADDR h_HERE
	CString ","
do_COMMA:
; ( XX -- ) save a word XX to HERE and advance
; HERE by 2
; : , HERE ! HERE 2 + DP ! ;
	JMP do_COLON
	.ADDR do_HERE, do_STORE
	.ADDR do_HERE, do_1PLUS, do_1PLUS
	.ADDR do_DP, do_STORE
	.ADDR do_SEMI

h_CCOMMA:
	.ADDR h_COMMA
	CString "C,"
do_CCOMMA:
; ( C -- ) save a byte C to HERE and advance
; HERE by 1
; : , HERE ! HERE 2 + DP ! ;
	JMP do_COLON
	.ADDR do_HERE, do_CSTORE
	.ADDR do_HERE, do_1PLUS
	.ADDR do_DP, do_STORE
	.ADDR do_SEMI

h_LBRAC:	; IMMEDIATE
	.ADDR h_CCOMMA
	CString "["
do_LBRAC:
; ( -- ) switch to EXECUTE/IMMEDIATE mode
	LDA #1
	STA MODE
	JMP NEXT

h_RBRAC:
	.ADDR h_LBRAC
	CString "]"
do_RBRAC:
; ( -- ) switch to COMPILE mode
	STZ MODE
	JMP NEXT

h_CREATE:
	.ADDR h_RBRAC
	CString ":"
do_CREATE:
; get next TOKEN in INPUT and creates 
; a Header for a new word
	JMP do_COLON
	.ADDR do_HERE		; keep current HERE on stack
	.ADDR do_LATEST, do_FETCH, do_COMMA ; store value of LATEST in the Link of new Header
	.ADDR do_LATEST, do_STORE ; store "old HERE" in LATEST

	.ADDR do_WORD		; get next TOKEN in INPUT (new word's name)
	.ADDR do_DUP, do_CCOMMA	; store LEN in Header
	.ADDR do_DUP, do_NROT	; ( len addr len )
	.ADDR do_HERE, do_SWAP, do_CMOVE ; store name
	.ADDR do_ALLOT		; advance HERE by LEN

	.ADDR do_CLIT	;
	.BYTE $4C		; store a 4C (JMP)
	.ADDR do_CCOMMA	;
	
	.ADDR do_LIT, do_COLON, do_COMMA	; store do_COLON's addr
	
	;.ADDR do_PUSH0, do_LIT, MODE, do_CSTORE ; Enter Compilation mode
	.ADDR do_RBRAC ; Enter Compilation mode
	
	.ADDR do_SEMI

h_VARIABLE:
	.ADDR h_CREATE
	CString "VARIABLE"
do_VARIABLE:
; get next TOKEN in INPUT and creates
; a Header for a new word
	JMP do_COLON
	.ADDR do_CREATE	; creates new header
	.ADDR do_LIT, do_LIT, do_COMMA
	.ADDR do_HERE		; put HERE on the stack
	.ADDR do_PUSH0, do_COMMA	; store 00 as
	.ADDR do_LIT, do_SEMI, do_COMMA	; word is complete
	.ADDR do_HERE, do_SWAP, do_STORE	; store the address right after the word into the address slot of the word
	.ADDR do_PUSH1, do_1PLUS, do_ALLOT
	.ADDR do_LBRAC ; Exits Compilation mode
	.ADDR do_SEMI

h_SEMICOLON:		; IMMEDIATE
	.ADDR h_VARIABLE
	CString ";"
do_SEMICOLON:
; Add's do_SEMI to header of word being defined
; and exits COMPILATION mode (1->MODE)
	JMP do_COLON
	.ADDR do_LIT, do_SEMI, do_COMMA	; commits do_SEMI addr
	
	;.ADDR do_PUSH1, do_LIT, MODE, do_CSTORE ; Exits Compilation mode
	.ADDR do_LBRAC ; Exits Compilation mode
	
	.ADDR do_SEMI

h_LATEST:
	.ADDR h_SEMICOLON
	CString "LATEST"
do_LATEST:
; ( -- LATEST )
	LDA #<LATEST
	STA 0,X
	LDA #>LATEST
	STA 1,X
	JMP DEX2_NEXT

h_MODE:
	.ADDR h_LATEST
	CString "MODE"
do_MODE:
; ( -- MODE ) MODE is the addr, not the value!
	LDA #<MODE
	STA 0,X
	LDA #>MODE
	STA 1,X
	JMP DEX2_NEXT

h_ALLOT:
	.ADDR h_MODE
	CString "ALLOT"
do_ALLOT:
; : ALLOT	HERE + DP ! ;
	JMP do_COLON
	.ADDR do_HERE, do_PLUS, do_DP, do_STORE
	.ADDR do_SEMI

h_CFA:
	.ADDR h_ALLOT
	CString ">CFA"
do_CFA:
	JMP do_COLON
	; ( ADDR -- ADDR )
	; takes the dictionary pointer to a word
	; returns the codeword pointer
	.ADDR do_1PLUS, do_1PLUS  ; 1+ 1+	; skip prev. word link
	.ADDR do_DUP, do_CFETCH 	; ( LEN ) with FLAG

	.ADDR do_CLIT	;
	.BYTE $1F		; ( LEN ) w/o FLAGs
	.ADDR do_AND	;

	.ADDR do_1PLUS, do_PLUS ; DUP c@ 1+ +	; add length
	.ADDR do_SEMI

; Put Data Stack Pointer on the stack
h_SP:
	.ADDR h_CFA
	CString "SP"
do_SP:
	TXA
	STA 0,X
	STZ 1,X
	JMP DEX2_NEXT

h_CMOVE:
	.ADDR h_SP
	CString "CMOVE"
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
@loop:
	DEY
	LDA (G1),Y
	STA (G2),Y
	CPY #0
	BNE @loop
	; remove top 3 elements of the stack
	TXA
	CLC
	ADC #6
	TAX

	JMP NEXT

h_WORD:
	.ADDR h_CMOVE
	CString "WORD"
do_WORD:
; Find next word in input buffer (and advance INP_IDX)
; ( -- ADDR LEN )

	; INPUT --> W
;	LDA #<INPUT
;	STA W
;	LDA #>INPUT
;	STA W+1

@next1:
	JSR _KEY
	
	CMP #' '
	BEQ @next1

	CMP #$0A
	BEQ @next1

	CMP #$0D
	BEQ @next1
	
	; otherwise --> start of a word (@startW)

; start of word
@startW:
	; First we store the ADDR on stack
	LDA INP_IDX
	STA G1	; we save Y in G1, temporarily
	DEA
	CLC
	ADC W
	STA 0,X
	LDA W+1		;
	ADC #0		; replace with BCC skip / INC ?
	STA 1,X		;
	DEX
	DEX
@next2:
	JSR _KEY

	CMP #' '
	BEQ @endW

	CMP #$0A
	BEQ @endW

	CMP #$0D
	BEQ @endW

	BRA @next2
@endW:
	; compute length
	LDA INP_IDX
	SEC
	SBC G1	; earlier we saved INP_IDX in G1 ;)
	STA 0,X
	STZ 1,X
	JMP DEX2_NEXT

h_KEY:
	.ADDR h_WORD
	CString "KEY"
do_KEY:
; Give next char in Input buffer
; ( -- char )
	JSR _KEY
	STA 0,X
	STZ 1,X
	JMP DEX2_NEXT

; internal routine that get a char from the input buffer
; leaves it in A
; advance INP_IDX. when reached end of buffer, we refill
_KEY:
	; INPUT --> W
	LDA #<INPUT
	STA W
	LDA #>INPUT
	STA W+1
@retry:
	; INP_IDX --> Y
	LDY INP_IDX

	CPY INP_LEN	; reached end of input string?
	BEQ @eos

	LDA (W),Y	; load char at (W)+Y in A
	INC INP_IDX	; ALEX: do we need this?
	RTS
	
@eos:	; refill input string
	JSR getline	
	BRA @retry	; and try again
	RTS

; Put Data Stack Pointer on the stack
h_EXEC:
	.ADDR h_KEY
	CString "EXEC"
do_EXEC:
	LDA 2,X
	STA W
	LDA 3,X
	STA W+1
	INX
	INX
	JMP (W)

h_NUMBER:
	.ADDR h_EXEC
	CString "NUMBER"
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

@next:
	LDA (W),Y
	JSR nibble_asc_to_value
	BCS @err
	
	ORA G2
	STA G2
	
	INY
	CPY G1	; Y = LEN ? --> end
	BEQ @go
; <<4
	ASL G2
	ROL G2+1
	ASL G2
	ROL G2+1
	ASL G2
	ROL G2+1
	ASL G2
	ROL G2+1
	BRA @next
@go:
; leave results G2 on the stack
	LDA G2
	STA 4,X
	LDA G2+1
	STA 5,X
	BRA @drop
@err:	
	LDA #1
	STA ERROR
@drop:	JMP do_DROP


h_INPUT:
	.ADDR h_NUMBER
	CString "INPUT"
do_INPUT:
	JSR getline
	JMP NEXT

; Push a literal Char (1 byte)
h_CLIT:
	.ADDR h_INPUT
	CString "CLIT"
do_CLIT:
; (IP) points to literal char
; instead of next instruction ;)
	LDA (IP)
	STA 0,X
	STZ 1,X
; Now advance IP
; IP+2 --> IP
	INC IP
	BNE @skip
	INC IP+1
@skip:
	JMP DEX2_NEXT

h_LITSTR:
	.ADDR h_CLIT
	CString "LITSTR"
do_LITSTR:
; W points to this word LITSTR in the definition
	; put (W) on the stack and add 2
	LDA IP
	STA 0,X
	LDY #1
	LDA IP+1
	STA 1,X
	; load str LEN into A and add 1 (the length byte)
	LDA (IP)
	INA
; Now advance IP by STR len (which is at IP!)
; IP+2 --> IP
	ADC IP
	STA IP
	BCC @skip
	INC IP+1
@skip:
	JMP DEX2_NEXT

h_SQUOT:
	.ADDR h_LITSTR
	CString "S("
do_SQUOT:
; ( -- ADDR )
;	LDA MODE
;	BEQ .CompilationMode
;.ExecutionMode:
;	JMP do_COLON
;	.ADDR do_SEMI
;.CompilationMode:
	JMP do_COLON
	.ADDR do_LIT, do_LITSTR, do_COMMA ; adds LITSTR to the definition

	; get HERE on the stack for later
	.ADDR do_HERE
	; push a 00 length
	.ADDR do_PUSH0, do_CCOMMA
@next:	; loop over each char in input
	.ADDR do_KEY
	.ADDR do_DUP, do_CLIT
	.BYTE ')'
	.ADDR do_MINUS, do_0BR, @endStr
	.ADDR do_CCOMMA
	.ADDR do_JUMP, @next
@endStr:
	.ADDR do_DROP
	.ADDR do_BREAK
	.ADDR do_HERE, do_OVER, do_MINUS	; compute str length
	.ADDR do_PUSH1, do_MINUS		;
	.ADDR do_SWAP, do_CSTORE
;	.ADDR do_DUP, do_DP, do_STORE	; restore "old" HERE in DP
	.ADDR do_SEMI
	
;-----------------------------------------------------------------
; ALWAYS update the latest word's 
; header address h_*
p_LATEST = h_SQUOT

;-----------------------------------------------------------------
; I/O routines for Kowalkski simulator 
; Change for SBC

IO_AREA = $F000

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

@next:	JSR getc

	CMP #BKSPACE
	BEQ @bkspace

	CPY #MAX_LEN
	BEQ @maxlen

	STA INPUT,y	; save char to INPUT
	INY

	CMP #$0D ; \n
	BEQ @finish

	JSR putc	; echo char

	BRA @next
@maxlen:
	PHA
	LDA #BKSPACE	; send bckspace to erase last char
	JSR putc
	PLA		; restore new char
	STA INPUT-1,y	; save char to INPUT
	JSR putc
	BRA @next
@bkspace:
	CPY #0		; start of line?
	BEQ @next	; do nothing
	JSR putc	; echo char
	DEY		; else: Y--
	BRA @next
@finish:
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
@next:	INY
	LDA (G1),Y

	CMP #$20 ; space
	BEQ @next

	CMP #$0A
	BEQ @next

	CMP #$0D
	BEQ @next

; if we get here we've found a new WORD
	; add Y to G1
	TYA
	CLC
	ADC G1
	STA G1
	BCC @skip1
	INC G1+1
@skip1:
	LDY #0

@next2:
	LDA (G1),Y

	BEQ @eobc	; $00, end of boostrap code

	; save char to INPUT (even if it's a separator)
	STA (W),Y
	INY

	CMP #$20 ; space
	BEQ @endW

	CMP #$0A
	BEQ @endW

	CMP #$0D
	BEQ @endW

	; Add letter to INPUT
	BRA @next2
@eobc:
	STZ BOOT	; clear BootStrap mode
@endW:
	; save word's length
	STY INP_LEN
	STZ INP_IDX

	; Advance W by Y -> BOOTP
	CLC
	TYA
	ADC G1
	STA BOOTP
	BCC @skip2
	INC G1+1
@skip2:
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
	BCC @skip
	ADC #$66
@skip:
	EOR #$30
	RTS

nibble_asc_to_value:
; converts a char representing a hex-digit (nibble)
; into the corresponding hex value

; boundary check is it a digit?
	CMP #'0'
	BMI @err
	CMP #'F'+1
	BPL @err
	CMP #'9'+1
	BMI @conv
	CMP #'A'
	BPL @conv
@err:	; nibble wasn't valid, error
	SEC
	RTS
@conv:	; conversion happens here
	CMP #$41
	BMI @less
	SBC #$37
@less:
	AND #$0F
	CLC
	RTS

; Interrupts routines

IRQ_vec:
NMI_vec:
	RTI

VERS_STR: CString {"ALEX FORTH v0", $0A, $0D}
WHAT_STR: CString {" ?", $0A, $0D}

; Bootstrap code:
; At this point we can extend our forth in forth
; Must end with $00. That will exit BOOTstrap mode and
; enter the interpreter
BOOT_PRG:
	.BYTE " : ? @ . ; "
	.BYTE " : TRUE FFFF ; "
	.BYTE " : FALSE 0 ; "
	.BYTE " : NOT 0= ; "
	.BYTE " : = - 0= ; "
	.BYTE " : 2* DUP + ; "
	.BYTE " : IMMEDIATE LATEST @ SETIMM ; "	; sets the latest word IMMEDIATE
	.BYTE " : ' WORD FIND >CFA ; " ; is this ever used?
	.BYTE " : STOP BREAK ; IMMEDIATE "

	.BYTE " : IF LIT 0BR , HERE LIT 0 , ; IMMEDIATE "
	.BYTE " : THEN HERE SWAP ! ; IMMEDIATE "
	.BYTE " : ELSE LIT JUMP , HERE LIT 0 , SWAP HERE SWAP ! ; IMMEDIATE "

; TEST IF
;	.BYTE " : T IF AAAA ELSE BBBB THEN ; "
;	.BYTE " 1 T . " ; should output AAAA
;	.BYTE " 0 T . " ; should output BBBB

	.BYTE " : BEGIN HERE ; IMMEDIATE "
	.BYTE " : AGAIN LIT JUMP , , ; IMMEDIATE "

; TEST BEGIN AGAIN
;	.BYTE " : TestLoop BEGIN 1 . AGAIN ; TestLoop "

	.BYTE " : UNTIL LIT 0BR  , , ; IMMEDIATE "

	.BYTE " : PAD HERE 64 + ; " ; $64 = d100, PAD is 100 byte above HERE
	.BYTE " : IMM? MODE C@ ; " ; 0: COMPILATION mode, 1 EXEC/IMMEDIATE mode
 
; TEST BEGIN UNTIL
;	.BYTE " : T 5 BEGIN DUP . CRLF 1 - DUP 0= UNTIL ; T "
	
	.BYTE $00


;	*= $0200
.segment  "BSS"

LATEST:	.res 2	; Store the latest ADDR of the Dictionary
MODE:	.res 1	; <>0 Execute, 0 compile
BOOT:	.res 1	; <>0 Boot, 0 not boot anymore
BOOTP:	.res 2	; pointer to BOOTstrap code
ERROR:	.res 1	; Error when converting number
INP_LEN: .res 1	; Length of the text in the input buffer
INPUT:	.res 80	; CMD string (extend as needed, up to 256!)
INP_IDX: .res 1	; Index into the INPUT Buffer (for reading it with KEY)
DP:		.res 2	; Data Pointer: Store the latest ADDR of next free space in RAM (HERE)


; Base of user memory area.
USER_BASE:

; system vectors

;    *=  $FFFA
.segment  "VECTORS"	

    .addr   NMI_vec     ; NMI vector
    .addr   RES_vec     ; RESET vector
    .addr   IRQ_vec     ; IRQ vector
