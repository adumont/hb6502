; FORTH 
; Alex Dumont

.pc02 ; 65C02 mode
.debuginfo      +       ; Generate debug info
.feature string_escapes

__word_last .set 0
__word_0 = 0

; Macro to encode FORTH counted strings
; Answered at https://retrocomputing.stackexchange.com/a/20379/19750
.macro CString str, flags
	.ifnblank flags
		.byte :++  -  :+  +flags
	.else
		; we diff the two unamed labels :++ and :+ --> the length
		.byte :++  -  :+
	.endif
:	.byte str
:
.endmacro

; macro for new word headers
; Answered at https://retrocomputing.stackexchange.com/a/20161/19750
.macro defword label, strname, flags
	.ident (.sprintf("h_%s", label)):
	.ident (.sprintf("__word_%u", __word_last + 1)):

	.addr .ident(.sprintf("__word_%u", __word_last))
	; this ifblank cascading can probably be enhanced...
	.ifnblank strname
		.ifnblank flags
			CString strname, flags
		.else
			CString strname
		.endif
	.else
		.ifnblank flags
			CString label, flags
		.else
			CString label
		.endif
	.endif

	__word_last .set __word_last + 1

	.ident (.sprintf("do_%s", label)):
.endmacro

; Macro for defining words without headers (like :NONAMEs) so they won't show in dictionay
; it just creates a label so we can call them from other words
.macro noheader label
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
MAX_LEN = $80		; Input Buffer MAX length, $80= 128 bytes
BP   = $4000 - 2	; top of LOCALS stack (grows down). Right below the HW addr block

IMMEDIATE_FLAG = $80
HIDDEN_FLAG = $40

; Offset of the WORD name in the label
; 2 bytes after the Header's addr
HDR_OFFSET_STR = 2	

.ifdef ACIA
	.include "lib/acia.s"
.endif

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

.ifdef ACIA
	JSR acia_init
.endif

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

	; clear the OK flag
	stz OK
	
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
	
loop1:

	; if OK flag is 0, don't show OK prompt
	.ADDR do_FETCH_OK
	.ADDR do_0BR, @skipOK

	; else show OK prompt
	.ADDR do_PRMP

	; and clear OK flag
	.ADDR do_CLEAR_OK

@skipOK:

	.ADDR do_WORD	; ( addr len )
	.ADDR do_OVER, do_OVER	; 2DUP ( addr len addr len )

	.ADDR do_OR, do_EQZ, do_0BR, @cont ; If WORD didn't returned 2 "0", we continue

; else we loop for a new word
	.ADDR do_DROP, do_DROP
	.ADDR do_JUMP, loop1

@cont:
	.ADDR do_OVER, do_OVER	; 2DUP ( addr len addr len )

;	.ADDR do_OVER, do_OVER, do_TYPE	; DEBUG: Show TOKEN

	.ADDR do_FIND ; ( addr len hdr )
	.ADDR do_DUP  ; ( addr len hdr hdr )
	.ADDR do_0BR, numscan ; Not a word? Goto numscan

; Found a word! ( addr len hdr )
	.ADDR do_NROT, do_DROP, do_DROP ; ( hdr ) header of the word

	.ADDR do_STATE   ; ( hdr MODE ) 0: compile, 1 execute

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
	; and clear OK flag
	.ADDR do_CLEAR_OK

	; if we were in compilation mode, we have to cancel the last word
	; that was started (but is unfinished). we have to restore LATEST to its
	; previous value (which is in LATEST @ )
	.ADDR do_STATE   ; ( n MODE ) 0: compile, 1 execute
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

	.ADDR do_STATE   ; ( n MODE ) 0: compile, 1 execute

	.ADDR do_0BR, commitN ; Mode = 0 --> CommitN to new word
	; if not 0, continue loop (N already on the stack)
	
	.ADDR do_JUMP, loop1

commitN:
	; ( n )
	; Add number to the word we are defining
	.ADDR do_COMPILE, do_LIT	; first add "LIT" ( n )
	.ADDR do_COMMA			; add n to word (  )

	.ADDR do_JUMP, loop1

;------------------------------------------------------

noheader "CLEAR_OK"
	; clears the OK flag
	stz OK
	JMP NEXT

noheader "FETCH_OK"
	; pushes OK flag on the stack
	lda OK
	sta 0,x
	stz 1,x
	JMP DEX2_NEXT

defword "COLON",,
; push IP to Return Stack
	LDA IP+1	; HI
	PHA
	LDA IP		; LO
	PHA

; W+3 --> IP 
; (Code at W was a JMP, so 3 bytes)
	CLC
	LDA W
	ADC #3
	STA IP
	LDA W+1
	ADC #0
	STA IP+1
	JMP NEXT

; SEMICOLON aka EXIT
defword "SEMI","EXIT",
; POP IP from Return Stack
	PLA
	STA IP
	PLA
	STA IP+1
; JMP NEXT
	JMP NEXT

defword "CLS",,
; clear stack
	LDX #DTOP
	JMP NEXT

defword "FALSE",,
	lda #$00
	bra store_on_ToS

defword "TRUE",,
	lda #$FF
store_on_ToS:
	sta 0,x
	sta 1,x
	jmp DEX2_NEXT

; RSIN ( -- )
; Reset Input buffer
; called on start-up, and each parse error
noheader "RSIN"
	STZ INP_LEN
	STZ INP_IDX
	JMP NEXT

defword "ROT",,
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

defword "NROT", "-ROT",
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

defword "OVER",,
; ( x y -- x y x )
	LDA 4,X
	STA 0,X
	LDA 5,X
	STA 1,X
	JMP DEX2_NEXT

defword "DROP",,
	INX
	INX
	JMP NEXT

; Convenience BREAK word we can add in the code
; to force a BREAK
defword "BREAK",,
	JMP NEXT	; set Breakpoint here!

defword "PUSH0","0",
	STZ 0,x
	STZ 1,x
	JMP DEX2_NEXT

defword "PUSH1","1",
	LDA #1
	STA 0,x
	STZ 1,x
	JMP DEX2_NEXT

defword "TWICE","2*",
; ( n -- 2*n )
; n is one signed or unsigned cell
; if n is signed, sign is kept
	ASL 2,X
	ROL 3,X
	JMP NEXT

defword "DTWICE","D2*",
; ( d -- 2*d )
; d is a signed or unsigned double (2 cells)
; if d is signed, sign is kept
	ASL 4,X
	ROL 5,X
	ROL 2,X
	ROL 3,X
	JMP NEXT

defword "UHALF","U2/",
; ( u -- u/2 )
; u is an unsigned cell
	LSR 3,X		; Logical Shift Right, The bit that was in bit 0 is shifted into the carry flag. Bit 7 is set to zero
	ROR 2,X
	JMP NEXT

defword "UDHALF","UD2/",
; ( ud -- ud/2 )
; u is an unsigned double
	LSR 3,X		; Logical Shift Right, The bit that was in bit 0 is shifted into the carry flag. Bit 7 is set to zero
	ROR 2,X
	ROR 5,X
	ROR 4,X
	JMP NEXT

defword "HALF","2/",
; ( n -- n/2 )
; n is an SIGNED cell
	SEC			; by default we set the carry so it will ROR into the MSB (assume it's negative number)
	LDA 3,X
	BMI @skip	; will branch if indeed negative
	CLC			; here we clear the Carry (so that it won't ROR into the MSB)
@skip:
	ROR 3,X
	ROR 2,X
	JMP NEXT

defword "DHALF","D2/",
; ( d -- d/2 )
; u is a SIGNED double
	SEC			; by default we set the carry so it will ROR into the MSB (assume it's negative number)
	LDA 3,X
	BMI @skip	; will branch if indeed negative
	CLC			; here we clear the Carry (so that it won't ROR into the MSB)
@skip:
	ROR 3,X
	ROR 2,X
	ROR 5,X
	ROR 4,X
	JMP NEXT

defword "COMPILE",
; like doing LIT, addr, COMMA
; we call COMPILE, addr
	JMP do_COLON
compile_addr:	; label so we we can jump here from the alias "LIT,"
	.ADDR do_FROM_R, do_DUP, do_FETCH, do_COMMA, do_2PLUS, do_TO_R ; COMPILE R> DUP @ , CELL+ >R
	.ADDR do_SEMI

defword "LIT",,
; Push a literal word (2 bytes)
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
	CLC
	LDA IP
	ADC #2
	STA IP
	BCC @skip
	INC IP+1
@skip:
	JMP DEX2_NEXT

defword "DUP",,
	LDA 2,X
	STA 0,X
	LDA 3,X
	STA 1,X
	JMP DEX2_NEXT
	
defword "PLUS","+",
	CLC
	LDA 2,X
	ADC 4,X
	STA 4,X
	LDA 3,X
	ADC 5,X
	STA 5,X
	JMP do_DROP

defword "DPLUS","D+",
; Double cells sum
; ( d1 d2 -- sum ) where really it's ( lo1 hi1 lo2 hi2 -- losum hisum )
; Stack   HI    LO
; cells   byte  byte
; LO1     9,X   8,X
; HI1     7,X   6,X
; LO2     5,X   4,X
; HI2     3,X	2,X
	CLC
	LDA 4,X
	ADC 8,X
	STA 8,X
	LDA 5,X
	ADC 9,X
	STA 9,X
	LDA 2,X
	ADC 6,X
	STA 6,X
	LDA 3,X
	ADC 7,X
	STA 7,X
	INX
	INX
	JMP do_DROP

defword "DMINUS","D-",
; Double cells diff
; ( d1 d2 -- diff ) where really it's ( lo1 hi1 lo2 hi2 -- lodiff hidiff )
; Stack   HI    LO
; cells   byte  byte
; LO1     9,X   8,X
; HI1     7,X   6,X
; LO2     5,X   4,X
; HI2     3,X	2,X
	SEC
	LDA 8,X
	SBC 4,X
	STA 8,X
	LDA 9,X
	SBC 5,X
	STA 9,X
	LDA 6,X
	SBC 2,X
	STA 6,X
	LDA 7,X
	SBC 3,X
	STA 7,X
	INX
	INX
	JMP do_DROP

defword "2PLUS","2+",
	CLC
	LDA 2,x
	ADC #2
	STA 2,x
	BCC @skip
	INC 3,X
@skip:	JMP NEXT

defword "EMIT",,
; EMIT emit a single char
	; char is on stack
	LDA 2,X
	JSR putc
	JMP do_DROP

defword "GETC",,
; get a single char from IO, leave on stack
	JSR getc ; leaves the char in A
	STA 0,X
	STZ 1,X
	JMP DEX2_NEXT

defword "TO_R",">R",
; >R: pop a cell (possibly an ADDR) from
; the stack and pushes it to the Return Stack
	LDA 3,X
	PHA
	LDA 2,X
	PHA
	JMP do_DROP

defword "FROM_R","R>",
; R>: pop a cell from the Return Stack
; and pushes it to the Stack
	PLA
	STA 0,X
	PLA
	STA 1,X
	JMP DEX2_NEXT

defword "I",,
; I is same code as R@
	BRA do_R_AT

defword "R_AT","R@",
; R@ : copy the cell from the Return Stack
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

defword "CFETCH","C@",
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

defword "DP",,
; DP ( -- addr )

; Alternative, as word definition:
;	JMP do_COLON
;	.ADDR do_LIT, CP, do_SEMI
	LDA #<DP
	STA 0,X
	LDA #>DP
	STA 1,X
	JMP DEX2_NEXT

; COUNT: ( addr -- addr+1 len )
; Converts a counted string, whose length is contained in
; the 1rst byte, into the form appropriate for TYPE, by
; leaving the address of the first character and the
; length on the stack.

defword "COUNT",,
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


defword "TYPE",,
; Print a STRING	( addr len -- )
; addr --> pointer to 1rst char of string
; len  --> length of string (1 byte)
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

defword "SPACE",,
; Print a space		( -- )
	LDA #' '
	JSR putc
	JMP NEXT

defword "CR",,
; Print a new line	( -- )
	JSR _crlf
	JMP NEXT

_crlf:
	LDA #$0a ; CR
	JSR putc
	LDA #$0d ; LF
	JSR putc
	RTS

defword "XOR",,
; ( a b -- a^b ) bitwise XOR
	LDA 2,X
	EOR 4,X
	STA 4,X
	LDA 3,X
	EOR 5,X
	STA 5,X
	JMP do_DROP

defword "OR",,
; ( a b -- a|b ) bitwise OR
	LDA 2,X
	ORA 4,X
	STA 4,X
	LDA 3,X
	ORA 5,X
	STA 5,X
	JMP do_DROP

defword "REVEAL",,
; ( -- ) Reveals (unhide) latest word. Used in ; and ;CODE.
	JMP do_COLON
	.ADDR do_LATEST, do_FETCH
	.ADDR do_STAR_REVEAL
	.ADDR do_DROP
	.ADDR do_SEMI

noheader "STAR_REVEAL"
	JSR _getWordLen			; sets Y=2 and A=LEN field, (W),Y point to LEN field
	AND #($FF-HIDDEN_FLAG)	; removes the HIDDEN_FLAG
	STA (W),Y				; store back in LEN field
	JMP NEXT

defword "GETIMM",,
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

defword "DP_STORE","DP!",
	; put DP in G1 in ZP
	LDA 2,X
	STA DP
	LDA 3,X
	STA DP+1
	; 
	JMP do_DROP

defword "CCOMMA","C,",
; ( C -- ) save a byte C to HERE and advance
; HERE by 1
; Primitive version!
	; put DP in G1 in ZP
	LDA DP
	STA G1
	LDA DP+1
	STA G1+1
	; save TOS in (DP), only LO byte
	LDA 2,X
	STA (G1)
	; Drop TOS
	INX
	INX
	; Advance HERE by 1 byte
	CLC
	LDA DP
	ADC #1	; TODO: I tried INC DP but it wasn't working... Why?
	STA DP
	BCC @skip
	INC DP+1
@skip:
	JMP NEXT

defword "UM_STAR","UM*",
; UM*     ( un1 un2 -- ud )
;   UM* multiplies the unsigned 16-bit integer un1 by the
;   unsigned 16-bit integer un2 and returns the unsigned
;   32-bit product ud.
; calls the do_STAR_MUL word in assembler
	JMP do_COLON
	.ADDR do_STAR_UM_STAR
	.ADDR do_ROT, do_DROP
	.ADDR do_SEMI

noheader "STAR_UM_STAR"
; ( n1 n2 0 -- n1 Dproduct )
	; we copy N2 to G1, clear G2
	; we will use G2G1 as HILO tmp register to shift-left n2
	; and we will add it to the partial product
	LDA 2,X
	STA G1
	LDA 3,X
	STA G1+1
	STZ G2
	STZ G2+1
	; clear the place for the partial product (4 bytes = 32 bits):
	STZ 2,X		
	STZ 3,X
	STZ 0,X
	STZ 1,X
	
	LDY #$10	; counter 16 bits
	
	; Shift N1 to the right.
@shift_right_n1:
	LSR 5,X
	ROR 4,X		; rightmost bit falls into carry
	
	BCC @shift_left_n2	; c=0, go to shift-left N2
; c=1 --> Add N2 to the partial product
	CLC
	LDA G1
	ADC 2,X
	STA 2,X
	LDA G1+1	
	ADC 3,X
	STA 3,X
	LDA G2
	ADC 0,X
	STA 0,X
	LDA G2+1	
	ADC 1,X
	STA 1,X
	
@shift_left_n2:
	ASL G1
	ROL G1+1
	ROL G2
	ROL G2+1
	
	DEY
	BNE @shift_right_n1

	JMP DEX2_NEXT	

defword "UM_DIV_MOD","UM/MOD",
; Takes a Double (32bit/2 cells) dividend and
; a 1 cell (16bit) divisor
; and returns remainder and quotient (1 cell each)
; ( UDdividend Udivisor -- Remainder Quotient )
; we just call the assembler routine STAR_UM_DIV_MOD and
; swap the results on the stack
	JMP do_COLON
	.ADDR do_STAR_UM_DIV_MOD, do_SWAP
	.ADDR do_SEMI

noheader "STAR_UM_DIV_MOD"
; Takes a Double (32bit/2 cells) dividend and
; a 1 cell (16bit) divisor
; and returns quotient and remainder (1 cell each)
; ( UDdividend Udivisor -- Quotient Remainder )
; From: How to divide a 32-bit dividend by a 16-bit divisor.
; By Garth Wilson (wilsonmines@dslextreme.com), 9 Sep 2002.    
; SRC: http://www.6502.org/source/integers/ummodfix/ummodfix.htm
        SEC
        LDA     4,X     ; Subtract hi cell of dividend by
        SBC     2,X     ; divisor to see if there's an overflow condition.
        LDA     5,X
        SBC     3,X
        BCS     @oflo   ; Branch if /0 or overflow.

        LDA     #$11    ; Loop 17 times
        STA     0,X     ; Use 0,X as loop counter
@loop:  ROL     6,X     ; Rotate dividend lo cell left one bit.
        ROL     7,X
        DEC     0,X     ; Decrement loop counter.
        BEQ     @fin    ; If we're done, then branch to .fin
        ROL     4,X     ; Otherwise rotate dividend hi cell left one bit.
        ROL     5,X
        STZ     G1
        ROL     G1      ; Rotate the bit carried out of above into G1.

        SEC
        LDA     4,X     ; Subtract dividend hi cell minus divisor.
        SBC     2,X
        STA     G1+1    ; Put result temporarily in G1+1 (lo byte)
        LDA     5,X
        SBC     3,X
        TAY             ; and Y (hi byte).
        LDA     G1      ; Remember now to bring in the bit carried out above.
        SBC     #0
        BCC     @loop

        LDA     G1+1    ; If that didn't cause a borrow,
        STA     4,X     ; make the result from above to
        STY     5,X     ; be the new dividend hi cell
        BRA     @loop   ; and then brach up

@oflo:  LDA     #$FF    ; If overflow or /0 condition found,
        STA     4,X     ; just put FFFF in both the remainder
        STA     5,X
        STA     6,X     ; and the quotient.
        STA     7,X

@fin:   JMP     do_DROP  ; When you're done, show one less cell on data stack

defword "LBRAC","[",IMMEDIATE_FLAG
; ( -- ) switch to EXECUTE/IMMEDIATE mode
	LDA #1
	STA MODE
	JMP NEXT

defword "RBRAC","]",
; ( -- ) switch to COMPILE mode
	STZ MODE
	JMP NEXT

noheader "STAR_HEADER"
	JMP do_COLON
	.ADDR do_HERE		; keep current HERE on stack
	.ADDR do_LATEST, do_FETCH, do_COMMA ; store value of LATEST in the Link of new Header
	.ADDR do_LATEST, do_STORE ; store "old HERE" in LATEST

	.ADDR do_DUP
	.ADDR do_CLIT     ;\
	.BYTE HIDDEN_FLAG ; | Sets the Hidden Flag (we OR with HIDDEN_FLAG)
	.ADDR do_OR       ;/
	.ADDR do_CCOMMA	; store LEN in Header
	.ADDR do_DUP, do_NROT	; ( len addr len )
	.ADDR do_HERE, do_SWAP, do_CMOVE ; store name
	.ADDR do_ALLOT		; advance HERE by LEN

	.ADDR do_SEMI

defword "PRMP",,
	; Print the OK Prompt
	JMP do_COLON
	.ADDR do_STATE, do_0BR, @skip
	.ADDR do_LIT, OK_STR, do_COUNT, do_TYPE
@skip:
	.ADDR do_SEMI

do_STAR_COMMIT_JMP:
; store a 4C (JMP) into the word
	JMP do_COLON
	.ADDR do_CLIT	;
	.BYTE $4C		; store a 4C (JMP)
	.ADDR do_CCOMMA	;
	.ADDR do_SEMI


defword "MARKER",,
; When called, MARKER creates a new word on the dictionary called FORGET
; in its definition, it encodes the FORTH code to restore LATEST and HERE
; at the values they had when running MARKER.
; we encode the values of LATEST and HERE using LIT in the FORGET definition
	JMP do_COLON
	.ADDR do_HERE		; keep current HERE on stack
	.ADDR do_LATEST, do_FETCH ;

	.ADDR do_LITSTR
	CString "FORGET"	; commits counted string
	.ADDR do_COUNT

	.ADDR do_STAR_HEADER, do_STAR_COMMIT_JMP

	.ADDR do_COMPILE, do_COLON	; do_COLON

	.ADDR do_COMPILE, do_LIT	; LIT
	.ADDR do_COMMA	; stores old LATEST

	.ADDR do_COMPILE, do_LATEST	; LATEST
	.ADDR do_COMPILE, do_STORE	; !

	.ADDR do_COMPILE, do_LIT	; LIT
	.ADDR do_COMMA	; stores old HERE

	.ADDR do_COMPILE, do_DP	; DP
	.ADDR do_COMPILE, do_STORE	; !
	
	.ADDR do_COMPILE, do_SEMI	; ;

	.ADDR do_REVEAL

	.ADDR do_SEMI

defword "CODE",,
; get next TOKEN in INPUT (with WORD) and creates a Header
; but doesn't fill the Code Field (can be used for primitive word, or secondary if we will it with do_COLON (see FCOLON))
	JMP do_COLON
	.ADDR do_WORD		; get next TOKEN in INPUT (new word's name)
	.ADDR do_STAR_HEADER
	.ADDR do_SEMI

defword "END_CODE",";CODE",
; commit a JMP NEXT to close the primitive word
	JMP do_COLON
	.ADDR do_REVEAL
	.ADDR do_STAR_COMMIT_JMP
	.ADDR do_COMPILE, NEXT
	.ADDR do_SEMI

defword "CREATE",,
	JMP do_COLON
	.ADDR do_CODE				; creates an empty header
	.ADDR do_STAR_COMMIT_JMP 	; adds JMP
	.ADDR do_COMPILE, do_COLON	; compiles do_COLON
	.ADDR do_COMPILE, do_LIT	; compiles do_LIT
	.ADDR do_HERE ; leave HERE on the stack, we'll need it later to fill this slot cell ;)
	.ADDR do_HEREPP	; Advance HERE by 1 cell (+2), we effectively leave the first empty cell (*)
	; now we'll fill two cells: one for SEMI, another free for now.
	; when we call DOES> it will patch them and overwrite them with [do_JUMP][someAddr]
	.ADDR do_COMPILE, do_SEMI
	.ADDR do_HEREPP	; Advance HERE by 1 cell (+2), we effectively leave an empty cell (again)
	; now we patch the empty cell (*) we left after the LIT
	.ADDR do_HERE, do_SWAP, do_STORE ; we store the addr in the first empty cell (*)
	.ADDR do_REVEAL ; we reveal the word
	.ADDR do_SEMI

defword "DOES","DOES>",
; the last word supposedly created by CREATE is made of a byte and 4 cells:
; [4C][do_COLON][LIT][addr][do_SEMI][xxxx]
; DOES> will replace the last 2 cells with [do_JUMP][Addr], leaving the last word like this:
; [4C][do_COLON][LIT][addr][do_JUMP][Addr]
	JMP do_COLON
	.ADDR do_LATEST, do_FETCH, do_CFA		; get CFA of latest word (created with CREATE)
	.ADDR do_CLIT
	.BYTE 7
	.ADDR do_PLUS				; advance 7 bytes (1+3 cells, so we point to [do_SEMI] cell)
	.ADDR do_LIT, do_JUMP
	.ADDR do_OVER, do_STORE		; overwrite the []do_SEMI cell with a [JUMP]
	.ADDR do_2PLUS				; advance 1 more cell
	.ADDR do_FROM_R				; pull next word in compiled definition (right after DOES>)
	.ADDR do_SWAP, do_STORE		; store it in last cell!
	.ADDR do_SEMI

defword "NONAME",":NONAME",	; Forth Anonymous word
; Create a new word, without header (not in dictionary)
; Leaves the CFA (XT) on the stack. Can be executed with EXEC
	JMP do_COLON
	.ADDR do_HERE	; leave CFA on the stack
	.ADDR do_JUMP, prep_cfa

defword "FCOLON",":",	; Forth Colon ":"
; get next TOKEN in INPUT and creates 
; a Header for a new word
	JMP do_COLON
	.ADDR do_CODE		; creates empty header
prep_cfa:	; jump from noname
	.ADDR do_STAR_COMMIT_JMP	; adds JMP
	.ADDR do_COMPILE, do_COLON ; store do_COLON addr
	.ADDR do_RBRAC ; Enter Compilation mode
	.ADDR do_SEMI

defword "VARIABLE",,
; get next TOKEN in INPUT and creates
; a Header for a new word
	JMP do_COLON
	.ADDR do_FCOLON	; creates a Forth colon word's header
	.ADDR do_COMPILE, do_LIT
	.ADDR do_HERE		; put HERE on the stack
	.ADDR do_HEREPP	; Advance HERE by 1 cell (+2), we effectively leave an empty cell
	.ADDR do_COMPILE, do_SEMI	; word is complete
	.ADDR do_HERE, do_SWAP, do_STORE	; store the address right after the word into the address slot of the word
	.ADDR do_PUSH1, do_1PLUS, do_ALLOT
	.ADDR do_LBRAC ; Exits Compilation mode
	.ADDR do_REVEAL
	.ADDR do_SEMI

defword "SEMICOLON",";",IMMEDIATE_FLAG
; Add's do_SEMI to header of word being defined
; and exits COMPILATION mode (1->MODE)
	JMP do_COLON
	.ADDR do_REVEAL
	.ADDR do_COMPILE, do_SEMI	; commits do_SEMI addr
	
	;.ADDR do_PUSH1, do_LIT, MODE, do_CSTORE ; Exits Compilation mode
	.ADDR do_LBRAC ; Exits Compilation mode
	
	.ADDR do_SEMI

defword "MODE",,
; ( -- MODE ) MODE is the addr, not the value!
	LDA #<MODE
	STA 0,X
	LDA #>MODE
	STA 1,X
	JMP DEX2_NEXT

defword "ALLOT",,
; : ALLOT	HERE + DP ! ;
	JMP do_COLON
	.ADDR do_HERE, do_PLUS, do_DP_STORE
	.ADDR do_SEMI

defword "SP",,
; Put Data Stack Pointer on the stack
	TXA
	STA 0,X
	STZ 1,X
	JMP DEX2_NEXT

defword "CMOVE",,
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

defword "KEY",,
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

defword "EXEC",,
; ( ADDR -- )
; JUMP to addr on the stack
	LDA 2,X
	STA W
	LDA 3,X
	STA W+1
	INX
	INX
	JMP (W)

defword "NUMBER",,
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

defword "INPUT",,
	JSR getline
	JMP NEXT

defword "CLIT",,
; Push a literal Char (1 byte)
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

defword "LITSTR",,
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
	CLC
	ADC IP
	STA IP
	BCC @skip
	INC IP+1
@skip:
	JMP DEX2_NEXT

defword "STAR_DO","*DO",
; ( end start -- )
; Used by DO
; We don't use >R as it would mean being a colon word, and that would
; mess with the return stack (colon world pushes next IP onto the rs)

	; pushes 'end' on the Return Stack
	LDA 5,X
	PHA
	LDA 4,X
	PHA
	; pushes 'start' on the Return Stack
	LDA 3,X
	PHA
	LDA 2,X
	PHA
	; drop start
	INX
	INX
	; drop end
	JMP do_DROP

defword "STAR_LOOP","*LOOP",
; ( INC -- )
; Used by LOOP, +LOOP, takes the increment on the stack

; *LOOP should be followed by JUMP, ADDR
; where ADDR is the instruction after DO
; *LOOP will with run it or bypass it

	JMP do_COLON
	.ADDR do_FROM_R	; get ADDR (NextIP). Right after LOOP is the JUMP back to DO, that we can bypass with *LOOP
	.ADDR do_FROM_R	;           ( INC ADDR I )
	.ADDR do_ROT	;           ( ADDR I INC )
	.ADDR do_PLUS	; I=I+INC   ( ADDR I )
	.ADDR do_FROM_R	; End
	.ADDR do_OVER, do_OVER	; 2DUP	( ADDR I END I END )
	.ADDR do_MINUS
	.ADDR do_LIT, $1000
	.ADDR do_AND	; 0 iif END>=I, $1000 iif I>END
	.ADDR do_EQZ	; invert
	.ADDR do_0BR, @loop
; exit loop:
	; ( ADDR I END )
	.ADDR do_DROP, do_DROP ; ( )
	.ADDR do_CLIT
	.BYTE $04
	.ADDR do_PLUS ; Add 4 to Next IP ( bypass jump do -> Exit DO-LOOP)
	.ADDR do_TO_R ; push NextIP back to R
	.ADDR do_SEMI ; jump over

@loop:	; ( ADDR I END )
	.ADDR do_TO_R	; push END back to R
	.ADDR do_TO_R	; push I back to R
	.ADDR do_TO_R	; push NextIP back to R
	.ADDR do_SEMI

defword "LEAVE",,
; ( -- )
; Leaves out of a DO-LOOP on the next *LOOP
; it works by setting I to END, so on  the next check in *LOOP
; it will exit ;)
	JMP do_COLON
	.ADDR do_FROM_R	; ( ADDR ) Next IP
	.ADDR do_FROM_R	; ( ADDR I )
	.ADDR do_DROP	; ( ADDR )			; we drop I
	.ADDR do_FROM_R	; ( ADDR END )
	.ADDR do_DUP	; ( ADDR END END )	; we used END as I
	.ADDR do_TO_R	; push END back to R
	.ADDR do_TO_R	; push I (=END) back to R
	.ADDR do_TO_R	; push Next IP back to R
	.ADDR do_SEMI

defword "STATE","STATE",IMMEDIATE_FLAG
; Is it immediate/execution mode? 
; returns the value of variable MODE
; 0 : Compilation mode, <>0 : Execution mode
	LDA #<MODE
	STA G1
	LDA #>MODE
	STA G1+1
	LDA (G1)
	STA 0,X
	STZ 1,X
	JMP DEX2_NEXT


defword "SQUOT","S(",IMMEDIATE_FLAG
; ( -- ADDR )
; This S( word is to enter a string. It will return the address of a
; counted string, suitable to follow with COUNT TYPE
; TODO: make more FORTH compliant, it should not require COUNT , only TYPE
; It's an immediate word.
; At the beginning we chek if we are in Immediate/Execute mode, or Compilation mode
; and we do different things in each case. Then there is the same loop (@commitStr)
; At the end, again we do different things depending on the MODE.

; In Execution mode, S( will store the string at HERE+$FF
; which eventually will be overwritten. Hence it's temporary.
; In Compilation mode, S( will store the String as LITSTR in the defined word.

	JMP do_COLON
	.ADDR do_STATE         ; MODE: 0 Compile, >0 Execute
	.ADDR do_0BR, @CModeStart
@XModeStart:
	; Save HERE on the stack.
	; In the execution mode, will save the STR at HERE + FF
	.ADDR do_HERE		; ( oldHERE )
	.ADDR do_DUP		; ( oldHERE oldHERE )
	.ADDR do_CLIT
	.BYTE $FF
	.ADDR do_PLUS, do_DUP	; ( oldHERE oldHERE+FF oldHERE+FF )
	.ADDR do_DP_STORE	; we update HERE
	.ADDR do_JUMP, @commitStr
	; here we have (oldHERE newHERE) newHERE=oldHERE+FF)
	; we'll restore oldHERE at the end. we store the STR at newHERE:
@CModeStart:
	.ADDR do_COMPILE, do_LITSTR ; adds LITSTR to the definition
	; get HERE on the stack for later
	.ADDR do_HERE
@commitStr:
	; push a 00 length
	.ADDR do_PUSH0, do_CCOMMA		; **
@next:	; loop over each char in input
	.ADDR do_KEY
	.ADDR do_DUP, do_CLIT
	.BYTE ')'
	.ADDR do_MINUS, do_0BR, @endStr
	.ADDR do_CCOMMA
	.ADDR do_JUMP, @next
@endStr:
	.ADDR do_DROP
	.ADDR do_HERE, do_OVER, do_MINUS	; compute str length
	.ADDR do_PUSH1, do_MINUS
; End of @commitStr loop
	.ADDR do_STATE         ; MODE: 0 Compile, >0 Execute
	.ADDR do_0BR, @CmodeEnd
@XmodeEnd:
	.ADDR do_OVER, do_CSTORE		; update len in length byte
	; (oldHERE newHERE )
	; restore old HERE and leave newHERE as str addr!
	.ADDR do_SWAP, do_DP_STORE
	.ADDR do_COUNT
	.ADDR do_SEMI

@CmodeEnd:
	.ADDR do_SWAP, do_CSTORE
	.ADDR do_COMPILE, do_COUNT ; add COUNT to the definition
	.ADDR do_SEMI
	
defword "DPRINT","D.",
; Print a double cell number (in hex for now)
; ( lo hi -- )
	LDA 3,X
	JSR print_byte
	LDA 2,X
	JSR print_byte
	INX
	INX
	BRA do_PRINT	; fallback to "."

defword "CPRINT","C.",
; Print data on top of stack (in hex for now)
; ( n -- )
	BRA cprint

defword "PRINT",".",
; Print data on top of stack (in hex for now)
; ( n -- )
	LDA 3,X
	JSR print_byte
cprint:
	LDA 2,X
	JSR print_byte
	LDA #' '
	JSR putc
	JMP do_DROP

defword "MINUS","-",
	SEC
	LDA 4,X
	SBC 2,X
	STA 4,X
	LDA 5,X
	SBC 3,X
	STA 5,X
	JMP do_DROP

defword "EQZ","0=",
; 0=, it's also equivalent to "logical NOT" (not a bitwise NOT)
; logical NOT --> use 0=
; 0<> --> use 0= 0=  (twice!)

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

defword "NOT",,
; ( a -- not(a) ) bitwise NOT
	LDA 2,X
	EOR #$FF
	STA 2,X
	LDA 3,X
	EOR #$FF
	STA 3,X
	JMP NEXT

defword "1PLUS","1+",
	INC 2,X
	BNE @skip
	INC 3,X
@skip:	JMP NEXT

defword "AND",,
; ( a b -- a&b ) bitwise AND
	LDA 2,X
	AND 4,X
	STA 4,X
	LDA 3,X
	AND 5,X
	STA 5,X
	JMP do_DROP

defword "LATEST",,
; ( -- LATEST )
	LDA #<LATEST
	STA 0,X
	LDA #>LATEST
	STA 1,X
	JMP DEX2_NEXT

defword "FETCH","@",
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

defword "SETIMM",,
; ( hdr -- )
; takes a header to a word in dictionary
; and sets its Immediate flag
	JSR _getWordLen

	ORA #IMMEDIATE_FLAG	; MSB set
	STA (W),Y	; LEN
		
	JMP do_DROP

defword "WORD",,
; Find next word in input buffer (and advance INP_IDX)
; ( -- ADDR LEN )
	lda #' '
	bra _parse

defword "PARSE",,
; parse input buffer with separator SEPR (and advance INP_IDX)
; ( SEP -- ADDR LEN )

	lda 2,x
	inx
	inx
_parse:
	sta SEPR

@next1:
	JSR _KEY
	
	CMP SEPR
	BEQ @next1

	CMP #$0A
	BEQ @return0		;--> tenemos que salir de WORD, dejando 2 0 ( 0 0 ) en el stack

	CMP #$0D
	BEQ @return0

	CMP #'\'
	BNE @startW

	; here it's a \ comment
@comment:
	JSR _KEY

	CMP #$0A
	BEQ @return0		;--> tenemos que salir de WORD, dejando 2 0 ( 0 0 ) en el stack

	CMP #$0D
	BNE @comment
	; fallthrough to @return0

@return0:
	lda BOOT
	bne :+		; if boot<>0 (aka boot mode, we don't set the prompt to 1)
	lda #1		; we mark 1 the OK flag
	sta OK
:
	stz 0,X		; we push a 0 on the stack
	stz 1,X
	DEX
	DEX
	stz 0,X		; we push another 0 on the stack
	stz 1,X
	JMP DEX2_NEXT	; exit "WORD"

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

	CMP SEPR
	BEQ @endW

	CMP #$0A
	BEQ @return

	CMP #$0D
	BEQ @return

	BRA @next2
@return:
	lda BOOT
	bne @endW	; if boot<>0 (aka boot mode, we don't set the prompt to 1)
	lda #1		; we mark 1 the OK flag
	sta OK
@endW:
	; compute length
	LDA INP_IDX
	SEC
	SBC G1	; earlier we saved INP_IDX in G1 ;)
	STA 0,X
	STZ 1,X
	JMP DEX2_NEXT

defword "FIND",,
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

; shortcuts in FIND for ":" and ";"
	LDA 2,X
	CMP #1
	BNE @nxt_word
; 1 char word. here we test if the word is ":"
	LDA (G2)
	CMP #':'
	BNE @not_colon
	; word is ":"!!
	LDA #<h_FCOLON
	STA 4,X
	LDA #>h_FCOLON
	STA 5,X
	JMP do_DROP
@not_colon:
	CMP #';'
	BNE @not_semi
	; word is ";"!!
	LDA #<h_SEMICOLON
	STA 4,X
	LDA #>h_SEMICOLON
	STA 5,X
	JMP do_DROP
@not_semi:
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
	TAY			; save A in Y
	AND #HIDDEN_FLAG
	BNE @advance_w	; Hidden word! skip it

	TYA			; restore A (current dictionay word's length)
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

defword "CFA",">CFA",
	JMP do_COLON
	; ( ADDR -- ADDR )
	; takes the dictionary pointer to a word
	; returns the codeword pointer
	.ADDR do_2PLUS  ; 2+ ; skip prev. word link
	.ADDR do_DUP, do_CFETCH 	; ( LEN ) with FLAG

	.ADDR do_CLIT	;
	.BYTE $1F		; ( LEN ) w/o FLAGs
	.ADDR do_AND	;

	.ADDR do_1PLUS, do_PLUS ; DUP c@ 1+ +	; add length
	.ADDR do_SEMI

defword "0BR",,
; Branch to Label if 0 on stack
	LDA 2,X
	ORA 3,X

	BNE @not0
	INX
	INX
	BRA do_JUMP	; 0?
@not0:	

; Now advance IP
; IP+2 --> IP
	CLC
	LDA IP
	ADC #2
	STA IP
	BCC @skip
	INC IP+1
@skip:

	JMP do_DROP

defword "JUMP",,
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

defword "HEREPP","HERE++",
; advance HERE by 1 cell (HERE+2 -> HERE)
	CLC
	LDA DP
	ADC #2
	STA DP
	BCC @skip
	INC DP+1
@skip:
	JMP NEXT

defword "COMMA",",",
; ( XX -- ) save a word XX to HERE and advance
; HERE by 2
; Primitive version!
	; put DP in G1 in ZP
	LDA DP
	STA G1
	LDA DP+1
	STA G1+1
	; save TOS in (DP)
	LDA 2,X
	STA (G1)
	LDA 3,X
	LDY #1
	STA (G1),y
	; Drop
	INX
	INX
	; Advance HERE by 1 cell
	BRA do_HEREPP

defword "LIT_COMMA_ALIAS", "LIT,"
; this is an alias for COMPILE, shorter, so it occupies less space in the bootstrap code in ROM
	JMP do_COLON
	.ADDR do_JUMP, compile_addr

defword "HERE",,
; : HERE	DP @ ;
; Primitive version!
	; put DP in G1 in ZP
	LDA #<DP
	STA G1
	LDA #>DP
	STA G1+1
	; Fetch HERE ie. (DP) and store in TOS
	LDA (G1)
	STA 0,X
	LDY #1
	LDA (G1),y
	STA 1,X
	;
	JMP DEX2_NEXT

defword "SWAP",,
	LDA 2,X
	LDY 4,X
	STY 2,X
	STA 4,X
	LDA 3,X
	LDY 5,X
	STY 3,X
	STA 5,X
	JMP NEXT

defword "CSTORE","C!",
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

defword "STORE","!",
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

;-----------------------------------------------------------------
; p_LATEST point to the latest defined word (using defword macro)
p_LATEST = .ident(.sprintf("__word_%u", __word_last))

.ifndef ACIA
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
.endif

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

	CMP #$04     ; CTRL-D -> BRK
	BEQ @break

	CMP #BKSPACE ; Backspace, CTRL-H
	BEQ @bkspace

	CMP #$7F    ; Backspace key on Linux?
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
  LDA #BKSPACE
	JSR putc	; echo char
	DEY		; else: Y--
	BRA @next
@break:
  BRK
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
OK_STR: CString {"ok "}

; Bootstrap code:
; At this point we can extend our forth in forth
; Must end with $00. That will exit BOOTstrap mode and
; enter the interpreter
BOOT_PRG:
	.BYTE " : ? @ . ; "
	.BYTE " : = - 0= ; "
	.BYTE " : NEG NOT 1+ ; " ; ( N -- -N ) Negate N (returns -N)
	.BYTE " : 0< 8000 AND ; " ; ( N -- F ) Is N strictly negative? Returns non 0 (~true) if N<0
	.BYTE " : IMMEDIATE LATEST @ SETIMM ; "	; sets the latest word IMMEDIATE
	.BYTE " : ' WORD FIND >CFA ; " ; is this ever used?
	.BYTE " : [,] , ; IMMEDIATE "
	; .BYTE " : STOP BREAK ; IMMEDIATE "

	.BYTE " : CHAR 20 PARSE DROP C@ ;"

	; LIT, is an alias for COMPILE, it's shorter ;)
	.BYTE " : IF LIT, 0BR HERE HERE++ ; IMMEDIATE "
	.BYTE " : THEN HERE SWAP ! ; IMMEDIATE "
	.BYTE " : ELSE LIT, JUMP HERE HERE++ SWAP HERE SWAP ! ; IMMEDIATE "

; TEST IF
;	.BYTE " : T IF AAAA ELSE BBBB THEN ; "
;	.BYTE " 1 T . " ; should output AAAA
;	.BYTE " 0 T . " ; should output BBBB

	.BYTE " : BEGIN HERE ; IMMEDIATE "
	.BYTE " : AGAIN LIT, JUMP , ; IMMEDIATE "

; TEST BEGIN AGAIN
;	.BYTE " : TestLoop BEGIN 1 . AGAIN ; TestLoop "

	.BYTE " : UNTIL LIT, 0BR , ; IMMEDIATE "

	; ( is an immediate word that will swallow all characters until a ')' is found
	.BYTE " : ( BEGIN KEY 29 = UNTIL ; IMMEDIATE " ; now we can use ( ) inline comments!

	.BYTE " : PAD HERE 64 + ; " ; $64 = d100, PAD is 100 byte above HERE
 
; TEST BEGIN UNTIL
;	.BYTE " : T 5 BEGIN DUP . CR 1 - DUP 0= UNTIL ; T "

	.BYTE " : WHILE LIT, 0BR HERE HERE++ ; IMMEDIATE "
	.BYTE " : REPEAT LIT, JUMP SWAP , HERE SWAP ! ; IMMEDIATE "
	
; Test BEGIN WHILE REPEAT
;	.BYTE " : T 6 BEGIN 1 - DUP WHILE DUP . REPEAT DROP ; T "

; DO LOOP
	.BYTE " : DO LIT, *DO HERE ; IMMEDIATE " ;
	.BYTE " : LOOP LIT, 1  LIT, *LOOP  LIT, JUMP , ; IMMEDIATE " ;
	.BYTE " : +LOOP LIT, *LOOP LIT, JUMP , ; IMMEDIATE " ;
	.BYTE " : LEAVE R> DROP R@ >R ; " ;

; Test DO-LOOP
;	.BYTE " : TEST1 6 1 DO I . LOOP ; TEST1 " ; Count from 1 to 5
;	.BYTE " : TEST2 A 0 DO I . 2 +LOOP ; TEST2 " ; Count from 0 to 8, 2 by 2

	.BYTE " : >D DUP 0< IF FFFF ELSE 0 THEN ; " ; Extends signed cell into signed double

	; UM+     ( un1 un2 -- ud )
	;  Add two unsigned single numbers and return a double sum
	.BYTE " : UM+ 0 SWAP 0 D+ ; "

; some more stack words
	.BYTE " : NIP SWAP DROP ; " ; ( x1 x0 -- x0 ) removes second on stack
	.BYTE " : PICK 2+ 2* SP + @ ; " ; ( xn ... x1 x0 n -- xn ... x1 x0 xn ) , removes n, push copy of xn on top. n>=0
	.BYTE " : DEPTH "
	.BYTE .sprintf("%X", DTOP-2)
	.BYTE  " SP - 2/ ; "
	.BYTE " : .S DEPTH DUP IF 1+ DUP 1 DO DUP I - PICK . LOOP CR THEN DROP ; " ; print stack, leave cells on stack

	; Double version of stack words
	.BYTE " : 2SWAP >R -ROT R> -ROT ; "
	.BYTE " : 2DUP OVER OVER ; "
	.BYTE " : 2DROP DROP DROP ; "
	.BYTE " : 2ROT >R >R 2SWAP R> R> 2SWAP ; "
	.BYTE " : 2>R R> -ROT SWAP >R >R >R ; "
	.BYTE " : 2R> R> R> R> SWAP ROT >R ; "

	.BYTE " : DU< D- NIP 0< ; " ; ( d1 d2 -- f ) returns wether d1<d2
	.BYTE " : M+ >D D+ ; " ; ( d1 n2 -- d3 ) d3=d1+n2

	.BYTE " : DNEG SWAP NOT SWAP NOT 1 0 D+ ; " ; ( D -- -D ) Negate double-signed D (returns -D)

	.BYTE " : S. DUP 0< IF 2D EMIT NEG THEN . ; "   ; Print as SIGNED integer
	.BYTE " : DS. DUP 0< IF 2D EMIT DNEG THEN D. ; " ; Print as SIGNED double

	.BYTE " : * UM* DROP ; "

; Local variables support (up to 4 locals)
;	We only support 4 locals (x,y,z,t in that order!)
;	If you need 2 locals, use "2 LOCALS" then in the word you can use x,y
;   /!\ At the end of the word, we need to use -LOCALS to free the locals

; 	Local variable storage grows downwards from BP (see address defined in assembly code)
; 	No safety checks are done to avoid running over the dictionary!

	.BYTE " VARIABLE BP "		; defines BP variable (Base Pointer)
	.BYTE .sprintf("%X", BP)	; put the address on the stack
	.BYTE " BP !"

	.BYTE " : LOCALS  BP @ DUP ROT 2* - DUP -ROT ! BP ! ;" ; ( n -- ) Allocates n local variables
	.BYTE " : -LOCALS BP DUP @ @ SWAP ! ;" ; ( n -- ) Dellocates n local variables

	.BYTE " : L@ BP @ + @ ;" ; ( n -- value) helper word to get local var n
	.BYTE " : L! BP @ + ! ;" ; ( n -- value) helper word to save to local var n

	.BYTE " : x 2 L@ ; : x! 2 L! ;"
	.BYTE " : y 4 L@ ; : y! 4 L! ;"
	.BYTE " : z 6 L@ ; : z! 6 L! ;"
	.BYTE " : t 8 L@ ; : t! 8 L! ;"
; End of Local variables support

	.BYTE " :NONAME SWAP C! ;" ; defined as noname to factor it, I can't find a name for that...
	.BYTE " :NONAME 2+ DUP C@ ;" ; defined as noname to factor it, I can't find a name for that...
	; the two NONAME words above leave their CFA on the stack. In the next 3 words (HIDDEN, HIDE, UNHIDE), we'll commit them using [,] (immediate ,).
	; I use 2DUP to keep a copy of the 2 CFA until I won't need them anymore.
	.BYTE " 2DUP : HIDDEN LATEST @ [,] 40 OR [,] ;" ; sets the latest word HIDDEN. The opposite of REVEAL.
	.BYTE " 2DUP : HIDE [,] 40 OR [,] ;" ; ( hdr -- ) Hide a selected word from dictionary, takes the word's header addr (as given by FIND)
	.BYTE " : UNHIDE [,] BF AND [,] ;" ; ( hdr -- ) Unhide a selected word from dictionary, takes the word's header addr (as given by FIND)

	; Recursivity
	; to do RECURSIVE words, we can force a REVEAL of a word by using [ REVEAL ] in its definition
	; or uncomment this RECURSIVE definition and use it inside its definition:
	; .BYTE " : RECURSIVE REVEAL ; IMMEDIATE "

	.BYTE " : DUMP SWAP DUP . DO I C@ C. LOOP ; " ; ( addr1 addr2 -- ) dumps memory from addr1 to addr2

; .NAME ( hdr -- ) takes the addr of the header of a Word in dictionary and print its name
; it will also print a "*" after the word if the word is hidden in the dictionary
	.BYTE " : .NAME DUP 2+ DUP C@ DUP 40 AND >R 1F AND SWAP 1+ SWAP TYPE R> IF SPACE 2A EMIT THEN ; "
; WORDS ( -- ) list all the words in the dictionary
; Format is : HEADER CFA NAME
; Press [qQ] to stop listing, any other key to continue (I use GETC to get a char from input)
	.BYTE " : WORDS 0 LATEST BEGIN @ DUP WHILE DUP . DUP >CFA . .NAME CR SWAP 1+ DUP 10 = IF GETC 20 OR 71 = IF 2DROP EXIT THEN DROP 0 THEN SWAP REPEAT 2DROP ; " ; 16 words per "page"

	.BYTE " MARKER " ; so we can return to this point using FORGET
	.BYTE " PRMP" ; Shows ok prompt to user

	.BYTE " ", $00

;	*= $0200
.segment  "BSS"

LATEST:	.res 2	; Store the latest ADDR of the Dictionary
MODE:	.res 1	; <>0 Execute, 0 compile
BOOT:	.res 1	; <>0 Boot, 0 not boot anymore
SEPR:	.res 1	; Separator for parsing input
BOOTP:	.res 2	; pointer to BOOTstrap code
ERROR:	.res 1	; Error when converting number
INP_LEN: .res 1	; Length of the text in the input buffer
INPUT:	.res 128	; CMD string (extend as needed, up to 256!)
INP_IDX: .res 1	; Index into the INPUT Buffer (for reading it with KEY)
OK:		.res 1	; 1 -> show OK prompt
DP:		.res 2	; Data Pointer: Store the latest ADDR of next free space in RAM (HERE)


; Base of user memory area.
USER_BASE:

; system vectors

;    *=  $FFFA
.segment  "VECTORS"	

    .addr   NMI_vec     ; NMI vector
    .addr   RES_vec     ; RESET vector
    .addr   IRQ_vec     ; IRQ vector
