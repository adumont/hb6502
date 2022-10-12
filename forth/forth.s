;
; Alex FORTH for 6502
;
; Copyright (C) 2021-2022 Alexandre Dumont <adumont@gmail.com>
;
; SPDX-License-Identifier: GPL-3.0-only
;

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

	.ifnblank strname
		CString strname, flags
	.else
		CString label, flags
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
DP	= G1-2		; Data/Dictionary Pointer: Store the latest ADDR of next free space in RAM (HERE)
DTOP	= DP-2		; Stack TOP
BKSPACE = $08 ; BACKSPACE = CTRL+BCKSPACE in LINUX (Python)
MAX_LEN = $80		; Input Buffer MAX length, $80= 128 bytes
BP   = $4000		; top of LOCALS stack (grows down). Right below the HW addr block

IMMEDIATE_FLAG = $80
HIDDEN_FLAG = $40

; Offset of the WORD name in the label
; 2 bytes after the Header's addr
HDR_OFFSET_STR = 2

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
.else
	; We need to respect the same length (to keep addresses in place)
	; I can't displace the code by 3 (instead of the JMP)
	; so I put 3 NOP instead
	NOP
	NOP
	NOP
.endif

; store the ADDR of the latest word to
; LATEST variable:

.ifdef LINKING
	; When building Stage 2 (linking)
	; last.dat will restore the values of LAST and HERE we
	; saved in Stage 1 right after compiling the bootstrap code
	.include "last.dat"
.else
	; Normal mode, we use labels to set
	; LATEST:
	LDA #<p_LATEST
	STA LATEST
	LDA #>p_LATEST
	STA LATEST+1

	; "HERE"
	LDA #<USER_BASE
	STA DP
	LDA #>USER_BASE
	STA DP+1
.endif

	; Used only in cross-compilation
	; HERE_ROM <- USER_BASE_ROM
	LDA #<USER_BASE_ROM
	STA HERE_ROM
	LDA #>USER_BASE_ROM
	STA HERE_ROM+1

	stz TO_ROM	; clear TO_ROM flag, by default we compile to RAM

	; Initialize bootP (pointer into Bootstrap code)
	LDA #<BOOT_PRG
	STA BOOTP
	LDA #>BOOT_PRG
	STA BOOTP+1

	LDA #$10 ; $10= d16 Hexadecimal Base
	STA BASE

.ifdef LINKING
	; set the OK flag
	sta OK
.else
	; clear the OK flag
	stz OK
.endif

; Here we copy the RAM block image (from the compiled dictionary)
; to RAM
; we use G1 as a pointer to the source (the data to be copied)
;        G2 as a pointer to the dest. (where we will copy)

	; initialize G1, with address of source (the data to be copied)
	lda #<start_ram_image
	sta G1
	lda #>start_ram_image
	sta G1+1

	; initialize G2, with address of destination
	lda #<RAM_BLOCK_DEST
	sta G2
	lda #>RAM_BLOCK_DEST
	sta G2+1

	ldy #0

@next:
	; Did we reach the end yet? Compare G1 with end_ram_image.
	; end_ram_image is addr of the first byte AFTER the block. We do
	; not want to copy it!
	lda G1+1
	cmp #(>end_ram_image)
	bne @not_finished

	lda G1
	cmp #(<end_ram_image)
	beq @finished

@not_finished:

	lda (G1),y
	sta (G2),y

; we increment both G1 and G2 by 1

	; G1++
	INC G1
	BNE :+
	INC G1+1
:
	; G2++
	INC G2
	BNE :+
	INC G2+1
:
	bra @next
@finished:

; This is a Direct Threaded Code based FORTH

entry_point:
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
; Print version string
	.ADDR do_LIT, VERS_STR
	.ADDR do_COUNT, do_TYPE

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
	.ADDR do_2DUP	; ( addr len addr len )

	.ADDR do_OR, do_EQZ, do_0BR, @cont ; If WORD didn't returned 2 "0", we continue

; else we loop for a new word
	.ADDR do_2DROP
	.ADDR do_JUMP, loop1

@cont:
	.ADDR do_2DUP	; 2DUP ( addr len addr len )

;	.ADDR do_2DUP, do_TYPE	; DEBUG: Show TOKEN

	.ADDR do_FIND ; ( addr len hdr )
	.ADDR do_DUP  ; ( addr len hdr hdr )
	.ADDR do_0BR, numscan ; Not a word? Goto numscan

; Found a word! ( addr len hdr )
	.ADDR do_NROT, do_2DROP ; ( hdr ) header of the word

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
	.ADDR do_2DUP ; 2DUP ( addr len addr len )
	.ADDR do_NUMBER ; ( addr len n flag )

	.ADDR do_0BR, numberError ; 0 -> no error => clean stack & loop

numberParsed:  ; ( addr len n )
	.ADDR do_NROT, do_2DROP ; ( n )

	; here we have the number N on the stack.
	; are we in compilation mode?

	.ADDR do_STATE   ; ( n MODE ) 0: compile, 1 execute

	.ADDR do_0BR, commitN ; Mode = 0 --> CommitN to new word
	; if not 0, continue loop (N already on the stack)

	.ADDR do_JUMP, loop1

numberError:	; Error: ( addr len n )
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
	.ADDR do_LAST, do_FETCH ; LATEST @ @
	.ADDR do_LATEST, do_STORE           ; LATEST !
	.ADDR do_JUMP, executeMode ; back to execute mode and reset input buffer

commitN:
	; ( n )
	; Add number to the word we are defining
	.ADDR do_DUP			; ( n n )
	.ADDR do_LIT, $FF00 	; ( n n FF00 )
	.ADDR do_AND			; ( n hi ) only keep high nibble
	.ADDR do_0BR, @commitN8b	; if hi nibble was 0 then we commit a byte, more compact
@commitN16b:
	.ADDR do_COMPILE, do_LIT	; first add "LIT" ( n )
	.ADDR do_COMMA				; add n to word (  )
	.ADDR do_JUMP, loop1
@commitN8b:
	.ADDR do_COMPILE, do_CLIT	; first add "CLIT" ( n )
	.ADDR do_CCOMMA				; add n to word (  )
	.ADDR do_JUMP, loop1

;------------------------------------------------------

defword "BASE",,
	LDA #<BASE
	STA 0,X
	LDA #>BASE
	STA 1,X
	JMP DEX2_NEXT

defword "OCT",,
	LDA #$08
	STA BASE
	JMP NEXT

defword "BIN",,
	LDA #$02
	STA BASE
	JMP NEXT

defword "DEC",,
	LDA #$0A
	STA BASE
	JMP NEXT

defword "HEX",,
	LDA #$10
	STA BASE
	JMP NEXT

defword "_BP","_BP",
	LDA #<BP
	STA 0,X
	LDA #>BP
	STA 1,X
	JMP DEX2_NEXT

defword "DTOP",,
	LDA #<DTOP
	STA 0,X
	LDA #>DTOP
	STA 1,X
	JMP DEX2_NEXT

defword "TO_ROM",">ROM",
; Used for cross-compilation
	; check we were compiling to RAM (TO_ROM==0?)
	; if TO_ROM != 0 we were already in >ROM mode, skip
	LDA TO_ROM
	BNE	@skip
	; we were compiling to RAM, switch to ROM area
	; HERE_RAM <- HERE
	LDA DP
	STA HERE_RAM
	LDA DP+1
	STA HERE_RAM+1
	; HERE <- HERE_ROM
	LDA HERE_ROM
	STA DP
	LDA HERE_ROM+1
	STA DP+1
	; update TO_ROM flag to !=0
	STA TO_ROM
@skip:
	JMP NEXT

defword "TO_RAM",">RAM",
; Used for cross-compilation
	; check first that we were compiling to ROM (TO_ROM!=0?)
	; if TO_ROM == 0 we were already in >RAM mode, skip
	LDA TO_ROM
	BEQ	@skip
	; we were compiling to ROM, switch to RAM area
	; HERE_ROM <- HERE
	LDA DP
	STA HERE_ROM
	LDA DP+1
	STA HERE_ROM+1
	; HERE <- HERE_RAM
	LDA HERE_RAM
	STA DP
	LDA HERE_RAM+1
	STA DP+1
	; clear TO_ROM flag
	STZ TO_ROM
@skip:
	JMP NEXT

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

defword "SP_STORE","SP!",
	LDA 2,X
	TAX
	JMP DEX2_NEXT

defword "RP_FETCH","RP@",
; RP@ : put the Return Stack Pointer on ToS
	STZ 1,X		; clear HI of ToS
	TXA		; Transfer X...
	TAY		; ... to Y
	TSX		; Transfer SP
	TXA		; ... to A
	STA 0,Y
	TYA		; Restore Y
	TAX		; into X
	JMP DEX2_NEXT

defword "RP_STORE","RP!",
; RP! : Take LO from ToS and store it in RP
	STX G1	; save X to G1
	LDA 2,X	; load ToS LO to A
	TAX		; and transfer to X
	TXS		; then to SP
	LDX G1	; finally restore X from G1
	JMP do_DROP

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

defword "2DUP",,
; ( a b -- a b a b )
	DEX
	DEX
	LDA 7,X
	STA 3,X
	LDA 6,X
	STA 2,X
	LDA 5,X
	STA 1,X
	LDA 4,X
	STA 0,X
	JMP DEX2_NEXT

defword "OVER",,
; ( x y -- x y x )
	LDA 4,X
	STA 0,X
	LDA 5,X
	STA 1,X
	JMP DEX2_NEXT

; Convenience BREAK word we can add in the code
; to force a BREAK
defword "BREAK",,
	JMP NEXT	; set Breakpoint here!

defword "PUSH1","1",
	LDA #1
	STA 0,x
	STZ 1,x
	JMP DEX2_NEXT

defword "DTWICE","D2*",
; ( d -- 2*d )
; d is a signed or unsigned double (2 cells)
; if d is signed, sign is kept
	ASL 4,X
	ROL 5,X
	ROL 2,X
	ROL 3,X
	JMP NEXT

head_bcd_shift:
; addr from ToS into register W
	LDA 2,X
	STA W
	LDA 3,X
	STA W+1

; copy 4 BCD bytes into G1-G2
	LDY #3
@next1:
	LDA (W),Y
	STA G1,Y
	DEY
	BPL @next1

	LDY #4

	RTS

defword "BCDSR",,
; ( addr -- )
; BCD shift right (shift by 1 nibble to the right a BCD number at addr)
; BCD float is 4 bytes long at addr+0 to addr+3
	JSR head_bcd_shift

@oneMoreShift:

	LSR G1
	ROR G1+1
	ROR G1+2
	ROR G1+3

	DEY
	BNE @oneMoreShift

tail_bcd_shift:	; we also call it from BCDSL
; copy 4 BCD bytes from G1-G2 to it's old place
	LDY #3
@next3:
	LDA G1,Y
	STA (W),Y
	DEY
	BPL @next3

	JMP do_DROP

defword "BCDSL",,
; ( addr -- )
; BCD shift left (shift by 1 nibble to the left a BCD number at addr)
; BCD float is 4 bytes long at addr+0 to addr+3
	JSR head_bcd_shift

@oneMoreShift:

	ASL G1+3
	ROL G1+2
	ROL G1+1
	ROL G1

	DEY
	BNE @oneMoreShift

	JMP tail_bcd_shift

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

defword "COMPILE",,
; like doing LIT, addr, COMMA
; we call COMPILE, addr
compile_addr:	; label so we we can jump here from the alias "LIT,"
	; IP points to the [ADDR] that we want to commit
	; (IP)-->(DP)
	LDY #1
	LDA (IP)
	STA (DP)
	LDA (IP),y
	STA (DP),y
	; Add 2 to G2
	CLC
	LDA IP
	ADC #2
	STA IP
	BCC @skip2
	INC IP+1
@skip2:
	; Advance Here by 2 (same code as HEREPP)
	; I could replace this by JMP do_HEREPP or BRA do_HEREPP
	; but when I tried and measured it added more clock cycles...
	CLC
	LDA DP
	ADC #2
	STA DP
	BCC @skip
	INC DP+1
@skip:
	JMP NEXT

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

defword "QDUP","?DUP",
	LDA 2,X
	ORA 3,X
	BEQ :+
	JMP do_DUP
:	JMP NEXT

defword "J","J",
	TXA
	TAY
	TSX
	LDA $0105,X
	STA 0,Y
	LDA $0106,X
	STA 1,Y
	TYA
	TAX
	JMP DEX2_NEXT

defword "I",,
; I is same code as R@
	BRA do_R_AT

defword "R_AT","R@",
; R@ : copy the cell from the Return Stack
; to the Stack
	TXA
	TAY
	TSX
	LDA $0101,X
	STA 0,Y
	LDA $0102,X
	STA 1,Y
	TYA
	TAX
	JMP DEX2_NEXT

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
	; we can remove those two lines, as DP is now in ZP, #>DP == 0
	; LDA #>DP
	; STA 1,X
	STZ 1,X
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


defword "XOR",,
; ( a b -- a^b ) bitwise XOR
	LDA 2,X
	EOR 4,X
	STA 4,X
	LDA 3,X
	EOR 5,X
	STA 5,X
	JMP do_DROP

; Routine used by REVEAL / HIDDEN
_LenOfLastWord:
; store the last WORD header's addr in W
; set Y to 2.
; (W),Y point to LEN field (including Immediate flag)
; returns LEN in A
	LDA #<LATEST
	STA G1
	LDA #>LATEST
	STA G1+1
	;
	LDA (G1)
	STA W
	LDY #1
	LDA (G1),y
	STA W+1
	;
	LDY #2
	LDA (W),Y	; LEN
	RTS

defword "REVEAL",,
; ( -- ) Reveals (unhide) latest word. Used in ; and ;CODE.
	JSR _LenOfLastWord
	AND #($FF-HIDDEN_FLAG)	; removes the HIDDEN_FLAG
	STA (W),Y				; store back in LEN field
	JMP NEXT

defword "HIDDEN",,
; ( -- ) Reveals (unhide) latest word. Used in ; and ;CODE.
	JSR _LenOfLastWord
	ORA #(HIDDEN_FLAG)		; sets the HIDDEN_FLAG
	STA (W),Y				; store back in LEN field
	JMP NEXT

defword "HIDE",,
; Hide a selected word from dictionary, takes the word's header addr (as given by FIND)
; ( HDR -- )
	JSR _getWordLen
	ORA #(HIDDEN_FLAG)		; sets the HIDDEN_FLAG
	STA (W),Y				; store back in LEN field
	JMP do_DROP

defword "UNHIDE",,
; Unhide a selected word from dictionary, takes the word's header addr (as given by FIND)
; ( HDR -- )
	JSR _getWordLen
	AND #($FF-HIDDEN_FLAG)	; removes the HIDDEN_FLAG
	STA (W),Y				; store back in LEN field
	JMP do_DROP

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
	; save TOS in (DP), only LO byte
	LDA 2,X
	; Drop TOS
	INX
	INX
	; commit to dictionary
shortcut_ccomma:
	STA (DP)
	; Advance HERE by 1 byte
	INC DP
	BNE @skip
	INC DP+1
@skip:
	JMP NEXT

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
@loop:
	ROL     6,X     ; Rotate dividend lo cell left one bit.
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

@oflo:
	LDA     #$FF    ; If overflow or /0 condition found,
	STA     4,X     ; just put FFFF in both the remainder
	STA     5,X
	STA     6,X     ; and the quotient.
	STA     7,X

@fin:
	JMP     do_DROP  ; When you're done, show one less cell on data stack

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
	.ADDR do_LAST, do_COMMA ; store value of LATEST in the Link of new Header
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

noheader "STAR_COMMIT_JMP"
; stores a 4C (JMP) into the word being defined
; and advances HERE
	LDA #$4C	; opcode for JMP
	JMP shortcut_ccomma

defword "MARKER",,
; When called, MARKER creates a new word on the dictionary called FORGET
; in its definition, it encodes the FORTH code to restore LATEST and HERE
; at the values they had when running MARKER.
; we encode the values of LATEST and HERE using LIT in the FORGET definition
	JMP do_COLON
	.ADDR do_HERE		; keep current HERE on stack
	.ADDR do_LAST ;

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

noheader "CREATED"
; : CREATED R> DUP 4 + SWAP >R ;
; we extract the addr of next cell, and add 2 --> it's the PFA
	JMP do_COLON
	.ADDR do_FROM_R, do_DUP, do_2PLUS
; then we fetch it, and effectively jump to it. By default it's a do_SEMI.
; if the word was patched by DOES>, it's the DOES> clause addr.
	.ADDR do_SWAP, do_FETCH, do_TO_R
	.ADDR do_SEMI

defword "CREATE",,
	JMP do_COLON
	.ADDR do_CODE				; creates an empty header
	.ADDR do_STAR_COMMIT_JMP 	; adds JMP
	.ADDR do_COMPILE, do_COLON	; compiles do_COLON
	.ADDR do_COMPILE, do_CREATED; compiles do_CREATED
	.ADDR do_COMPILE, @addr_to_do_SEMI	; we start with SEMI , later DOES> can patch it to
	.ADDR do_REVEAL ; we reveal the word
@addr_to_do_SEMI:	; trick: we use this label to store into the created word the address to a do_SEMI call. It will be retrieved by CREATED and pushed to R-stack
	.ADDR do_SEMI

defword "DOES","DOES>",
; the last word supposedly created by CREATE is made of a byte and 3 cells:
; [4C][do_COLON][CREATED][ADDR]
; DOES> will replace the last cell with the ADDR of the DOES> clause. CREATED will "jump" into it.
	JMP do_COLON
	.ADDR do_LAST, do_CFA		; get CFA of latest word (created with CREATE)
	.ADDR do_CLIT
	.BYTE 5
	.ADDR do_PLUS				; advance 7 bytes (1+3 cells, so we point to [do_SEMI] cell)
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
; : VARIABLE CREATE 0 , ;
; Creates a variable, initialized to 0
	JMP do_COLON
	.ADDR do_CREATE
	.ADDR do_PUSH0
	.ADDR do_COMMA
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
; G1 <- LEN, length of the string to parse
	LDA 2,X
	STA G1
; G2 <-- 0000 (Will be the result)
	STZ G2
	STZ G2+1
; Y <-- 0, index into the string to parse
	LDY #0
; Reset error flag (no error by default)
	STZ ERROR

; We save X because we're going to use it to store the base
	PHX	; save X on the stack

; Read first char, If it's a valid number prefix,
; we jump to the corresponding branch to set force the base
	LDA (W),Y
	CMP #'#'
	BEQ @prefixed_base10
	CMP #'$'
	BEQ @prefixed_base16
	CMP #'%'
	BEQ @prefixed_base2
	CMP #'o'
	BEQ @prefixed_base8

; Otherwise, load BASE into X
	LDX BASE

	; we skip the first "multiply G2 by BASE", as atm G2 is 0 anyway
	BRA @read_digit

@next:
	JSR mulG2xBASE

@read_digit:
; We read the next digit, and get it in A
	LDA (W),Y
	JSR nibble_asc_to_value
	BCS @err

; We add the Digit (in A) to the Number (in G2)
	CLC
	ADC G2
	STA G2
	LDA G2+1
	ADC #0
	STA G2+1

	INY
	CPY G1	; Y = LEN ? --> end
	BNE @next

; leave results G2 on the stack
	; Restore X
	PLX

	LDA G2
	STA 4,X
	LDA G2+1
	STA 5,X

	; leave a non 0 value on the stack
	TXA	; X isn't 0, we don't care the value really. we put it in A
	STA 2,X	; we also don't care the value in 3,X
	JMP NEXT

@err:
	; Restore X
	PLX

	; we leave a 0000 on ToS --> error
	STZ 3,X
	STZ 2,X

	JMP NEXT

; branches where we set teh base from the prefix:
@prefixed_base2:
	LDX #2
	INY
	BRA @read_digit
@prefixed_base8:
	LDX #8
	INY
	BRA @read_digit
@prefixed_base10:
	LDX #10
	INY
	BRA @read_digit
@prefixed_base16:
	LDX #16
	INY
	BRA @read_digit

; Multiply G2 by BASE, results in G2
mulG2xBASE:
	CPX #$10
	BEQ @base16
	CPX #$A
	BEQ @base10
	CPX #$2
	BEQ @base2
	CPX #$8
	BEQ @base8
@base16:
	ASL G2
	ROL G2+1
@base8:
	ASL G2
	ROL G2+1
	ASL G2
	ROL G2+1
@base2:
	ASL G2
	ROL G2+1
	RTS

@base10:
	; Multiply G2 by 10
	; G2 *= 2
	ASL G2
	ROL G2+1
	; we temporarily use BCD variable to store a copy of G2
	LDA G2
	STA BCD
	LDA G2+1
	STA BCD+1
	; G2 *= 2
	ASL G2
	ROL G2+1
	; G2 *= 2
	ASL G2
	ROL G2+1
	; G2 <-- G1 + G2
	CLC
	LDA BCD
	ADC G2
	STA G2
	LDA BCD+1
	ADC G2+1
	STA G2+1
	RTS

defword "DIV10","/10",
; Divide number by DECIMAL 10
; copy number in ToS to LONG1 in scratch area
	LDA 3,X
	STA LONG1	; HI
	LDA 2,X
	STA LONG1+1	; LO
	STZ LONG1+2	;  0
	STZ LONG1+3	;  0
; Divide by 10
	JSR divide_by_10
; copy result to ToS
	LDA LONG2+1
	STA 2,X
	LDA LONG2
	STA 3,X
; end
	JMP NEXT

divide_by_10:
; Divide LONG1 by 10, leave in LONG2
; Implementation of the algorithm is unefficient. TODO: rewrite.
; LONG1 and LONG2 format is [HI][LO],[00][00] (2 bytes integer part, 2 bytes of fractional part for precision)
	; First, add 1 to LONG1 integer part
	; Deemed necesary to avoid error in 10% of cases otherwise (for ex. 10/10 would give 0)
	INC LONG1+1
	BNE :+
	INC LONG1+0
; clear result area (LONG2)
:	stz LONG2+3
	stz LONG2+2
	stz LONG2+1
	stz LONG2+0
; start dividing by 10:
	jsr halveLONG1
	ldy #4
@again:
	jsr halveLONG1
	jsr halveLONG1
	jsr halveLONG1
	jsr addLONG1toLONG2
	jsr halveLONG1
	jsr addLONG1toLONG2
	dey
	bne @again
	RTS

halveLONG1:
	; halves LONG1 in place
	LSR LONG1
	ROR LONG1+1
	ROR LONG1+2
	ROR LONG1+3
	RTS

addLONG1toLONG2:
	CLC
	LDA LONG2+3
	ADC LONG1+3
	STA LONG2+3
	LDA LONG2+2
	ADC LONG1+2
	STA LONG2+2
	LDA LONG2+1
	ADC LONG1+1
	STA LONG2+1
	LDA LONG2+0
	ADC LONG1+0
	STA LONG2+0
	RTS

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

defword "QDO","?DO",IMMEDIATE_FLAG
; ( end start -- )
; ?DO will only enter the loop if end != start
; If end == start, the loop will be skipped
	JMP do_COLON
	.ADDR do_COMPILE, do_STAR_SKIP_DO
	.ADDR do_HERE	; ADDR of slot for the 0BR jump's addr, we'll use it in LOOP
	.ADDR do_HEREPP	; advance Here by 1 cell
	.ADDR do_COMPILE, do_STAR_DO
	.ADDR do_HERE	; ADDR of start of do/loop
	.ADDR do_SEMI

noheader "STAR_SKIP_DO"
; ( end start -- )
; Used by ?DO
	JMP do_COLON
	.ADDR do_2DUP		; ( end start end start )
	.ADDR do_MINUS		; ( end start flag )
	.ADDR do_0BR, @skip
	; skip over address and we enter the DO loop
	.ADDR do_FROM_R, do_2PLUS, do_TO_R
	.ADDR do_SEMI
	; Here we handle the case where end=start
	; we have to skip the loop. The addr where to resume execution
	; (addr of the next word is stored in the next cell)
@skip:
	; skip_over_the_loop:
	.ADDR do_2DROP	; we drop end & start as we won't use them
	.ADDR do_FROM_R, do_FETCH, do_TO_R ; get the addr to skip over the DO/LOOP (and push it back to R)  R> @ >R
	.ADDR do_SEMI

defword "PLUS_LOOP","+LOOP",IMMEDIATE_FLAG
;  : +LOOP LIT, *LOOP , ; IMMEDIATE " ;
	JMP do_COLON
	.ADDR do_COMPILE, do_STAR_PLUS_LOOP
call_from_do_LOOP:
	.ADDR do_COMMA

; support for ?DO
	; here we take the value in ToS: 0000 --> skip
	; not 0000 --> assume the addr where to store back the ADDR of HERE at <-- this very point of the execution!
	.ADDR do_DUP
	.ADDR do_0BR, @end_do_loop
	.ADDR do_HERE, do_SWAP, do_STORE	; we store this Addr (HERE) = addr of "after the LOOP" into the corresponding ?DO 0BR branch slot
	.ADDR do_SEMI
@end_do_loop:
	.ADDR do_DROP
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

defword "STATE","?EXEC"		; Renamed as ?EXEC as it's 1 if EXEC mode
; Is it immediate/execution mode?
; returns the value of variable MODE
; 0 : Compilation mode, <>0 : Execution mode
	LDA MODE
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
	JMP do_PRINT	; fallback to "."

defword "CPRINT","C.",
; Print data on top of stack (in hex for now)
; ( n -- )
	JMP cprint

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
@false:
	STZ 2,X
	STZ 3,X
	JMP NEXT
@true:
	LDA #$FF
	STA 2,X
	STA 3,X
	JMP NEXT

defword "GETC",,
; get a single char from IO, leave on stack
	JSR getc ; leaves the char in A
	STA 0,X
	STZ 1,X
	JMP DEX2_NEXT

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
@retry:
	; INP_IDX --> Y
	LDY INP_IDX

	CPY INP_LEN	; reached end of input string?
	BEQ @eos

	LDA INPUT,Y	; load char at INPUT,Y in A
	INC INP_IDX	; ALEX: do we need this?
	RTS

@eos:	; refill input string
	JSR getline
	BRA @retry	; and try again

defword "LATEST",,
; ( -- LATEST )
	LDA #<LATEST
	STA 0,X
	LDA #>LATEST
	STA 1,X
	JMP DEX2_NEXT

defword "LAST",,
; ( -- ADDR ) returns header addr of last word in dict
; equivalent to LATEST @
	LDA #<LATEST
	STA G1
	LDA #>LATEST
	STA G1+1
	LDA (G1)
	STA 0,X
	LDY #1
	LDA (G1),Y
	STA 1,X
	JMP DEX2_NEXT

defword "OR",,
; ( a b -- a|b ) bitwise OR
	LDA 2,X
	ORA 4,X
	STA 4,X
	LDA 3,X
	ORA 5,X
	STA 5,X
	JMP do_DROP

defword "SETIMM",,
; ( hdr -- )
; takes a header to a word in dictionary
; and sets its Immediate flag
	JSR _getWordLen

	ORA #IMMEDIATE_FLAG	; MSB set
	STA (W),Y	; LEN

	JMP do_DROP

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

@nxt_word:
	LDY #2
	LDA (W),Y ; load Length

	TAY
	AND #HIDDEN_FLAG
	BNE @advance_w	; Hidden word! skip it
	TYA

	AND #$1F    ; remove flags (3 MSB)
	CMP 2,X   ; compare to len on stack (1byte)
	BEQ @same_length
	; not same length, advance to next word
@advance_w:
	; W points to the previous entry
	; (W) -> W
	LDY #1
	LDA (W),Y
	TAY			; keep in Y temporarily
	LDA (W)
	STA W
	STY W+1		; now we can save Y to W+1
	BNE @nxt_word
	LDA W+1
	BNE @nxt_word
	; here: not found :(, we put 00 on stack and exit
	STZ 4,x
	STZ 5,x
	JMP do_DROP

@same_length:
; same length: compare str

	; we previously loaded LEN in A --> Y (for STRCMP)
	TAY

	; Before calling STRCMP,
	; we store W+3 in G1 (G1 now points to the counted str)
	; W + 3 --> G1 (now points to STR, not length)
	SEC
	LDA W
	ADC #HDR_OFFSET_STR
	STA G1
	LDA W+1   ; replace with BCC skip / INC G1+1 ?
	ADC #0    ;
	STA G1+1  ;

	JSR STRCMP
	; BNE: not found: look for next word in
	; dictionnary
	BNE @advance_w
	; Found!

@found: ; ADDR is W -> TOS
	LDA W
	STA 4,X
	LDA W+1
	STA 5,X
	JMP do_DROP

defword "XLOHI","<>",
; exchange the LO and HI bytes of the word on ToS
	LDA 2,X
	TAY
	LDA 3,X
	STA 2,X
	TYA
	STA 3,X
	JMP NEXT

defword "FRM1STORE","FRM1!",
; ( 'f2 -- )
	; put 'f2 in G2
	LDA 2,X
	STA G2
	LDA 3,X
	STA G2+1

	; put a 1 on the first nibble (usually after a carry happened)
	LDY #0
	LDA (G2),Y
	ORA #$10
	STA (G2),Y

	JMP do_DROP


head_bcp_addsub:
	; put 'f1 in G1
	LDA 4,X
	STA G1
	LDA 5,X
	STA G1+1
	; put 'f2 in G2
	LDA 2,X
	STA G2
	LDA 3,X
	STA G2+1

	; drop
	INX
	INX
	; drop
	INX
	INX

	RTS


defword "FRMPLUS","FRM+",
; BCD Add mantissas of two Floats Registers
; ( 'm1 'm2 -- carry ) addresses of the mantissas of two float registers
	JSR head_bcp_addsub

	; BCD addition of G1 and G2 into G2
	; I tried doing a loop (Y=3 to 0, but DEY was messing with the carry)
	LDY #3
	SED ; set decimal flag
	CLC ; clear carry

	LDA (G1),Y
	ADC (G2),Y
	STA (G2),Y

	DEY
	LDA (G1),Y
	ADC (G2),Y
	STA (G2),Y

	DEY
	LDA (G1),Y
	ADC (G2),Y
	STA (G2),Y

	DEY
	LDA (G1),Y
	ADC (G2),Y
	STA (G2),Y

	CLD ; clear decimal flag

	BCC @no_carry
; carry is 1
	JMP do_PUSH1
@no_carry:
	JMP do_PUSH0

defword "FRMMINUS","FRM-",
; BCD substract mantissas of two Floats Registers
; ( 'm1 'm2 -- carry ) addresses of the mantissas of two float registers
	JSR head_bcp_addsub

	; BCD addition of G1 and G2 into G2
	; I tried doing a loop (Y=3 to 0, but DEY was messing with the carry)
	LDY #3
	SED ; set decimal flag
	SEC ; clear carry

	LDA (G1),Y
	SBC (G2),Y
	STA (G2),Y

	DEY
	LDA (G1),Y
	SBC (G2),Y
	STA (G2),Y

	DEY
	LDA (G1),Y
	SBC (G2),Y
	STA (G2),Y

	DEY
	LDA (G1),Y
	SBC (G2),Y
	STA (G2),Y

	CLD ; clear decimal flag

	BCC @no_carry
; carry is 1
	JMP do_PUSH1
@no_carry:
	JMP do_PUSH0

defword "FRMGTQ","FRM>?",
; Compare the mantissa of 2 floats registers
; ( 'm1 'm2 -- carry ) addresses of the mantissas of two float registers
	JSR head_bcp_addsub

	LDY #0
@again:
	LDA (G1),Y
	CMP (G2),Y
	BEQ @next
	BCS @true
@false:
	JMP do_FALSE

@next:
	INY
	CPY #4
	BEQ @false
	BRA @again

@true:
	JMP do_TRUE

defword "BCD_HALVE", "BCD2/",
; ( 'mantissa -- )
; takes an addr on ToS of where is the mantissa
; algorithm to halve a BCD: LSR.
; If And $80 -> sbc #$30, if AND #$08 -> sbc #$03
	; put 'mantissa address in W register
	LDA 2,X
	STA W
	LDA 3,X
	STA W+1

	PHX		; save X
	CLC		; clear carry first
	PHP		; save carry

	LDY #0
@next:
	PLP		; restore carry
	LDA (W),Y
	ROR
	PHP		; save carry

	TAX		; save A
	AND #$80
	BEQ @loNibble
	TXA
	SEC
	SBC #$30
	TAX

@loNibble:
	TXA
	AND #$08
	BEQ @continue
	TXA
	SEC
	SBC #$03
	TAX

@continue:
	TXA
	STA (W),y	; save the halved byte

	INY
	CPY #4
	BNE @next

	PLP		; restore carry (we just need to pull this byte off the stack)
	PLX		; restore X
	; ends by dropping the 'mantissa's addr
	JMP do_DROP

defword "BCD_TWICE", "BCD2*",
; ( 'mantissa -- carry )
; takes an addr on ToS of where is the mantissa
; multiplies the BCD mantissa by 2 (by addition)
; returns the carry on ToS (0,1)
	; put 'mantissa address in W register
	LDA 2,X
	STA W
	LDA 3,X
	STA W+1
	; drop 'mantissa
	INX
	INX
	; now we iterate over the bytes, starting by least significant
	SED		; decimal mode
	CLC		; clear carry
	LDY #3
	PHP
@next:
	PLP			; restore carry
	LDA (W),Y
	ADC (W),Y
	STA (W),Y
	PHP			; save carry
	DEY
	BPL @next	; loop while Y>=0

	PLP			; restore carry
	CLD			; end decimal mode (after restoring flags!)
	BCC @zero
	JMP do_PUSH1
@zero:
	JMP do_PUSH0

defword "FDOT","F.",
; get number sign and print "-" if needed
	LDA 5,X
	BPL :+
	; print a - sign
	LDA #'-'
	JSR putc
:
; print first digit
	LDA 4,X
	PHA	; save A for 2nd nibble
	LSR	; here we shift right
	LSR	; to get HI nibble
	LSR
	LSR
	JSR print_nibble
; print a dot '.'
	LDA #'.'
	JSR putc
; print second digit
	PLA
	AND #$0F ; LO nibble
	; fallthrough to print_nibble
	JSR print_nibble
; print 2 next digits
	LDA 3,X
	JSR print_byte
; print last 2 digits
	LDA 2,X
	JSR print_byte
; print an 'e'
	LDA #'e'
	JSR putc
; load exponent again
	LDA 5,X
	TAY
	AND #%01000000
	BEQ @load_exp
; exponent is negative
	; print a - sign
	LDA #'-'
	JSR putc
	; ; We negate it to get the positive exponent
	; TYA	; restore exponent byte
	; ORA #%10000000
	; EOR #$FF
	; INA
	; BRA @print_exp
@load_exp:
	TYA	; restore exponent byte
	AND #%00111111
@print_exp:
	JSR hex8toBCD
	LDA HTD_OUT
	JSR print_byte
; Drop two cells
	INX
	INX
	JMP do_DROP

hex8toBCD:
; Only for the exponent, 6 bits only, so we don't use HTD_OUT+1
; (max is 63 so 1 byte BCD)
; Adapted from http://6502.org/source/integers/hex2dec.htm
; A       = Hex input number (gets put into HTD_IN)
; HTD_OUT = 1s & 10s output byte

	CLD				; (Make sure it's not in decimal mode for the
	STA HTD_IN		; ADCs below.)
	STZ HTD_OUT		; (NMOS 6502 will need LDA #0, STA ...)
	; STZ HTD_OUT+1
	LDY #8

@htd1:
	ASL HTD_IN
	ROL HTD_OUT
	; ROL HTD_OUT+1

	DEY				; The shifting will happen 5 times.  After
	BEQ @htd3		; the last shift, you don't check for digits of
					; 5 or more.
	LDA HTD_OUT
	AND #$F
	CMP #5
	BMI @htd2

	CLC
	LDA HTD_OUT
	ADC #3
	STA HTD_OUT

@htd2:
	LDA HTD_OUT
	CMP #50H
	BMI @htd1

	CLC
	ADC #30H
	STA HTD_OUT

	BRA @htd1		; NMOS 6502 can use JMP.

@htd3:
	RTS

defword "SPACE",,
; Print a space		( -- )
	LDA #' '
	JSR putc
	JMP NEXT

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

defword "PARSE",,
; parse input buffer with separator SEPR (and advance INP_IDX)
; ( SEP -- ADDR LEN )
	lda 2,x
	inx
	inx
	bra _parse

defword "WORD",,
; Find next word in input buffer (and advance INP_IDX)
; ( -- ADDR LEN )
	lda #' '

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
	inc OK		; we mark 1 the OK flag
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

	; Addr of INPUT + INP_IDX (in A) -1 --> ToS
	DEA
	CLC
	ADC #<INPUT
	STA 0,X
	LDA #>INPUT
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
	inc OK		; we mark 1 the OK flag
@endW:
	; compute length
	LDA INP_IDX
	SEC
	SBC G1	; earlier we saved INP_IDX in G1 ;)
	STA 0,X
	STZ 1,X
	JMP DEX2_NEXT

defword "2PLUS","2+",
	CLC
	LDA 2,x
	ADC #2
	STA 2,x
	BCC @skip
	INC 3,X
@skip:
	JMP NEXT

defword "TWICE","2*",
; ( n -- 2*n )
; n is one signed or unsigned cell
; if n is signed, sign is kept
	ASL 2,X
	ROL 3,X
	JMP NEXT

defword "CR",,
; Print a new line	( -- )
	JSR _crlf
	JMP NEXT

_crlf:
	LDA #$0a ; CR
	JSR putc
	LDA #$0d ; LF
	JMP putc

defword "CFA",">CFA",
	; ( HDR -- CFA )
	; takes the addr to the header of a word
	; returns the code field address pointer

	; ToS + 2 --> W
	CLC
	LDA 2,X
	ADC #2
	STA W
	LDA 3,X
	ADC #0
	STA W+1

	; get length+flags byte
	LDA (W)
	AND #$1F	; remove flags, keep only length
	INA

	; W + length --> ToS
	CLC
	ADC W
	STA 2,X
	LDA W+1
	ADC #0
	STA 3,X
	JMP NEXT

defword "DO",,IMMEDIATE_FLAG
;  : DO LIT, *DO HERE ; IMMEDIATE
	JMP do_COLON
	.ADDR do_PUSH0	; we leave a 0000. Not a valid ADDR. We'll use it in LOOP to know if it was a DO or ?DO
	.ADDR do_COMPILE, do_STAR_DO
	.ADDR do_HERE
	.ADDR do_SEMI

noheader "STAR_DO"
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

defword "LOOP",,IMMEDIATE_FLAG
;  : LOOP LIT, 1 LIT, *LOOP , ; IMMEDIATE
	JMP do_COLON
	.ADDR do_COMPILE, do_STAR_LOOP
	.ADDR do_JUMP, call_from_do_LOOP

noheader "STAR_LOOP"
; ( INC -- )
; Used by LOOP, takes NO arguments on the stack (we know the increment is 1)

; Previously, *LOOP and *+LOOP were the same. LOOP was just inserting a PUSH1 before calling *LOOP. Now I have differentiated both. In a normal LOOP, we don't need to PUSH1 , so we save 2 bytes in the dictionary (and it's a bit faster), also *LOOP already knows that the increment it 1.

; *LOOP should be followed by [ADDR], so IP points to ADDR
; where ADDR is the word after DO
; *LOOP will either run it (ie. jump back to DO) or bypass it (ie. leaving the DO LOOP)

	; Save X in Y
	TXA
	TAY

	; Transfer SP to X
	TSX
	; END and I (of DO/LOOP) are on the 6502 stack, after transfering S to X
	; we can now address them like that:
	; $104,X $103,X [ END ]
	; $102,X $101,X [  I  ]

	; I++
	INC $101,X
	BNE @skip
	INC $102,X
@skip:

from_STAR_PLUS_LOOP:
	; Compare I to END
	LDA $102,X	; HI I ; we can skip that line, as $102,X is in A already!
	CMP $104,X	; HI END
	BNE :+
	LDA $101,X	; LO I
	CMP $103,X	; LO END
:	BCC @loop_again

; Here we exit the LOOP
	; Remove I and END from 6502 Hw Stack
	INX	; INX is only 2 cycles, vs PLA 4 cycles
	INX
	INX
	INX
	TXS
	; Restore X from Y
	TYA
	TAX

	; Skip over the [ADDR]
	; IP+2 --> IP
	CLC
	LDA IP
	ADC #2		; A<-A+2
	STA IP
	BCC :+
	INC IP+1
:	JMP NEXT

@loop_again:
	; Restore X from Y
	TYA
	TAX

	; IP just happen to point to [ADDR]!
	; now we call JUMP
	JMP do_JUMP

noheader "STAR_PLUS_LOOP"
; ( INC -- )
; Used by LOOP, +LOOP, takes the increment on the stack

; *+LOOP should be followed by [ADDR], so IP points to ADDR
; where ADDR is the word after DO
; *+LOOP will either run it (ie. jump back to DO) or bypass it (ie. leaving the DO LOOP)

	; Copy INC (on ToS) to G1
	LDA 2,X
	STA G1
	LDA 3,X
	STA G1+1

	; Drop INC from ToS
	INX
	INX
	; Save X in Y
	TXA
	TAY

	TSX
	; END and I (of DO/LOOP) are on the 6502 stack, after transfering S to X
	; we can now address them like that:
	; $104,X $103,X [ END ]
	; $102,X $101,X [  I  ]

	; Add INC to I: I+INC -> I
	CLC
	LDA G1
	ADC $101,X
	STA $101,X
	LDA G1+1
	ADC $102,X
	STA $102,X

	; the end is ine STAR_LOOP (it was the same code, so I branch to it)
	BRA from_STAR_PLUS_LOOP

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

defword "SP_FETCH","SP@",
; Put Data Stack Pointer on the stack
	TXA
	INA
	INA
	STA 0,X
	STZ 1,X
	JMP DEX2_NEXT

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

defword "AND",,
; ( a b -- a&b ) bitwise AND
	LDA 2,X
	AND 4,X
	STA 4,X
	LDA 3,X
	AND 5,X
	STA 5,X
	JMP do_DROP

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

defword "EMIT",,
; EMIT emit a single char
	; char is on stack
	LDA 2,X
	JSR putc
	JMP do_DROP

defword "ULESS","U<",
; U< ( n1 n2 -- flag ), unsigned comparison
	; First compare HI byte
	LDA 5,X
	CMP 3,X
	BCC @true
	BNE @false
	; HI byte were the same, compare LO byte
	LDA 4,X
	CMP 2,X
	BCC @true
@false:
	STZ 5,X
	STZ 4,X
	JMP do_DROP
@true:
	LDA #$FF
	STA 5,X
	STA 4,X
	JMP do_DROP

defword "JUMP",,
; (IP) points to literal address to jump to
; instead of next word ;)
	LDY #1
	LDA (IP),y
	TAY
	LDA (IP)
	STA IP
	TYA
	STA IP+1
	JMP NEXT

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
	; save TOS in (DP)
	LDA 2,X
	STA (DP)
	LDA 3,X
	LDY #1
	STA (DP),y
	; Drop
	INX
	INX
	; Advance HERE by 1 cell
	BRA do_HEREPP

defword "NOT",,
; ( a -- not(a) ) bitwise NOT
	LDA 2,X
	EOR #$FF
	STA 2,X
	LDA 3,X
	EOR #$FF
	STA 3,X
	JMP NEXT

defword "MINUS","-",
	SEC
	LDA 4,X
	SBC 2,X
	STA 4,X
	LDA 5,X
	SBC 3,X
	STA 5,X
	JMP do_DROP

defword "PLUS","+",
	CLC
	LDA 2,X
	ADC 4,X
	STA 4,X
	LDA 3,X
	ADC 5,X
	STA 5,X
	JMP do_DROP

defword "1PLUS","1+",
	INC 2,X
	BNE @skip
	INC 3,X
@skip:
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

defword "2DROP",,
	INX
	INX
	BRA do_DROP

defword "DROP",,
	INX
	INX
	JMP NEXT

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

defword "PUSH0","0",
	STZ 0,x
	STZ 1,x
	JMP DEX2_NEXT

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

defword "HERE",,
; : HERE	DP @ ;
; Primitive version!
	; Fetch HERE ie. (DP) and store in TOS
	LDA DP
	STA 0,X
	LDA DP+1
	STA 1,X
	;
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

defword "DUP",,
	LDA 2,X
	STA 0,X
	LDA 3,X
	STA 1,X
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

;-----------------------------------------------------------------
; p_LATEST point to the latest defined word (using defword macro)
p_LATEST = .ident(.sprintf("__word_%u", __word_last))

; Input Buffer Routines

start_comms_block:
.ifdef ACIA
	; This block is $24 bytes long
	; see the .res $24 below for padding
	.include "lib/acia.s"
.else
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
end_comms_block:

; we pad the previous block to $24 bytes, so it has a fixed size.
; needed so that the cross-compiled dictionary is in the same place
; independently of wether we build for Emulator or HW
.res $24-(end_comms_block-start_comms_block)

; Getline refills the INPUT buffer
; Bootstrapping mode BOOT<>0 then refill from BOOT_PRG
; If we are in user mode (BOOT=0) we refill from user input
getline:
	STZ INP_IDX	; reset Input index
	LDY #0

	LDA BOOT
	BNE boot_refill

	PHX		; save X as we'll mess with it

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

	CMP #$0A ; \n
	BEQ @finish

	CMP #$0D ; \r
	BEQ @finish

	JSR putc	; echo char

	BRA @next
@maxlen:
	TAX
	LDA #BKSPACE	; send bckspace to erase last char
	JSR putc
	TXA		; restore new char
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
	PLX			; Restore X
	JMP _crlf

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
@next:
	INY
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
	STA INPUT,Y
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
	; we clobber Y here. (replace with TAY/TYA with PHA/PLA if not acceptable)
	TAY	; save A for 2nd nibble
	LSR	; here we shift right
	LSR	; to get HI nibble
	LSR
	LSR
	JSR print_nibble
	TYA
	AND #$0F ; LO nibble
	; fallthrough to print_nibble

print_nibble:
	CMP #$0A
	BCC @skip
	ADC #$66
@skip:
	EOR #$30
	JMP putc

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
	RTI
NMI_vec:
	; this NMI service routine will allow the user to get control back and drop into
	; the interpreter. In the emulator, it's called when pressing teh ESC key
	; If the computer was in BOOT mode (still interpreting the bootstrap code) it will drop out of boot mode
	; Stack and return stack will be reset
	; User will be presented again with the versions strings and the ok prompt
	; All memory and dictionary should be available as left when the NMI was triggered
	CLD             ; clear decimal mode
	LDX #$FF
	STX MODE		; set MODE to FF --> reset to exec mode
	STX OK			; set the OK flag so the prompt shows
	STZ BOOT		; force not in BOOT mode. that allow to force interrupt out of boot mode
	TXS             ; set the stack pointer
	LDX #DTOP
	CLI
	JMP entry_point

; Version string is now generated with version.sh
; VERS_STR: CString {"ALEX FORTH v0", $0A, $0D, "(c) 2021-2022 Alex Dumont", $0A, $0D}
.include "version.dat"

WHAT_STR: CString {" ?", $0A, $0D}
OK_STR: CString {"ok "}

; we set the address of USER_BASE_ROM right after the bootstrap code (if any)
USER_BASE_ROM:

start_rom_image:
.ifdef LINKING
.out "Including rom.dat"
.incbin "rom.dat"
.endif
end_rom_image:

start_ram_image:
.ifdef LINKING
.out "Including ram.dat"
.incbin "ram.dat"
.endif
end_ram_image:

; Bootstrap code:
; At this point we can extend our forth in forth
; Must end with $00. That will exit BOOTstrap mode and
; enter the interpreter
BOOT_PRG:
.ifdef LINKING
	; We can add things here, they will be compiled at boot time.
	; This was superseeded by cross-compilation of the bootstrap code
	; But it's still available. This will note run in Stage 1.

	; .BYTE " : T .( Hello world!) ;"
.endif
	; keep outside the ifdef/endif, as we always want to end the BOOT_PRG with $00
	.BYTE " ", $00

.segment  "BSS"

LATEST:	.res 2	; Store the latest ADDR of the Dictionary
MODE:	.res 1	; <>0 Execute, 0 compile
BOOT:	.res 1	; <>0 Boot, 0 not boot anymore
SEPR:	.res 1	; Separator for parsing input
BOOTP:	.res 2	; pointer to BOOTstrap code
BASE:	.res 1	; Base for number conversion
ERROR:	.res 1	; Error when converting number
INP_LEN: .res 1	; Length of the text in the input buffer
INPUT:	.res 128	; CMD string (extend as needed, up to 256!)
INP_IDX: .res 1	; Index into the INPUT Buffer (for reading it with KEY)
OK:		.res 1	; 1 -> show OK prompt
SCRATCH: .res 8	; 8 bytes we can use in routines...
HERE_RAM: .res 2	; these variable will be used by >ROM / >RAM
HERE_ROM: .res 2	; to save/restore the HERE value (when cross compiling)
TO_ROM:   .res 1	; flag that indicate if we're compiling to ROM

BCD = SCRATCH
LONG1 = SCRATCH		; Two long (4bytes) numbers in scratch area.
LONG2 = SCRATCH+4	; both are used in divide_by_10
HTD_IN = SCRATCH
HTD_OUT = SCRATCH+1

RAM_BLOCK_DEST:	.res (end_ram_image-start_ram_image)

; Base of user memory area.
USER_BASE:

; system vectors

;    *=  $FFFA
.segment  "VECTORS"

	.addr   NMI_vec     ; NMI vector
	.addr   RES_vec     ; RESET vector
	.addr   IRQ_vec     ; IRQ vector
