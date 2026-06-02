# System Words

## ABORT ( -- )
Reset the Forth system. Clears the data and return stacks, resets MODE to execute, sets OK flag, forces BOOT mode off. Returns to the main interpreter loop.

```
ABORT:
LDX #$FF
STX MODE     ; execute mode
STX OK       ; show prompt
STZ BOOT     ; leave boot mode
TXS          ; reset return stack
LDX #DTOP    ; reset data stack
LDA #<rsin
STA IP
LDA #>rsin
STA IP+1
JMP NEXT
```

## CLS ( -- )
Clear the data stack only. Resets X pointer to DTOP.

```
LDX #DTOP
JMP NEXT
```

## BREAK ( -- )
A no-op word that exists solely as a debugging breakpoint target. Place `BREAK` in your code and set a debugger breakpoint on `do_BREAK`.

```
BREAK:
JMP NEXT    ; <-- set breakpoint here
```

## STATE / ?EXEC ( -- flag )
Return the current mode: 0 = compile mode, nonzero = execute mode.

```
LDA MODE
STA 0,X
STZ 1,X
JMP DEX2_NEXT
```

## MODE ( -- addr )
Push the address of the MODE variable.

```
LDA #<MODE
STA 0,X
LDA #>MODE
STA 1,X
JMP DEX2_NEXT
```

## System Variables (BSS segment)

Defined in the `.BSS` segment at `$0200`+:

```
LATEST: .res 2   ; pointer to last dictionary entry
MODE:   .res 1   ; <>0 execute, 0 compile
BOOT:   .res 1   ; <>0 bootstrap mode
SEPR:   .res 1   ; parse separator
BOOTP:  .res 2   ; bootstrap code pointer
BASE:   .res 1   ; numeric conversion base
INP_LEN: .res 1  ; input buffer length
INPUT:  .res 128 ; input buffer
INP_IDX: .res 1  ; input buffer index
OK:     .res 1   ; prompt flag
SCRATCH: .res 8  ; general scratch space (also BCD)
HERE_RAM: .res 2 ; saved HERE for RAM compilation
HERE_ROM: .res 2 ; saved HERE for ROM compilation
TO_ROM: .res 1   ; compilation target flag
```

## RESET Vector

On reset, the 6502 jumps to `RES_vec` which:

1. Clears decimal mode (CLD)
2. Initializes return stack (LDX #$FF, TXS)
3. Sets MODE to $FF (execute mode)
4. Sets BOOT to $FF (bootstrap mode)
5. Initializes data stack (LDX #DTOP)
6. Sets LATEST and DP from labels (stage 1) or `.dat` files (stage 2)
7. Copies the RAM-based dictionary image from ROM to RAM
8. Sets IP to the start of the Forth program (`forth_prog`)
9. Jumps to NEXT to begin execution

## NMI Vector

The NMI handler provides a soft-reset/break-in:

1. Clears stacks and resets MODE to execute
2. Sets OK flag and clears BOOT mode
3. Returns to `entry_point`, re-running the main Forth program

In the emulator, pressing ESC triggers an NMI, allowing the user to break out of any running code and return to the interpreter.

## IRQ Vector

Currently unused. Just an RTI.

## DP ( -- addr )
Push the address of the DP variable (Dictionary Pointer, also called HERE).

```
LDA #<DP
STA 0,X
STZ 1,X      ; DP is in zero page, so HI byte = 0
JMP DEX2_NEXT
```

## DTOP ( -- addr )
Push the address of the DTOP variable (data stack top boundary).

```
LDA #<DTOP
STA 0,X
LDA #>DTOP
STA 1,X
JMP DEX2_NEXT
```

## _BP ( -- addr )
Push the address BP ($4000), which is the top of the locals stack (grows down).

```
LDA #<BP
STA 0,X
LDA #>BP
STA 1,X
JMP DEX2_NEXT
```

## >ROM ( -- )
Switch compilation target to ROM. Saves current HERE to HERE_RAM, restores HERE from HERE_ROM.

## >RAM ( -- )
Switch compilation target back to RAM. Saves current HERE to HERE_ROM, restores HERE from HERE_RAM.
