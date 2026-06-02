# Internals

## Direct Threaded Code (DTC)

AlexForth implements Direct Threaded Code. Each word is represented by the address of its machine code entry point. The inner interpreter walks through a list of these addresses and jumps to each one in sequence.

## The Inner Interpreter: NEXT

`NEXT` is the heart of the Forth system. It fetches the address pointed to by IP (Instruction Pointer), stores it in W, advances IP by 2 bytes, then jumps to the code at W.

```
NEXT:                       ; (IP) --> W
    LDA (IP)
    STA W
    LDY #1
    LDA (IP),y
    STA W+1
                            ; IP += 2
    CLC
    LDA IP
    ADC #2
    STA IP
    BCC @skip
    INC IP+1
@skip:
    JMP (W)                 ; jump to the word's code
```

## DEX2_NEXT

A common pattern: many primitive words end with `DEX DEX JMP NEXT` to push a value on the data stack and continue execution. `DEX2_NEXT` is placed right before `NEXT` so those words can jump here and fall through, saving ROM space.

```
DEX2_NEXT:
    DEX
    DEX                     ; drop through to NEXT
    JMP NEXT                ; (actually fall-through, no JMP needed)
```

## Zero Page Registers

| Register | Address | Size | Description |
|----------|---------|------|-------------|
| DTOP     | $F2     | 2B   | Top of data stack boundary |
| DP       | $F4     | 2B   | Dictionary Pointer (HERE) |
| G1       | $F6     | 2B   | General purpose register 1 |
| G2       | $F8     | 2B   | General purpose register 2 |
| IP       | $FA     | 2B   | Instruction Pointer |
| W        | $FC     | 2B   | Working register (current word address) |

The data stack pointer is the X register. The return stack pointer is the hardware stack (S register).

## System Variables (BSS segment)

| Variable | Size | Description |
|----------|------|-------------|
| LATEST   | 2B   | Address of the most recently defined word in the dictionary |
| MODE     | 1B   | <>0 = execute mode, 0 = compile mode |
| BOOT     | 1B   | <>0 = bootstrap mode (reading from BOOT_PRG), 0 = interactive |
| SEPR     | 1B   | Separator character for WORD/PARSE |
| BOOTP    | 2B   | Pointer into bootstrap code (BOOT_PRG) |
| BASE     | 1B   | Number base for numeric conversion |
| INP_LEN  | 1B   | Length of text in INPUT buffer |
| INPUT    | 128B | Input buffer (up to 128 bytes) |
| INP_IDX  | 1B   | Current index into INPUT buffer (for KEY) |
| OK       | 1B   | Flag: 1 = show "ok" prompt |
| SCRATCH  | 8B   | Scratch space used by utility routines (also aliased as BCD) |
| HERE_RAM | 2B   | Saved HERE value when cross-compiling to ROM (>ROM) |
| HERE_ROM | 2B   | Saved HERE value when switching back to RAM (>RAM) |
| TO_ROM   | 1B   | Flag: nonzero = currently compiling to ROM area |

## Memory Layout

```
$0000-$00FF   Zero page (registers + system)
$0100-$01FF   Hardware stack (return stack)
$0200-$7FFF   RAM: dictionary, variables, user data
$8000-$F7FF   ROM: kernel, dictionary (cross-compiled)
$F000-$F004   I/O area (emulator or ACIA)
$FFFA-$FFFF   6502 vectors (NMI, RESET, IRQ)
```

### Two-stage compilation

AlexForth uses a cross-compiler (`xcompiler.py`) to compile Forth code written in `bootstrap.f` into `.dat` files that are linked into the ROM. Stage 1 compiles the assembler kernel + cross-compiler, then stage 2 links the pre-compiled dictionary into the final ROM image.

## Data Stack

The data stack grows downward from `DTOP`. X register points to the next free slot below the current top of stack.

- Stack entry at `0,X` and `1,X` (LO, HI) — top of stack (ToS)
- Next entry at `2,X` and `3,X`

To push: `DEX DEX` then store at `0,X`/`1,X`.
To pop: `INX INX`.

## Return Stack

The 6502 hardware stack at `$0100-$01FF` serves as the Forth return stack. `S` register points to the top. Used by `COLON` (push IP) and `EXIT` (pop IP), as well as DO/LOOP for loop control parameters.
