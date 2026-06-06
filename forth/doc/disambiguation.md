# Disambiguation: Confusing Word Pairs

This document explains the differences between easily-confused words in AlexForth.

## Quick reference

| Pair / Group | Key difference |
|---|---|
| `COMPILE,` vs `POSTPONE` | `COMPILE,` reads inline from code stream; `POSTPONE` reads next word from input stream |
| `LITERAL` vs `LIT` | `LITERAL` (compile-time) injects `LIT`+value into dict; `LIT` (runtime) pushes inline value |
| `[` vs `]` | `[` → execute mode; `]` → compile mode |
| `'` vs `[']` | `'` finds XT now; `[']` compiles XT as literal at compile time |
| `,` vs `C,` | `,` compiles 2 bytes; `C,` compiles 1 byte |
| `EXEC` vs `COMPILE,` | `EXEC` runs now; `COMPILE,` saves for later |
| `;` vs `EXIT` | `;` closes definition (compile-time); `EXIT` returns early (runtime) |
| `HERE` vs `DP` | `HERE` returns value (free dict addr); `DP` returns addr of pointer |
| `LATEST` vs `LAST` | `LATEST` returns addr-of variable; `LAST` returns value-of (`LATEST @`) |
| `?EXEC` vs `STATE` | Same word (aliases) |
| `>ROM` vs `>RAM` | `>ROM` writes to ROM dict; `>RAM` writes to RAM dict |
| `REVEAL` vs `HIDDEN` vs `HIDE` vs `UNHIDE` | REVEAL/HIDDEN operate on LATEST; HIDE/UNHIDE operate on stack addr |
| `?DO` vs `DO` | `DO` always enters loop; `?DO` skips if start == end |
| `LOOP` vs `+LOOP` | `LOOP` increments by 1; `+LOOP` increments by arbitrary value |
| `DOES>` vs `*CREATED` | `DOES>` patches at compile time; `*CREATED` dispatches at runtime |
| `COLON` vs `SEMI` | `COLON` pushes IP, enters body; `SEMI` pops IP, returns |
| `CREATE` vs `VARIABLE` vs `VALUE`/`CONSTANT` vs `DEFER` | CREATE leaves PFA; VARIABLE allocates+PFA; VALUE returns constant; DEFER calls stored XT |
| `WORD` vs `PARSE` | `WORD` splits on space; `PARSE` splits on any delimiter |

## Table of Contents

- [COMPILE, vs POSTPONE](#compile-vs-postpone)
- [LITERAL vs LIT](#literal-vs-lit)
- [LBRAC vs RBRAC (`[` vs `]`)](#lbracc-vs-rbracc---vs-)
- [TICK vs BRACKET-TICK (`'` vs `[']`)](#tick-vs-bracket-tick---vs-)
- [COMMA vs CCOMMA (`, vs C,`)](#comma-vs-ccomma--vs-c)
- [EXEC vs COMPILE,](#exec-vs-compile)
- [SEMICOLON vs EXIT (`;` vs EXIT)](#semicolon-vs-exit--vs-exit)
- [HERE vs DP](#here-vs-dp)
- [LATEST vs LAST](#latest-vs-last)
- [?EXEC vs STATE](#exec-vs-state)
- [>ROM vs >RAM](#rom-vs-ram)
- [REVEAL vs HIDDEN vs HIDE vs UNHIDE](#reveal-vs-hidden-vs-hide-vs-unhide)
- [?DO vs DO](#do-vs-do)
- [LOOP vs +LOOP](#loop-vs-loop)
- [DOES> vs *CREATED](#does-vs-created)
- [COLON vs SEMI](#colon-vs-semi)
- [CREATE vs VARIABLE vs VALUE vs CONSTANT vs DEFER](#create-vs-variable-vs-value-vs-constant-vs-defer)
- [WORD vs PARSE](#word-vs-parse)

---

## COMPILE, vs POSTPONE

Both compile a word address into the dictionary, but they differ in **where the address comes from**.

### COMPILE, (assembly primitive — `forth.s:655`)

The address to compile is embedded **inline** in the definition body, immediately after the `COMPILE,` address. It reads from the code stream (IP), copies to HERE, advances both IP and DP.

Used inside hand-compiled assembly definitions:

```
.ADDR do_COMPILE, do_LIT    ; compiles do_LIT into word being built
.ADDR do_COMPILE, do_SEMI   ; compiles do_SEMI into word being built
```

Equivalent to: "copy the next cell of my own body into the dictionary."

### POSTPONE (high-level — `bootstrap.f:22`)

```
: POSTPONE ' , ; IMMEDIATE
```

Takes the **next word from the input stream** at compile time (it's IMMEDIATE), finds its XT with `'`, and compiles that XT with `,`.

Used inside Forth colon definitions:

```
: foo POSTPONE IF ... ;
```

Equivalent to: "read the next token from input, look it up, compile its XT."

**Summary:** `COMPILE,` reads from the code stream (inline data); `POSTPONE` reads from the input stream (next word typed).

---

## LITERAL vs LIT

Both deal with embedding constants in threaded code.

### LIT (primitive — `forth.s:686`)

The runtime primitive. During execution, it reads the 2 bytes immediately following its address in the code stream and pushes them onto the data stack. IP advances past the literal.

```
LDA (IP)    ; read LO from code stream
STA 0,X
LDY #1
LDA (IP),y  ; read HI from code stream
STA 1,X
CLC         ; advance IP by 2
LDA IP
ADC #2
STA IP
...
JMP DEX2_NEXT
```

### LITERAL (colon word — `forth.s:380`)

A compile-time word. Takes a value from the **data stack** and compiles `LIT addr` + the value into the dictionary. Used like:

```
: foo [ 42 ] LITERAL ;   \ compiles "LIT 002A" into foo
```

Internally, `LITERAL` optimizes: if the value fits in 8 bits, it compiles `CLIT` + byte instead (saving 1 byte per small literal).

**Summary:** `LIT` is the runtime behavior; `LITERAL` is the compile-time word that injects `LIT` (or `CLIT`) + data into a definition.

---

## LBRAC vs RBRAC (`[` vs `]`)

### `[` (LBRAC) — IMMEDIATE (`forth.s:1042`)

Switch to **execution (interpretation) mode**. Sets MODE to non-zero (1).

Used to temporarily leave compilation state mid-definition:

```
: foo [ 42 ] LITERAL ;   \ [ switches to execute mode, 42 pushes, ] goes back
```

### `]` (RBRAC) (`forth.s:1048`)

Switch to **compilation mode**. Sets MODE to 0. Called by `:` to enter compilation state.

**Summary:** `[` → execute mode; `]` → compile mode. They are inverses.

---

## TICK vs BRACKET-TICK (`'` vs `[']`)

### `'` (TICK)

Finds the next word in the input stream **at the time it runs** and leaves its XT on the stack. In the outer interpreter, `'` works as expected; inside a colon definition, if you want to get the XT at compile time, you need `[']`.

### `[']` (BRACKET-TICK) — IMMEDIATE (`bootstrap.f:20`)

```
: ['] ' LITERAL ; IMMEDIATE
```

Finds the next word at **compile time** (because it's IMMEDIATE, it runs during compilation), then compiles its XT as a literal value into the definition.

```
: foo ' DUP ;        \ TICK runs when foo executes → TICK will run when foo is executed, and find the next word in the input stream (not DUP)
: foo ['] DUP ;      \ ['] runs when foo is compiled → compiles DUP's XT as literal
```

**Summary:** `'` runs at interpretation time; `[']` runs at compile time and works inside colon definitions.

---

## COMMA vs CCOMMA (`,` vs `C,`)

### `,` (COMMA — `forth.s:2320`)

Take the top 16-bit cell from the data stack and store it at HERE, then advance HERE by 2.

```
LDA 2,X     ; LO
STA (DP)
LDA 3,X     ; HI → can be zero, doesn't matter
STA (DP)
CLC
LDA DP
ADC #2
STA DP
...
JMP do_DROP
```

### `C,` (CCOMMA — `forth.s:959`)

Take the low byte of ToS, store at HERE, advance HERE by 1.

```
LDA 2,X     ; only LO byte matters
INX         ; DROP
INX
STA (DP)
INC DP      ; advance by 1
BNE @skip
INC DP+1
@skip:
JMP NEXT
```

**Summary:** `,` compiles 2 bytes; `C,` compiles 1 byte.

---

## EXEC vs COMPILE,

### EXEC (`forth.s:1249`)

Take an address from the data stack and **jump to it immediately**. Runs the word now.

```
EXEC
```

### COMPILE, (see above)

Copies an address from the code stream into the dictionary for **later** execution.

**Summary:** EXEC runs now; COMPILE, saves for later.

---

## SEMICOLON vs EXIT (`;` vs EXIT)

In this implementation they share the same runtime code (`do_SEMI` / `do_EXIT`), but differ in semantics.

### `;` (SEMICOLON — `forth.s:1194`)

IMMEDIATE. Ends a colon definition: calls `REVEAL`, compiles `do_SEMI` into the definition, then switches back to execute mode via `LBRAC`.

```
: SEMICOLON  REVEAL COMPILE EXIT LBRAC ;
```

### EXIT (`forth.s:537`)

The runtime primitive that pops IP from the return stack, returning to the caller. Used inside a definition for early return.

```
: foo ... condition IF EXIT THEN ... ;
```

**Summary:** `;` is a compile-only word that closes a definition; `EXIT` is the runtime return primitive that can appear anywhere inside a definition.

---

## HERE vs DP

### HERE (assembly primitive — `forth.s`)

Returns the **value** of the dictionary pointer — the address of the next free dictionary space.

```
LDA DP
STA 0,X
STZ 1,X      ; DP is in ZP, HI byte = 0
JMP DEX2_NEXT
```

### DP

Returns the **address** of the DP zero-page variable (a fixed address in ZP). Equivalent to `HERE` in ANS Forth terminology: `HERE` returns the current value of DP; `DP` returns where DP is stored.

So `HERE @` is `HERE` and `HERE` is `DP @`.

**Summary:** `HERE` gives the value (free dictionary address). `DP` gives the address of the pointer.

---

## LATEST vs LAST

### LATEST (`forth.s:1683`)

Returns the **address of the LATEST variable** — a zero-page cell that holds a pointer to the most recent word header.

```
LDA #<LATEST
STA 0,X
LDA #>LATEST
STA 1,X
JMP DEX2_NEXT
```

### LAST (`forth.s:1691`)

Returns the **value** of LATEST — i.e., the actual header address of the most recently defined word.

```
LDA #<LATEST
STA G1
LDA #>LATEST
STA G1+1
LDA (G1)      ; dereference
STA 0,X
LDY #1
LDA (G1),Y
STA 1,X
JMP DEX2_NEXT
```

Equivalent to: `LAST` = `LATEST @`.

**Summary:** `LATEST` returns address-of; `LAST` returns value-of (the header address itself).

---

## ?EXEC vs STATE

These are the same word: `defword "STATE","?EXEC"`. Returns 0 in compilation mode, non-zero in execute mode. Reads the `MODE` variable.

**Summary:** They are aliases. No difference.

---

## >ROM vs >RAM

Toggle the dictionary target between ROM and RAM areas. Used only during cross-compilation (two-stage build). When `>ROM` is active, all `,` `C,` `ALLOT` operations write to the ROM dictionary space. When `>RAM`, they write to the RAM dictionary space.

Also updates the `HERE_ROM` / `HERE_RAM` shadow pointers and the `TO_ROM` flag.

**Summary:** `>ROM` compiles to ROM (final dictionary); `>RAM` compiles to RAM (variables and mutable data).

---

## REVEAL vs HIDDEN vs HIDE vs UNHIDE

### REVEAL (forth.s:889)

Clear the HIDDEN flag on the **latest** word (pointed to by LATEST). Called by `;` and `;CODE`.

### HIDDEN (forth.s:896)

Set the HIDDEN flag on the **latest** word. Makes it invisible to `FIND`.

### HIDE (forth.s:903)

Set the HIDDEN flag on a **specific word** given its header address from the stack.

### UNHIDE (forth.s:911)

Clear the HIDDEN flag on a **specific word** given its header address.

**Summary:** REVEAL/HIDDEN operate on LATEST; HIDE/UNHIDE operate on a stack-provided address.

---

## ?DO vs DO

### DO

Standard loop: `end start DO ... LOOP` — always enters the loop body (even if start equals end). Increments `I` each iteration until `I >= end`.

### ?DO (IMMEDIATE — `forth.s:1467`)

Conditional loop: `end start ?DO ... LOOP` — checks if start equals end before entering. If they are equal, the entire loop is skipped. Uses `*SKIP_DO` at runtime to decide.

```
: ?DO COMPILE *SKIP_DO HERE HERE++ COMPILE *DO HERE ;
```

**Summary:** `DO` always enters; `?DO` skips if start == end.

---

## LOOP vs +LOOP

### LOOP (IMMEDIATE)

Increment the loop index by **1** each iteration, then check if index >= end.

```
: LOOP COMPILE *LOOP , ;
```

### +LOOP (IMMEDIATE — `forth.s:1498`)

Increment the loop index by an **arbitrary signed value** on the data stack each iteration, then check.

```
: +LOOP COMPILE *+LOOP , ;
```

**Summary:** `LOOP` increments by 1; `+LOOP` increments by a value you specify.

---

## DOES> vs *CREATED

### DOES> (forth.s:1154)

Compile-time word. Patches the last `CREATE`d word so that when it executes, it runs the code following `DOES>` instead of just returning. It overwrites the placeholder address in the created word's body.

```
: DOES>  LAST >CFA 5 +  R>  SWAP ! ;
```

Advances past JMP (3) + do_COLON (2) to reach the action address cell, then stores the address of the DOES> clause from the return stack.

### *CREATED (forth.s:1133)

Runtime primitive inside every `CREATE`d word. When the word executes, *CREATED extracts the PFA from the return stack, then fetches and jumps to the DOES> action address (which defaults to `do_SEMI` if not patched).

```
: *CREATED  R> DUP 2+  SWAP @ >R ;
```

**Summary:** `DOES>` is compile-time patching; `*CREATED` is runtime dispatch. They work together to enable `CREATE ... DOES>` behavior.

---

## COLON vs SEMI

### COLON / ENTER (forth.s:518)

Runtime code that runs upon entering any colon word. Pushes IP to the return stack and sets IP to `W+3` (the word body, after the JMP).

### SEMI / EXIT (forth.s:537)

Runtime code that returns from a colon word. Pops IP from the return stack.

**Summary:** COLON pushes return address and enters the word body; SEMI pops it and returns.

---

## CREATE vs VARIABLE vs VALUE vs CONSTANT vs DEFER

All create named dictionary entries, but with different runtime behaviors.

| Word | Behavior when executed | Storage |
|---|---|---|
| **CREATE** | Leaves the PFA (parameter field address) on the stack | 0 bytes |
| **VARIABLE** | Leaves the PFA on the stack | Allocates 2 bytes (1 cell) |
| **VALUE** | Leaves the stored value on the stack | 2 bytes (the value itself) |
| **CONSTANT** | Same as VALUE (alias) | 2 bytes |
| **DEFER** | Executes whatever XT is stored in the PFA | 2 bytes (an XT) |

VALUE and CONSTANT are the same in this implementation — they use `CREATE ... DOES>` to push the stored cell:

```
: VALUE CREATE , DOES> @ ;
: CONSTANT VALUE ;
```

DEFER stores an XT and executes it:

```
: DEFER CREATE 0 , DOES> @ EXEC ;
```

**Summary:** CREATE leaves an address; VARIABLE allocates + leaves address; VALUE/CONSTANT return a constant; DEFER calls a stored XT.

---

## WORD vs PARSE

### WORD (forth.s:1899)

Find the next space-delimited token from input. Ignores leading spaces. Returns `( addr len )`.

```
lda #' '
bra _parse
```

### PARSE (forth.s:1905)

Like WORD but takes a delimiter character from the stack:

```
: PARSE  ( sep -- addr len )
```

**Summary:** WORD always splits on spaces; PARSE splits on any delimiter you supply.
