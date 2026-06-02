# Control Structures

Control structures are implemented as IMMEDIATE words that compile branch primitives (JUMP and 0BR) into the dictionary. They don't exist at runtime — they generate code during compilation.

## IF ELSE THEN

### IF ( -- addr )
Compile a `0BR` followed by a placeholder address. The addr marks where the forward branch target goes.

```
: IF  COMPILE 0BR JUMP> ; IMMEDIATE
```

`JUMP> = HERE HERE++` — leaves HERE on stack and advances DP by 2, creating an empty slot for the address.

### ELSE ( addr1 -- addr2 )
Compile `JUMP` with a placeholder, then fill the IF's placeholder with the current HERE.

```
: ELSE  COMPILE JUMP JUMP> SWAP >HERE ; IMMEDIATE
```

- `>HERE = HERE SWAP !` — stores current HERE into the placeholder slot

### THEN ( addr -- )
Fill the placeholder with the current HERE (the address after the THEN clause).

```
: THEN  >HERE ; IMMEDIATE
```

### Example:
```
: TEST  IF  ." YES"  ELSE  ." NO"  THEN  ." DONE" ;
```
Compiles to:
```
COLON
0BR   ----+-- jump to ELSE if false
HERE      |  (IF clause: "YES")
JUMP   ---|--+-- jump over ELSE
HERE      |  |  (ELSE clause: "NO")
       <--+  |
HERE   <-----+  (THEN: "DONE")
EXIT
```

## BEGIN loops

### BEGIN ( -- addr )
Mark the start of a loop. Leaves the current HERE on the stack so `AGAIN` and `UNTIL` can jump back to it.

```
: BEGIN  HERE ; IMMEDIATE
```

### AGAIN ( addr -- )
Compile an unconditional JUMP back to addr.

```
: AGAIN  COMPILE JUMP , ; IMMEDIATE
```

### UNTIL ( addr -- )
Compile a conditional 0BR that jumps back to addr if flag is zero.

```
: UNTIL  COMPILE 0BR , ; IMMEDIATE
```

### WHILE ( addr1 -- addr1 addr2 )
Compile a conditional forward branch (IF), leaving its address for REPEAT to patch.

```
: WHILE  POSTPONE IF ; IMMEDIATE
```

### REPEAT ( addr1 addr2 -- )
Compile a JUMP back to the BEGIN address, then patch the WHILE forward branch.

```
: REPEAT  COMPILE JUMP SWAP , >HERE ; IMMEDIATE
```

## DO loops

### DO ( end start -- )
Initialize a DO loop. Compiles `*DO` and leaves the address of the loop body start.

```
: DO  0 COMPILE *DO HERE ; IMMEDIATE
```

The `0` marks this as a regular DO (not ?DO). The LOOP word checks this.
`*DO` pushes `end` and `start` onto the return stack:

```
STAR_DO:
LDA 5,X  ; end HI -> R-stack
PHA
LDA 4,X  ; end LO
PHA
LDA 3,X  ; start HI
PHA
LDA 2,X  ; start LO
PHA
INX      ; drop start
INX
JMP do_DROP  ; drop end
```

### ?DO ( end start -- )
Conditional DO: only enters the loop if `end ≠ start`. If equal, skips the entire loop.

```
: ?DO  COMPILE *SKIP_DO  HERE HERE++  COMPILE *DO  HERE ; IMMEDIATE
```

`*SKIP_DO` checks if end != start:
```
STAR_SKIP_DO:
2DUP - 0BR @skip    ; if end != start, continue
R> 2+ >R            ; skip over the loop
EXIT
@skip:
2DROP               ; drop end & start
R> @ >R             ; get address to skip to
EXIT
```

### LOOP ( addr -- )
Increment the loop counter by 1, check if done. If not, jump back to addr.

```
: LOOP  COMPILE *LOOP  ; IMMEDIATE
(and also handles patching the ?DO slot)
```

`*LOOP` increments the start value by 1 and checks against the end limit.

### +LOOP ( addr -- )
Like LOOP but increment by a value taken from the data stack.

```
: +LOOP  COMPILE *+LOOP , ; IMMEDIATE
```

The actual increment is the value on the stack when `*+LOOP` executes.

### LEAVE ( -- )
Force exit from a DO loop at the next iteration check. Sets I equal to the limit.

```
: LEAVE  R> R> DROP R> DUP >R >R >R ;
```

### I ( -- n )
Copy the loop index from the return stack to the data stack.

```
I:
TXA
TAY
TSX
LDA $0101,X  ; read index from R-stack
STA 0,Y
LDA $0102,X
STA 1,Y
TYA
TAX
JMP DEX2_NEXT
```

### J ( -- n )
Copy the outer loop index (from a nested DO loop) to the data stack.

```
J:
TXA
TAY
TSX
LDA $0105,X  ; outer loop index (4 bytes deeper than I)
STA 0,Y
LDA $0106,X
STA 1,Y
TYA
TAX
JMP DEX2_NEXT
```

## Anatomy examples

See [anatomy.md](anatomy.md) for detailed memory dumps showing how each control structure is compiled.
