# Anatomy of compiled words

- [Anatomy of compiled words](#anatomy-of-compiled-words)
- [Literals](#literals)
  - [String literal](#string-literal)
  - [Char literal](#char-literal)
- [Conditionals](#conditionals)
  - [IF THEN](#if-then)
  - [IF ELSE THEN](#if-else-then)
- [BEGIN Loops](#begin-loops)
  - [BEGIN AGAIN](#begin-again)
  - [BEGIN UNTIL](#begin-until)
  - [BEGIN WHILE REPEAT](#begin-while-repeat)
- [DO Loops](#do-loops)
  - [DO LOOP](#do-loop)
  - [DO +LOOP](#do-loop-1)
  - [?DO LOOP](#do-loop-2)
  - [?DO +LOOP](#do-loop-3)
- [Defining words](#defining-words)
  - [CREATE example](#create-example)
  - [CREATE DOES\> example](#create-does-example)

The purpose of this page is to document the anatomy of compiled words in AlexFORTH. I'm using my [FORTH Decompiler](https://github.com/adumont/hb6502/blob/main/forth/doc/examples.md#forth-decompiler) word `SEE` and I have added some comments and arrows to illustrate the execution flow inside the words. Be sure to also read [Headers in AlexForth](headers.md) to understand the header structure.

The output of `SEE` produces 3 columns:
- The first column shows the address in the code
- The second column is the actual content of the cell. Usually it's the XT/CFA of a word, or another address to jump to. It can also be a literal.
- The last colunm shows the name of the word -- if found in the dictionary--, or the content again otherwise.

```
ADDR VALUE NAME
0860 082F  HEAD
```

# Literals

## String literal

This example shows how a word with a string literal and a char-literal are compiled:

```
ok : T .( HelloWorld) CHAR " EMIT ;
ok SEE T
02AD 8263 COLON
02AF 88BE LITSTR "HelloWorld"
02BC 8451 COUNT
02BE 8B57 TYPE
02C0 88A6 CLIT 22 
02C3 8DCB EMIT
02C5 8280 EXIT
ok T
HelloWorld"ok 
```

## Char literal

```
: T CHAR " EMIT 2 3 + . ;

ok SEE T
02CC 8263 COLON
02CE 88A6 CLIT 22 
02D1 8DCB EMIT
02D3 88A6 CLIT 02 
02D6 88A6 CLIT 03 
02D9 8E5E +
02DB 9829 .
02DD 8280 EXIT
ok 
```

# Conditionals

## IF THEN

```
ok : TEST HEAD IF IF-CLAUSE THEN TAIL ;
ok SEE TEST
085E 8170 COLON
0860 082F HEAD
0862 8B75 0BR         ----+   Jump over the IF-CLAUSE 
0864 0868 0868            |   to the TAIL clause (after THEN)
0866 0840 IF-CLAUSE       |
0868 0851 TAIL        <---+
086A 818D EXIT
ok
```

## IF ELSE THEN

```
ok : TEST HEAD IF IF-CLAUSE ELSE ELSE-CLAUSE THEN TAIL ;
ok SEE TEST
0887 8170 COLON
0889 082F HEAD
088B 8B75 0BR           ----+       Jump to ELSE code
088D 0895 0895              |
088F 0840 IF-CLAUSE         |
0891 8BCE JUMP          ----|----+  Jump over ELSE code (to the TAIL clause)
0893 0897 0897              |    |
0895 087A ELSE-CLAUSE   <---+    |
0897 0851 TAIL          <--------+
0899 818D EXIT
ok 
```

# BEGIN Loops

## BEGIN AGAIN

```
ok : T BEGIN 1 . CR AGAIN ;
ok SEE T 
07E1 8108 COLON
07E3 819B 1       <---+   BEGIN
07E5 8C32 .           |
07E7 8A23 CR          |
07E9 8B6D JUMP    ----+   Inconditional jump back to BEGIN
07EB 07E3 07E3
07ED 8125 EXIT
ok 
```

## BEGIN UNTIL

```
ok : T BEGIN DUP . CR 1 - DUP 0= UNTIL ;
ok SEE T 
072B 8108 COLON
072D 8CD4 DUP     <---+   BEGIN
072F 8C32 .           |
0731 8A23 CR          |
0733 819B 1           |
0735 8BBA -           |
0737 8CD4 DUP         |
0739 878D 0=          |
073B 8B14 0BR         |   Conditional branching back to BEGIN
073D 072D 072D    ----+
073F 8125 EXIT
ok 
```


## BEGIN WHILE REPEAT

```
ok : T 1111 BEGIN 2222 WHILE 3333 REPEAT 4444 ;
ok SEE T
082B 8108 COLON
082D 8230 LIT
082F 1111 1111 
0831 8230 LIT   <------+  begin
0833 2222 2222         |
0835 8B14 0BR   ---+ While Cond. branch over While-repeat block
0837 0841 0841     |   |
0839 8230 LIT      |   |
083B 3333 3333     |   |
083D 8B6D JUMP     |   | Repeat: jump to begin
083F 0831 0831  ---|---+
0841 8230 LIT   <--+
0843 4444 4444 
0845 8125 EXIT
ok 
```

# DO Loops

## DO LOOP

```
ok : T BEFORE DO INSIDE LOOP AFTER ;
ok SEE T
02F3 81FE COLON
02F5 02B7 BEFORE
02F7 8B6B *DO              Initializes DO/LOOP control flow
02F9 02D2 INSIDE   <---+
02FB 8B90 *LOOP        | *LOOP branches back to start of loop code
02FD 02F9 02F9     ----+ 
02FF 02C4 AFTER
0301 821B EXIT
ok
```

## DO +LOOP

```
ok : T BEFORE DO INSIDE 4 +LOOP AFTER ;
ok SEE T
0321 81FE COLON
0323 02B7 BEFORE
0325 8B6B *DO            Initializes DO/LOOP control flow
0327 02D2 INSIDE   <---+
0329 8327 LIT          |
032B 0004 0004         |
032D 8BCB *+LOOP       | *+LOOP branches back to start of loop code
032F 0327 0327     ----+
0331 02C4 AFTER
0333 821B EXIT
```

## ?DO LOOP

```
ok : T BEFORE ?DO INSIDE LOOP AFTER ;
ok SEE T 
033A 81FE COLON
033C 02B7 BEFORE
033E 8797 *?DO   ---+    Conditional branch over the ?DO/LOOP 
0340 034A 034A      |
0342 8B6B *DO       |    Initializes DO/LOOP control flow
0344 02D2 INSIDE <--|--+
0346 8B90 *LOOP     |  |
0348 0344 0344   ---|--+ *LOOP branches back to start of the loop code 
034A 02C4 AFTER  <--+
034C 821B EXIT
ok
```

## ?DO +LOOP

```
ok : T BEFORE ?DO INSIDE 4 +LOOP AFTER ;
ok SEE T 
0353 81FE COLON
0355 02B7 BEFORE
0357 8797 *?DO   ---+    Conditional branch over the ?DO/LOOP  
0359 0367 0367      |
035B 8B6B *DO       |    Initializes DO/LOOP control flow
035D 02D2 INSIDE <--|--+
035F 8327 LIT       |  |
0361 0004 0004      |  |
0363 8BCB *+LOOP    |  | *+LOOP branches back to start of loop code 
0365 035D 035D   ---|--+
0367 02C4 AFTER  <--+
0369 821B EXIT
ok
```

# Defining words

## CREATE example

First we decompile CREATE. Then we use CREATE to define a new variable MYVAR, and we commit two arbitrary values into the dictionary: 00FF and AA55. Then we decompile MYVAR.

```
ok SEE CREATE
855E 8170 COLON
8560 8525 CODE
8562 84D2 *COMMIT_JMP 	; Compiles a JMP ($4C)
8564 826F COMPILE
8566 8170 COLON
8568 826F COMPILE
856A 8543 8543 
856C 826F COMPILE
856E 8572 8572 
8570 8394 REVEAL
8572 818D EXIT    <---+
                      |
ok CREATE MYVAR       |
ok 00FF , AA55 ,      |
                      |
ok SEE MYVAR          |
084B 8170 COLON       |
084D 8543 *CREATED    | *CREATED will jump to this Addr (EXIT)
084F 8572 8572     ---+
0851 00FF 00FF        \
0853 AA55 AA55        / Data cells
```

Notice how by default, the action exectuted by *CREATED will be to jump to EXIT. In the next [CREATE DOES> example](#create-does-example) we'll see how `DOES>` can change this default behaviour.

## CREATE DOES> example

Let's look at how `VALUE` and a *`VALUE` defined* word `FIVE` are both compiled into the dictionary:

`: VALUE CREATE , DOES> @ ;`

```
ok SEE VALUE
06F5 8170 COLON
06F7 855D CREATE
06F9 8BF4 ,
06FB 857C DOES>
06FD 8D1A @       <---+  (DOES> clause)
06FF 818D EXIT        |
ok                    |
ok 5 VALUE FIVE       |
ok                    |
ok SEE FIVE           |
0862 8170 COLON       |
0864 8543 *CREATED    | *CREATED will jump to this Addr (DOES> clause)
0866 06FD 06FD     ---+
0868 0005 0005     <---  PFA slot, right after the DOES> clause addr.
ok 
```

`*CREATED` will leave the address of the PFA (`0868`) on the ToS, then jump to the `DOES>` clause. The `DOES>` clause of `VALUE` will then fetch the value at the address, effectively leaving `0005` on the stack

Compared to the previous example (MYVAR), notice how `DOES>` has patched the cell after `*CREATED` and replaced the address of `EXIT` with the address of the `DOES>` clause.


```
```


```
```




