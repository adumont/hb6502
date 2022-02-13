# Anatomy of compiled words

- [Anatomy of compiled words](#anatomy-of-compiled-words)
- [Conditionals](#conditionals)
  - [IF THEN](#if-then)
  - [IF ELSE THEN](#if-else-then)
- [BEGIN Loops](#begin-loops)
  - [BEGIN AGAIN](#begin-again)
  - [BEGIN UNTIL](#begin-until)
  - [BEGIN WHILE REPEAT](#begin-while-repeat)
- [DO Loops](#do-loops)
  - [DO LOOP](#do-loop)
  - [?DO LOOP](#do-loop-1)
- [Defining words](#defining-words)
  - [CREATE example](#create-example)
  - [CREATE DOES> example](#create-does-example)

The purpose of this page is to document the anatomy of compiled words in AlexFORTH. I'm using my [FORTH Decompiler](https://github.com/adumont/hb6502/blob/main/forth/doc/examples.md#forth-decompiler) word `SEE` and I have added some comments and arrows to illustrate the execution flow inside the words.

The output of `SEE` produces 3 columns:
- The first column shows the address in the code
- The second column is the actual content of the cell. Usually it's the XT/CFA of a word, or another address to jump to. It can also be a literal.
- The last colunm shows the name of the word -- if found in the dictionary--, or the content again otherwise.

```
ADDR VALUE NAME
0860 082F  HEAD
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
073B 8B14 0BR     ----+   Conditional branching back to BEGIN
073D 072D 072D
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
083D 8B6D JUMP  ---|---+  Repeat: jump to begin
083F 0831 0831     |
0841 8230 LIT   <--+
0843 4444 4444 
0845 8125 EXIT
ok 
```

# DO Loops

## DO LOOP

```
ok : T 1111 -ROT DO I . CR LOOP 2222 ;
ok SEE T
07E5 8108 COLON
07E7 8230 LIT
07E9 1111 1111
07EB 8BF3 -ROT
07ED 8A6D *DO           *DO initializes DO/LOOP control flow
07EF 8292 I       <---+
07F1 8C32 .           |
07F3 8A23 CR          |
07F5 819B 1           |
07F7 8A90 *LOOP   ----+ *LOOP branches back to start of loop code
07F9 07EF 07EF 
07FB 8230 LIT
07FD 2222 2222
07FF 8125 EXIT
ok 
```

## ?DO LOOP

```
ok : T 1111 -ROT ?DO I . CR LOOP 2222 ;
ok SEE T 
0806 8108 COLON
0808 8230 LIT
080A 1111 1111 
080C 8BF3 -ROT
080E 8699 *?DO  --+     Conditional branch over the ?DO/LOOP
0810 0820 0820    |
0812 8A6D *DO     |     Initializes DO/LOOP control flow
0814 8292 I     <-|---+
0816 8C32 .       |   |
0818 8A23 CR      |   |
081A 819B 1       |   |
081C 8A90 *LOOP --|---+ *LOOP branches back to start of the loop code
081E 0814 0814    |
0820 8230 LIT   <-+ 
0822 2222 2222 
0824 8125 EXIT
ok
```

# Defining words

## CREATE example

First we decompile CREATE. Then we use CREATE to define a new variable MYVAR, and we commit two arbitrary values into the dictionary: 00FF and AA55. Then we decompile MYVAR.

```
ok SEE CREATE
855E 8170 COLON
8560 8525 CODE
8562 84D2 84D2 
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

Notice how by defaul

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




