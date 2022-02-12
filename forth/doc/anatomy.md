# Anatomy of compiled words

- [Anatomy of compiled words](#anatomy-of-compiled-words)
- [Conditionals](#conditionals)
  - [IF THEN](#if-then)
  - [IF ELSE THEN](#if-else-then)
- [Loops](#loops)
  - [BEGIN AGAIN](#begin-again)
  - [BEGIN UNTIL](#begin-until)

The purpose of this page is to document the anatomy of compiled words. I'm using the word decompiler SEE and I have added some comments and arrow to illustrate the execution thread.

The output of `SEE` shows 3 columns:
- the address in the code
- the content (usually the XT/CFA of a word), or another address to jump to. It can also be a literal
- the name of the word if found in the dictionary, the content otherwise.

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

# Loops

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
