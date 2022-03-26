# AlexFORTH Examples

- [AlexFORTH Examples](#alexforth-examples)
  - [Stack dump](#stack-dump)
  - [Strings](#strings)
  - [User input](#user-input)
  - [FORTH Decompiler](#forth-decompiler)
  - [Debugging & Tracing](#debugging--tracing)
  - [Exceptions (THROW / CATCH)](#exceptions-throw--catch)
  - [Free Memory](#free-memory)

This page collects some random unclassified examples and code snippets of AlexFORTH code, and shows how to do some advanced things.

## Stack dump

`SP@` retuns the Data Stack pointer (ToS). `SP@ @` would be equivalent to `DUP`.

`DP` is the end (top, as my stack grows downward) of the memory range of the stack.

```forth
\ Dump stack as memory dump
: SDUMP SP@ DP DUMP ;
```

Example:

We put 1, 2 and 3 on the stack. `SDUMP` will dump it as hex bytes.

```
ok 1 2 3
ok SDUMP
00F0 03 00 02 00 01 00 ok 
```


## Strings

### Store strings in the dictionary

The defining word `STRING` gives the string a name in the dictionary.and commits the string in the dictionary.

The runtime behavior leaves the addr of a counted string on the stack suitable to store in an cells array for example. After eventually use COUNT TYPE to print it.

```
: STRING CREATE HERE -ROT 1+ DUP ALLOT SWAP 1 - -ROT CMOVE ;
```

Example:

```
S( ABCDEF) STRING TEST
TEST COUNT TYPE CR
ABCDEF
ok
```

## User input

### CHAR

`CHAR` is immediate, it relies on `KEY` and reads the next char, and commit it to the dictionary after a `CLIT` (when in compile mode). Interpret semantics is like `KEY`.

```
: CHAR ?EXEC IF KEY ELSE LIT, CLIT KEY C, THEN ; IMMEDIATE
```

### Ask for alfanumeric strings

```forth
\ Allocate a string buffer, 32 chars
VARIABLE STR 20 ALLOT

: PLACE ( SRC CNT DST )
  \ Copy string to DST
  2DUP C!
  1+ SWAP CMOVE
;

: ASK$ ( ADDR -- )
  \ store the counted string to ADDR
  0A PARSE ROT PLACE
;

\ Example 1: Ask and store the string in STR
STR ASK$

: TEST$
  3 0 DO
    \ Show question
    .( What's your name? )
    \ Ask the answer and store it in STR
    STR ASK$ 
    \ Reply with a greeting :)
    .( Hello )
    STR COUNT TYPE CR
  LOOP
;
```

### Ask for numbers

```forth
: ASK# ( -- N ) \ returns the number on the stack
  0A PARSE  ( ADDR LEN )
  NUMBER    ( N )
;

: TEST#
  3 0 DO
    \ Show question
    .( What's your age? )
    \ Ask for the answer and return the number on the stack
    ASK#
    \ Reply using the number
    .( Your age is ) . CR
  LOOP
;
```

## FORTH Decompiler

With this words you can decompile colon words. Not all cases are supported (strings literal for example are not supported. Words not finished with EXIT will likely not work).

```forth
\ Finds the Header of an XT in the dictionary (decompile)
: >HDR ( XT -- HDR/0 ) \ 0 if not found
  LATEST
  BEGIN
    @ DUP
    DUP
    IF
      >CFA
      2 PICK
      =
    ELSE
      0= \ force exit leaving FALSE on ToS
    THEN
  UNTIL
  NIP
;

\ Print the name or the ADDR if not found
: >NAME ( XT -- )
  DUP >HDR
  DUP IF
    .NAME 2DROP
  ELSE
    DROP .
  THEN
;

\ Call with SEE WORDNAME , for example SEE T
: SEE
  2 LOCALS \ x is pointer to XT, y is XT
  WORD FIND >CFA \ get CFA of name to decompile
  \  x . S( : ) TYPE x >NAME CR
  1+ x! \ skip 4C JMP  // TODO: what if not a colon word??
  BEGIN
    x .
    x @ y!
    y .   \ // TODO handle LITSTR (dump str and skip over it)

    \\ what if it is a JUMP? should we follow it? (I think yes)

    y >NAME CR
    x 2+ x!
    y LIT EXIT =
  UNTIL
  -LOCALS
;
```

## Debugging & Tracing

### Approach 1

In this approach: 
- I declare a defered word `(TRACE)`
- I save the original `:` word to `OLD:`
- Define a new `TRACE` word (using the `OLD:` colon definition)
- Define `(TRACE)`to run `TRACE`
- Write a new definition of `:` that will call `(TRACE)`

By using a defered word for `(TRACE)`, we can redefine it at any time and it will apply to any word that were defined using the new `:` definition.

Notice: In this approach, after having defined our new `:`, if we ever want to define another tracing word, we MUST use `OLD:` instead of `:` to avoid an infinite loop.

In this approach, by default all new words defined using the new `:` will be subject to tracing. If we want to avoid a word being traced we need to define it with `OLD:`.

We must have defined the words `>HDR` and `>NAME` (see above in this page).

```forth
DEFER (TRACE)

: OLD: : ;

OLD: TRACE 103 RP@ + @ 5 - >NAME SPACE .S CR ;

' TRACE IS (TRACE)

\ Rewrite : to call (TRACE)
: : CODE 4C C, LIT, COLON LIT, (TRACE) ] ;
```

How to use:

Once the new `:` is defined, there's nothing to do, all new word will be traced.

```
ok : TEST S( Hello) TYPE CR ;
ok 1 2 3 TEST
TEST 0001 0002 0003 

Hello
ok 
```

Nested traced calls:

```
: TEST2 
    S( TEST2 calls TEST) TYPE CR 
    TEST
;
ok TEST2
TEST2 0001 0002 0003 

TEST2 calls TEST
TEST 0001 0002 0003 

Hello
```

### Approach 2

In this approach: 
- We declare a defered word `(TRACE)`
- Define a new `TRACE` word
- Define `(TRACE)` to run `TRACE`
- Write a new defining word `T:` that we will use when we want to define a traced colon word.

By using a defered word for `(TRACE)`, we can redefine it at any time and it will apply to any word that were defined using the new `:` definition.

In this approach, only words defined using the new `T:` will be subject to tracing.

We must have defined the words `>HDR` and `>NAME` (see above in this page).

```forth
DEFER (TRACE)

: TRACE 103 RP@ + @ 5 - >NAME SPACE .S CR ;

' TRACE IS (TRACE)

\ T: is an alternative : that will call (TRACE)
: T: CODE 4C C, LIT, COLON LIT, (TRACE) ] ;
```

How to use:

We use `T:` to indicate we want to trace this word

```forth
T: TEST S( Hello) TYPE CR ;
```

Execution:

The trace show the called word (`TEST`) and the stack when entering `TEST`, then it executes `TEST` showing "Hello".

```
ok TEST
TEST 0001 0002 0003 
Hello
ok
```

### Enabling/Disabling the tracing

We can enhance the above mechanism using a flag `?TRACE`. (0 will disable trace, any other value will enable it).

```forth
1 VALUE ?TRACE
```

And we add the logic into our `TRACE`:

```forth
: TRACE
  ?TRACE IF 
    103 RP@ + @ 5 - >NAME SPACE .S CR
  THEN
;
' TRACE IS (TRACE)
```

## Exceptions (THROW / CATCH)

We can implement [CATCH](https://forth-standard.org/standard/exception/CATCH) and [THROW](https://forth-standard.org/standard/exception/THROW):

Interesting article about [How Forth implements exceptions](https://niedzejkob.p4.team/bootstrap/throw-catch/).

```
VARIABLE HANDLER 0 HANDLER ! \ last exception handler

: CATCH ( xt -- exception# | 0 )   \ return addr on stack
   SP@ >R             ( xt )       \ save data stack pointer
   HANDLER @ >R       ( xt )       \ and previous handler
   RP@ HANDLER !      ( xt )       \ set current handler
   EXEC               ( )          \ execute returns if no THROW
   R> HANDLER !       ( )          \ restore previous handler
   R> DROP            ( )          \ discard saved stack ptr
   0                  ( 0 )        \ normal completion
;

: THROW ( ??? exception# -- ??? exception# )
    ?DUP IF           ( exc# )     \ 0 THROW is no-op
      HANDLER @ RP!   ( exc# )     \ restore prev return stack
      R> HANDLER !    ( exc# )     \ restore prev handler
      R> SWAP >R      ( saved-sp ) \ exc# on return stack
      SP! DROP R>     ( exc# )     \ restore stack
      \ Return to the caller of CATCH because return
      \ stack is restored to the state that existed
      \ when CATCH began execution
    THEN
;

\ this word throws an expetion 12
: TEST-THROW 1 2 3 4 + . 12 THROW ;

\ this word calls the previous word eventually catching an exception
: TEST-CATCH LIT TEST-THROW CATCH ;

TEST-CATCH
```

## Free Memory

`FREE` will print the available memory (between the upper end of dictionary and the lower end of the local variable stack)

```
: FREE BP @ 2+ HERE - DEC. .( BYTES FREE) CR ;
```
