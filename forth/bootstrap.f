\ DON'T REMOVE THIS CODE block
>ROM
HERE 1 !        \ we save the start of ROM area at 0001
>RAM
HERE 5 !        \ we save the start of RAM area at 0005
>ROM
\ ----------------------------

: ? @ . ;
: = - 0= ;
: NEG NOT 1+ ; \ ( N -- -N ) Negate N (returns -N)
: 0< 8000 AND ; \ ( N -- F ) Is N strictly negative? Returns non 0 (~true) if N<0
: IMMEDIATE LAST SETIMM ; \	; sets the latest word IMMEDIATE
: ' WORD FIND >CFA ;
: [,] , ; IMMEDIATE 

\ Comparison operators ( a b -- f )
: < - 0< ;
: <= < 0= ;
: > SWAP < ;
: >= SWAP <= ;

\ LIT, is an alias for COMPILE, it's shorter ;)
: IF LIT, 0BR HERE HERE++ ; IMMEDIATE
: THEN HERE SWAP ! ; IMMEDIATE
: ELSE LIT, JUMP HERE HERE++ SWAP HERE SWAP ! ; IMMEDIATE

: BEGIN HERE ; IMMEDIATE
: AGAIN LIT, JUMP , ; IMMEDIATE

: UNTIL LIT, 0BR , ; IMMEDIATE

: ( BEGIN KEY 29 = UNTIL ; IMMEDIATE \ ; now we can use ( ) inline comments!

: WHILE LIT, 0BR HERE HERE++ ; IMMEDIATE
: REPEAT LIT, JUMP SWAP , HERE SWAP ! ; IMMEDIATE

: >D DUP 0< 0= 0= ; \ ; Extends signed cell into signed double (0= 0= will convert any non 0 into FFFF)

: UM+ 0 SWAP 0 D+ ;

: NIP SWAP DROP ;
: PICK 1+ 2* SP@ + @ ;
: DEPTH DTOP SP@ - 2/ ;
: .S DEPTH DUP IF 1+ DUP 1 DO DUP I - PICK . LOOP CR THEN DROP ;

: 2SWAP >R -ROT R> -ROT ;
: 2ROT >R >R 2SWAP R> R> 2SWAP ;
: 2>R R> -ROT SWAP >R >R >R ;
: 2R> R> R> R> SWAP ROT >R ;

: DU< D- NIP 0< ;
: M+ >D D+ ;

: DNEG SWAP NOT SWAP NOT 1 0 D+ ;

: S. DUP 0< IF 2D EMIT NEG THEN . ;
: DS. DUP 0< IF 2D EMIT DNEG THEN D. ;

: * UM* DROP ;

>RAM

VARIABLE BP \ ; defines BP variable (Base Pointer)
_BP BP !

>ROM

: LOCALS  BP @ DUP ROT 2* - DUP -ROT ! BP ! ;
: -LOCALS BP DUP @ @ SWAP ! ;   \ ( n -- ) Dellocates n local variables

: L@ BP @ + @ ; \  ; ( n -- value) helper word to get local var n
: L! BP @ + ! ; \  ; ( n -- value) helper word to save to local var n

: x 2 L@ ; : x! 2 L! ;
: y 4 L@ ; : y! 4 L! ;
: z 6 L@ ; : z! 6 L! ;
: t 8 L@ ; : t! 8 L! ;


: RECURSIVE REVEAL ; IMMEDIATE

: DUMP SWAP DUP . DO I C@ C. LOOP ; \ ( addr1 addr2 -- ) dumps memory from addr1 to addr2

: .NAME DUP 2+ DUP C@ DUP 40 AND >R 1F AND SWAP 
  1+ SWAP TYPE R> IF SPACE 2A EMIT THEN ;
: WORDS 0 LATEST BEGIN @ DUP WHILE DUP . DUP >CFA . 
  .NAME CR SWAP 1+ DUP 10 = IF GETC 20 OR 71 = 
  IF 2DROP EXIT THEN DROP 0 THEN SWAP REPEAT 2DROP ;

: VALUE CREATE , DOES> @ ;
: CONSTANT VALUE ;
: TO ' 7 + ?EXEC IF ! ELSE LIT, LIT , LIT, ! THEN ; IMMEDIATE
: DEFER CREATE 0 , DOES> @ EXEC ;
: POSTPONE ' , ; IMMEDIATE
: IS POSTPONE TO ; IMMEDIATE

: .( [ ' S( , ] ?EXEC IF TYPE ELSE LIT, TYPE THEN ; IMMEDIATE

: FREE BP @ 2+ HERE - DEC. .( BYTES FREE) CR ;

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

\ Base Numbers

\ Only positive numbers ( 0 instead of >D for signed doubles... problem with FFFF)
: /MOD SWAP 0 ROT UM/MOD ;

: MOD /MOD DROP ;
: / /MOD NIP ;

: U. ( u -- )
   RECURSIVE
   \ BREAK
   BASE @ /MOD    ( width rem quot )
   ?DUP IF        ( if quotient <> 0 then )
      U.          ( print the quotient )
   THEN

   ( print the remainder )
   DUP A < IF
      30    ( decimal digits 0..9 )
   ELSE
      A -    ( hex and beyond digits A..Z )
      41
   THEN
   +
   EMIT
;

\ Redefine .S with BASE support (only positive numbers for now! )
: .S
  10 BASE @ = IF
    .S
  ELSE
    DEPTH DUP IF 1+ DUP 1 DO DUP I - PICK U. SPACE LOOP CR THEN DROP
  THEN
;

\ redefine . with BASE support (only positive numbers for now! )
: .
  10 BASE @ = IF
    .
  ELSE
    U. SPACE
  THEN
;

MARKER
\ PRMP

\ DON'T REMOVE THIS CODE block
>ROM
HERE 3 !    \ we save the end of ROM area at 0003
>RAM
HERE 7 !    \ we save the end of RAM area at 0007

LAST 9 !    \ we save LAST at 0009

1 0 C!      \ we signal the end of bootstrap compilation by saving 1 in 0000
