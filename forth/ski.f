\ \\\\\\\\\\\\\\\\\\\\\\\ SKI CALCULUS EXTENSION

\ SKI Calculus in AlexForth
\ Tested in AlexForth for 6502
\ (C) 2023 Alexandre Dumont <adumont@gmail.com>
\ SPDX-License-Identifier: GPL-3.0-only

\ ENTER, places a empty header in the dictionary
\ $4C is 6502's JMP
: ENTER, ( -- ) 4C C, COMPILE COLON ;

\ Create a Function (or combinator)
\ Combinators are Higher Order Functions, meaning
\ they take a function as an Argument and return a function
\ which we will eventually apply later using ")"
: :FUNC ( "name" -- ) CREATE ENTER, ] ;


\ Application operator
: )   ( xt -- xt ) EXEC  ; \ Apply <=> "Application"

\ Syntactic sugar definitions
: ))  ( xt -- xt ) ) )   ;
: ))) ( xt -- xt ) ) ) ) ;


\ Here we redefine I,
\ no problem we can always use R> in DO LOOPs...

\ Identity Combinator
\ Ix=x    λx.x
:FUNC I ;

\ Constant Combinator, aka Kestrel
\ Kxy=x   λxy.x
:FUNC K
  HERE \ leaves the XT of the :NONAME word on the stack
  \ now we compile the :NONAME word
  ENTER,
  COMPILE DROP     \ Drop X
  COMPILE LIT      \ Push Y onto the stack
  SWAP \ put Y back on TOS
  ,  \ store Y into the definition
  COMPILE EXIT
;

\ Kite Combinator
\ KIxy=y    λxy.y
I K )   CONSTANT   KI

\ Cardinal combinator
\ λfab.fba  Cfab=fba
:FUNC C ( f -- CF )
  HERE
  ENTER,
  COMPILE HERE
  COMPILE ENTER,
  COMPILE COMPILE COMPILE LIT
  COMPILE SWAP
  COMPILE ,   \ stores a
  COMPILE COMPILE COMPILE SWAP
  COMPILE COMPILE COMPILE LIT
  COMPILE LIT
  SWAP
  ,
  COMPILE ,
  COMPILE COMPILE COMPILE ))
  COMPILE COMPILE COMPILE EXIT
  COMPILE EXIT
;

\ S combinator
\ λxyz.xz(yz)  Sxyz = xz(yz)
:FUNC S
  HERE
  ENTER,
  COMPILE HERE
  COMPILE ENTER,
  COMPILE SWAP
  COMPILE COMPILE COMPILE DUP
  COMPILE COMPILE COMPILE LIT \ y
  COMPILE , \ y
  COMPILE COMPILE COMPILE )
  COMPILE COMPILE COMPILE SWAP
  COMPILE COMPILE COMPILE LIT
  COMPILE LIT \ x
  SWAP
  , \ store x
  COMPILE ,
  COMPILE COMPILE COMPILE ))
  COMPILE COMPILE COMPILE EXIT
  COMPILE EXIT
;

\ Hack: we define those two functions so we can check results of boolean operations
:FUNC .T .( TRUE )  ;
:FUNC .F .( FALSE ) ;

: BOOL .F .T ;

\ BOOLEANS
K  CONSTANT T    \ TRUE  λxy.x
KI CONSTANT F    \ FALSE λxy.y

\ Test with:
\ .F .T K  )))  --> TRUE
\ .F .T KI )))  --> FALSE

\ We define the INCR function so we
\ can check results of church numerals operations
:FUNC INCR 1+ ;
: CN 0 INCR ;

#ifdef BOOTSTRAP_TEST_SKI

K K ) I S K ) S )) K K ) S )) S K ) S )) S ))
CONSTANT NOT

K I S K ) S )) S ))
CONSTANT AND

I K ) I I S )) K K ) S )) S K ) S )) S ))
CONSTANT OR

K K ) K ) K K ) I K ) K ) I S )) S )) K ) K S K ) S )) S )) S K ) S )) S ))
CONSTANT NAND

K K ) I K ) K ) I S )) S )) K ) I K ) K ) K K ) I S )) S )) K ) K S K )
S )) S )) S K ) S )) S ))
CONSTANT XNOR

KI                  CONSTANT ZERO
I                   CONSTANT ONE
I K S K ) S )) S )) CONSTANT TWO



#endif