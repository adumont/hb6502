\ \\\\\\\\\\\\\\\\\\\\\\\ SKI CALCULUS EXTENSION

\ SKI Calculus in AlexForth
\ Tested in AlexForth for 6502
\ (C) 2023 Alexandre Dumont <adumont@gmail.com>
\ SPDX-License-Identifier: GPL-3.0-only

\ ENTER, places a empty header in the dictionary
\ $4C is 6502's JMP
: ENTER, ( -- ) 4C C, COMPILE COLON ;

\ Combinators are Higher Order Functions, meaning
\ they take a function as an Argument and return a function
\ which we will eventually apply later using ")"


\ Application operator
: )   ( xt -- xt ) EXEC  ; \ Apply <=> "Application"

\ Syntactic sugar definitions
: ))  ( xt -- xt ) ) )   ;
: ))) ( xt -- xt ) ) ) ) ;


\ Here we redefine I,
\ no problem we can always use R> in DO LOOPs...

\ Identity Combinator
\ Ix=x    λx.x
:NONAME ; CONSTANT I

\ Constant Combinator, aka Kestrel
\ Kxy=x   λxy.x
:NONAME
  HERE \ leaves the XT of the :NONAME word on the stack
  \ now we compile the :NONAME word
  ENTER,
  COMPILE DROP     \ Drop X
  COMPILE LIT      \ Push Y onto the stack
  SWAP \ put Y back on TOS
  ,  \ store Y into the definition
  COMPILE EXIT
; CONSTANT K

\ Kite Combinator
\ KIxy=y    λxy.y
I K )   CONSTANT   KI

\ Cardinal combinator
\ λfab.fba  Cfab=fba
:NONAME ( f -- CF )
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
; CONSTANT C

\ S combinator
\ λxyz.xz(yz)  Sxyz = xz(yz)
:NONAME
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
; CONSTANT S

\ Hack: we define those two functions so we can check results of boolean operations
:NONAME .( TRUE )  ; CONSTANT .T
:NONAME .( FALSE ) ; CONSTANT .F

: BOOL .F .T ;

\ BOOLEANS
K  CONSTANT T    \ TRUE  λxy.x
KI CONSTANT F    \ FALSE λxy.y

\ Test with:
\ .F .T K  )))  --> TRUE
\ .F .T KI )))  --> FALSE

#ifdef BOOTSTRAP_TEST_SKI

\ Boolean operations
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

\ Church Numerals
KI                  CONSTANT ZERO
I                   CONSTANT ONE
I K S K ) S )) S )) CONSTANT TWO

K K ) F K ) K ) I S )) S ))
CONSTANT IS0

K S K ) S )) S )
CONSTANT SUCC

\ We define the INCR function so we
\ can check results of church numerals operations
' 1+ CONSTANT INCR
: CN 0 INCR ;

TWO SUCC ) CONSTANT THREE
TWO TWO  ) CONSTANT FOUR

K S K ) S )) K ) S ) S K ) S ))
CONSTANT ADD

I K ) K S K ) S )) K K ) S )) S K ) S )) S ))
CONSTANT MUL

#endif
