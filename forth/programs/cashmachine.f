DEC

VARIABLE BILL \ total amount owed by the customer
VARIABLE CASH \ total amount handed by the customer
VARIABLE %DCT \ eventual discount to apply

\ Print integer as $ currency xyzt -> $xy.zt
: .$ DUP $8000 AND IF $2D EMIT NEG THEN
100 /MOD .( $) U. .( .) DUP 10 U< IF $30 EMIT THEN . SPACE ;

\ Money-level Operations

: OWED BILL +! ;
: DISCOUNT %DCT ! ;
: PAID CASH +! ;

\ Words to handle calculations

\ Leave discounted amount on ToS
: $DCT ( -- n ) BILL @ %DCT @ UM* 100 UM/MOD NIP ;

\ Leave total owed (after applying discount)
: TOTAL ( -- n ) BILL @ $DCT - ;

\ Computes balance (positive: owed by customer)
: BALANCE ( -- n ) TOTAL CASH @ - ;

\ Words to print individual quantities

: .TOTAL   .( TOTAL: ) TOTAL .$ CR ;
: .BALANCE .( BALANCE: ) BALANCE .$ CR ;
: .BILL .( BILL: ) BILL @ .$ CR ;
: .CASH .( CASH: ) CASH @ .$ CR ;

: .%DCT 
  %DCT @ ?DUP IF
    .( %DCT: ) U. .( % ) $DCT .$ CR
  THEN
;

\ Status that shows all quantities

: STATUS 
  .BILL
  .%DCT
  .TOTAL
  .CASH
  .BALANCE
;

\ Dispense an amount back to user

: DISPENSE ( n -- )
  2000 /MOD ?DUP IF .( $20.00 x ) . CR THEN
  1000 /MOD ?DUP IF .( $10.00 x ) . CR THEN
   500 /MOD ?DUP IF .( $ 5.00 x ) . CR THEN
   100 /MOD ?DUP IF .( $ 1.00 x ) . CR THEN
    25 /MOD ?DUP IF .( $ 0.25 x ) . CR THEN
    10 /MOD ?DUP IF .( $ 0.10 x ) . CR THEN
     5 /MOD ?DUP IF .( $ 0.05 x ) . CR THEN
            ?DUP IF .( $ 0.01 x ) . CR THEN
;

\ Transaction-level Operations

: NEW
  0 BILL !
  0 %DCT !
  0 CASH !
  .( New transaction starts.) CR
;

: CANCEL 
  CASH @
  ?DUP IF
    .( Dispensing ) .CASH
    DISPENSE 0 CASH !
  ELSE
    .( Nothing to dispense) CR
  THEN
  \ Start new transacion
  CR
  NEW
;

: END
  BALANCE
  ?DUP IF 
    DUP 0< IF
      NEG DUP
      .( Dispense CHANGE: ) .$ CR
      DISPENSE
      NEW
    ELSE
      .( ERROR: Customer ows ) .$ CR
    THEN
  ELSE
    NEW
  THEN
;

\ Handy shotcuts to operate the Cash Register

: ++ OWED CR STATUS ;
: -- PAID CR STATUS ;
: ?? STATUS ;
: // END CR STATUS ;
: ** CANCEL CR STATUS ;
: %% DISCOUNT CR STATUS ;

: HELP
  .( Cash Register Machine in AlexFORTH 6502) CR
  .( n ++  : Add to BILL) CR
  .( n %%  : Apply n% discount to BILL) CR
  .( n --  : Add to CASH) CR
  .(   //  : End transaction) CR
  .(   **  : Cancel transaction) CR
  .(   ??  : Print status) CR
;

