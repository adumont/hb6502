\ MOD ( n1 n2 -- mod ) returns n1 modulo n2
: MOD 0 SWAP UM/MOD DROP ;

\ FACT? ( n1 n2 -- bool ) True if n2 is factor of n1
: FACT? MOD 0= ;

\ .FACT ( n1 n2 -- ) Print n2 if it's a factor of n1
: .FACT SWAP OVER FACT? IF . ELSE DROP THEN ;

\ FACTORS ( n -- ) Print all factors of n
: FACTORS DUP 1+ 2 DO DUP I .FACT LOOP DROP ;


