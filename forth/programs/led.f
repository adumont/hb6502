: PORTA 6001 ;
: DDRA 6003 ;

DDRA C@ 1 OR DDRA C!
: off PORTA C@ FE AND PORTA C! ;
: on PORTA C@ 1 OR PORTA C! ;
: t PORTA C@ 1 XOR PORTA C! ;

: WAIT 0 DO LOOP ;

: BLINK 10 0 DO t 90 WAIT LOOP ;
