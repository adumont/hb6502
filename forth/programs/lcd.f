: PORTB 6000 ;
: PORTA 6001 ;
: DDRB 6002 ;
: DDRA 6003 ;

: LCD_E 80 ;
: LCD_RW 40 ;
: LCD_RS 20 ;

: LCDWAIT 0 DDRB C!
BEGIN
LCD_RW PORTA C!
LCD_RW LCD_E OR PORTA C!
PORTB C@ 80 AND
0= UNTIL
LCD_RW PORTA C!
FF DDRB C! ;

: LCDINSTR
LCDWAIT
PORTB C!
0 PORTA C!
LCD_E PORTA C!
0 PORTA C! ;

: L1 80 LCDINSTR ;
: L2 C0 LCDINSTR ;
: L3 94 LCDINSTR ;
: L4 D4 LCDINSTR ;

: LCDEMIT
LCDWAIT
PORTB C!
LCD_RS PORTA C!
LCD_RS LCD_E OR PORTA C!
LCD_RS PORTA C! ;

: LCDINIT
FF DDRB C!
DDRA C@ E0 OR DDRA C!
38 LCDINSTR
1 LCDINSTR
2 LCDINSTR
E LCDINSTR
6 LCDINSTR ;

: LCDPRT
0 DO
DUP C@ LCDEMIT
1+
LOOP DROP ;

LCDINIT

L1 S( Alex FORTH v0) LCDPRT
L2 S( Forth2020 Meeting) LCDPRT
L3 S( Dic, 11 2021 ) LCDPRT
L4
