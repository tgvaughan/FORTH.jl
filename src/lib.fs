
: / /MOD SWAP DROP ;
: MOD /MOD DROP ;

: '\n' 10 ;
: BL 32 ;

: CR '\n' emit ;
: SPACE BL emit ;

: NEGATE 0 swap - ;

: TRUE -1 ;
: FALSE 0 ;
: NOT 0= ;

: LITERAL IMMEDIATE ' LIT , , ;

: ':' [ CHAR : ] LITERAL ;

: ';' [ CHAR ; ] LITERAL ;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '"' [ CHAR " ] LITERAL ;
: 'A' [ CHAR A ] LITERAL ;
: '0' [ CHAR 0 ] LITERAL ;
: '-' [ CHAR - ] LITERAL ;
: '.' [ CHAR . ] LITERAL ;

