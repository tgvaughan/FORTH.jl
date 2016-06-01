\ Basic definitions

: / /MOD SWAP DROP ;
: MOD /MOD DROP ;
: */ -ROT * SWAP / ;

: NEGATE \ ( x -- -x )
    0 SWAP - ;

: NIP \ ( x y -- y )
    SWAP DROP ;

: TUCK \ ( x y -- y x y )
    DUP -ROT ;

: PICK \ ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
        1+ PSP@ SWAP - @ ;

: TRUE -1 ;
: FALSE 0 ;
: NOT 0= ;

\ Standard words for manipulating BASE.
: DECIMAL   10 BASE ! ;
: HEX       16 BASE ! ;


\ Translate a number of cells into memory units
\ (in our case 1 cell = 1 memory unit)
: CELLS ;

\ Since the smallest unit of memory in our system is 64 bits and since strings
\ are stored as arrays of 64 bit integers, the character store/fetch words are
\ just aliases of the standard store/fetch words.
: C! ! ;
: C@ @ ;
: C, , ;

\ Retrieve stack depth
: DEPTH PSP@ PSP0 - ;

\ Words for whitespace
: '\n' 10 ;
: BL 32 ;
: CR '\n' emit ;
: SPACE BL emit ;

\ CFA retrieval
: ' BL WORD FIND DROP ;
: ['] IMMEDIATE
     LIT LIT , ' , ;

\ Compilation

: [COMPILE] IMMEDIATE ' , ;
: COMPILE IMMEDIATE
    LIT LIT , ' , LIT , , ;

: LITERAL IMMEDIATE ['] LIT , , ;

: CHAR BL WORD 1+ @ ;
: [CHAR] IMMEDIATE
    CHAR
    ['] LIT , ,
;

\ LFA of most recent definition
: LATEST
    CURRENT @ ;

\ Compile in recursive call to current word
: RECURSE IMMEDIATE
        LATEST @        \ LATEST points to the word being compiled at the moment
        LINK>           \ get the codeword
        ,               \ compile it
;

