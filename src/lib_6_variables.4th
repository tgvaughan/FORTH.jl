\ Constants and Variables

: CONSTANT
        CREATE ,
DOES>   @
;

: ALLOT         ( n -- )
        H +!         ( adds n to H, after this the old value of H is still on the stack )
;

: VARIABLE
        CREATE
        1 CELLS ALLOT   ( allocate 1 cell of memory, push the pointer to this memory )
;

: VALUE         ( n -- )
        CREATE ,
DOES>   @
;

: TO IMMEDIATE  ( n -- )
        BL WORD         ( get the name of the value )
        FIND DROP       ( look it up in the dictionary )
        >BODY           ( get a pointer to the first data field (the 'LIT') )
        STATE @ IF      ( compiling? )
                ['] LIT ,         ( compile LIT )
                ,               ( compile the address of the value )
                ['] ! ,           ( compile ! )
        ELSE            ( immediate mode )
                !               ( update it straightaway )
        THEN
;

( x +TO VAL adds x to VAL )
: +TO IMMEDIATE
        BL WORD         ( get the name of the value )
        FIND DROP       ( look it up in the dictionary )
        >BODY           ( get a pointer to the first data field (the 'LIT') )
        STATE @ IF      ( compiling? )
                ['] LIT ,         ( compile LIT )
                ,               ( compile the address of the value )
                ['] +! ,          ( compile +! )
        ELSE            ( immediate mode )
                +!              ( update it straightaway )
        THEN
;

( Fill u ints, starting at a, with the value b )
: FILL          ( a u b -- )
        -ROT OVER + SWAP ?DO
                DUP I !
        LOOP
        DROP
;

: ERASE         ( a u -- )
        0 FILL
;
