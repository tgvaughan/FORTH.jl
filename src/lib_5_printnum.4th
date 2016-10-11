\ Displaying numbers

( Write n spaces to stdout. )
: SPACES        ( n -- )
        DUP 0> IF
            0 DO SPACE LOOP
        ELSE
            DROP
        THEN
;
( This is the underlying recursive definition of U. )
: U.            ( u -- )
        BASE @ /MOD     ( width rem quot )
        ?DUP IF                 ( if quotient <> 0 then )
                RECURSE         ( print the quotient )
        THEN

        ( print the remainder )
        DUP 10 < IF
                [CHAR] 0             ( decimal digits 0..9 )
        ELSE
                10 -            ( hex and beyond digits A..Z )
                [CHAR] A
        THEN
        +
        EMIT
;

( This word returns the width (in characters) of an unsigned number in the current base )
: UWIDTH        ( u -- width )
        BASE @ /        ( rem quot )
        ?DUP IF         ( if quotient <> 0 then )
                RECURSE 1+      ( return 1+recursive call )
        ELSE
                1               ( return 1 )
        THEN
;

: U.R           ( u width -- )
        SWAP            ( width u )
        DUP             ( width u u )
        UWIDTH          ( width u uwidth )
        ROT            ( u uwidth width )
        SWAP -          ( u width-uwidth )
        ( At this point if the requested width is narrower, we'll have a negative number on the stack.
          Otherwise the number on the stack is the number of spaces to print.  But SPACES won't print
          a negative number of spaces anyway, so it's now safe to call SPACES ... )
        SPACES
        ( ... and then call the underlying implementation of U. )
        U.
;

: .R            ( n width -- )
        SWAP            ( width n )
        DUP 0< IF
                NEGATE          ( width u )
                1               ( save a flag to remember that it was negative | width n 1 )
                -ROT             ( 1 width u )
                SWAP            ( 1 u width )
                1-              ( 1 u width-1 )
        ELSE
                0               ( width u 0 )
                -ROT             ( 0 width u )
                SWAP            ( 0 u width )
        THEN
        SWAP            ( flag width u )
        DUP             ( flag width u u )
        UWIDTH          ( flag width u uwidth )
        ROT            ( flag u uwidth width )
        SWAP -          ( flag u width-uwidth )

        SPACES          ( flag u )
        SWAP            ( u flag )

        IF                      ( was it negative? print the - character )
                [CHAR] - EMIT
        THEN

        U.
;

: . 0 .R SPACE ;

: .S            ( -- )
        [CHAR] < EMIT DEPTH U. [CHAR] > EMIT SPACE
        PSP0 1+
        BEGIN
                DUP PSP@ 2 - <=
        WHILE
                DUP @ .
                1+
        REPEAT
        DROP
;

: U. U. SPACE ;

( ? fetches the integer at an address and prints it. )
: ? ( addr -- ) @ . ;

( c a b WITHIN returns true if a <= c and c < b )
: WITHIN
        -ROT             ( b c a )
        OVER            ( b c a c )
        <= IF
                > IF            ( b c -- )
                        TRUE
                ELSE
                        FALSE
                THEN
        ELSE
                2DROP           ( b c -- )
                FALSE
        THEN
;

