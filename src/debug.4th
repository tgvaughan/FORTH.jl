: CFA>
        LATEST @
        BEGIN
                ?DUP
        WHILE
                2DUP SWAP
                < IF           
                        NIP  
                        EXIT
                THEN
                @ 
        REPEAT
        DROP
        0
;

: DECIMAL  10 BASE ! ;
: HEX  16 BASE ! ;

: ID.
        1+
        DUP @
        F_LENMASK AND 

        BEGIN
                DUP 0>
        WHILE
                SWAP 1+
                DUP @ EMIT
                SWAP 1-
        REPEAT
        2DROP
;

: name cfa> id. ;

: [trace] immediate
    trace ;

\ : test 20 0 [trace] do 42 emit [trace] loop ;

