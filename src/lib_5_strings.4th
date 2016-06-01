\ Strings

: CMOVE ( src dest length -- )

        DUP 0<= IF
                DROP DROP DROP
                EXIT
        THEN

        -ROT OVER -         ( length src (dest-src) )
        -ROT DUP ROT + SWAP ( (dest-src) (src+length) src )
    
        DO
                I @         ( (dest-src) i@ )
                OVER I +    ( (dest-src) i@ (dest-src+i) )
                !           ( (dest-src) )
        LOOP

        DROP
;

: CMOVE> ( src dest length -- )
        DUP 0<= IF
                DROP DROP DROP
                EXIT
        THEN

        -ROT OVER -         ( length src (dest-src) )
        -ROT DUP ROT + 1-   ( (dest-src) src (src+length-1) )
        
        DO
                I @
                OVER I +
                !
        -1 +LOOP

        DROP
;

: S" IMMEDIATE          ( -- addr len )
        STATE @ IF      ( compiling? )
                ['] LITSTRING ,   ( compile LITSTRING )
                HERE          ( save the address of the length word on the stack )
                0 ,             ( dummy length - we don't know what it is yet )

                BEGIN
                        >IN @ #TIB @ >= IF      \ End of TIB?
                                QUERY           \ Get next line
                        THEN

                        TIB >IN @ + @ 1 >IN +!  \ Get char from TIB

                        DUP [CHAR] " <>
                WHILE
                        C,              ( copy character )
                REPEAT
                DROP            ( drop the double quote character at the end )
                DUP             ( get the saved address of the length word )
                HERE SWAP -   ( calculate the length )
                1-              ( subtract 1 (because we measured from the start of the length word) )
                SWAP !          ( and back-fill the length location )
        ELSE            ( immediate mode )
                HERE          ( get the start address of the temporary space )
                
                BEGIN
                        >IN @ #TIB @ >= IF      \ End of TIB?
                                QUERY           \ Get next line
                        THEN

                        TIB >IN @ + @ 1 >IN +!  \ Get char from TIB

                        DUP [CHAR] " <>
                WHILE
                        OVER C!         ( save next character )
                        1+              ( increment address )
                REPEAT
                DROP            ( drop the final " character )
                HERE -        ( calculate the length )
                HERE          ( push the start address )
                SWAP            ( addr len )
        THEN
;

: ." IMMEDIATE          ( -- )
        [COMPILE] S"    ( read the string, and compile LITSTRING, etc. )
        ['] TYPE ,      ( compile the final TYPE )
;

: .( 
        BEGIN
                >IN @ #TIB @ >= IF      \ End of TIB?
                        QUERY           \ Get next line
                THEN

                TIB >IN @ + @ 1 >IN +!  \ Get char from TIB

                DUP [CHAR] ) = IF
                        DROP    ( drop the double quote character )
                        EXIT    ( return from this function )
                THEN
                EMIT
        AGAIN
;

( Converts address of counted string into address of
  start of string and length of string. )
: COUNT ( addr1 -- addr2 n )
        DUP 1+ SWAP @ ;


