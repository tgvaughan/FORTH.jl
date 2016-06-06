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

( Compile-mode word which compiles everything until the next
  double quote as a litstring. )
: S" IMMEDIATE          ( -- addr len )
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
;

( Compile-mode word which compiles everything until the
  next double quote as a litstring and appends a TYPE. )
: ." IMMEDIATE
        [COMPILE] S"
        ['] TYPE ,
;

( Interpret-mode word which prints everything until the next
  right-paren to the terminal. )
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

: ABORT" IMMEDIATE
        [COMPILE] S"

        ['] rot ,
        [COMPILE] if
                s" Aborted: " ['] lit , , ['] lit , , ['] swap ,
                ['] type ,
                ['] type ,
                ['] cr ,
                ['] abort ,
        [COMPILE] else
                ['] 2drop ,
        [COMPILE] then
;
