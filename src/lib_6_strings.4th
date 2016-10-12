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
                >IN @ #IB @ >= IF      \ End of IB?
                        QUERY-INPUT    \ Get next line
                THEN

                IB >IN @ + @ 1 >IN +!  \ Get char from IB

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
                >IN @ #IB @ >= IF      \ End of IB?
                        QUERY-INPUT    \ Get next line
                THEN

                IB >IN @ + @ 1 >IN +!  \ Get char from IB

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

( Compares two strings, returns 0 if identical. )
: COMPARE ( addr1 n1 addr2 n2 -- res )
    rot 2dup <> if
        2drop 2drop 1 exit
    then
    
    drop

    0 do
        2dup i + @ swap i + @ <> if
            unloop 2drop 1 exit
        then
    loop

    2drop 0
;

( Converts a string to lower case. )
: TOLOWER ( addr n -- )
    0 do
        dup i + @ dup dup ( addr char char char )
        [char] A >=
        swap [char] Z <= and if
            [char] A - [char] a +
            over i + !
        else
            drop
        then
    loop

    drop
;

( Abort if flag is true. )
: ABORT" IMMEDIATE  ( flag -- )
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
