\ Decompilation

: .NAME
        DUP @           ( get the flags/length byte )
        F_LENMASK AND   ( mask out the flags - just want the length )

        BEGIN
                DUP 0>          ( length > 0? )
        WHILE
                SWAP 1+         ( addr len -- len addr+1 )
                DUP @           ( len addr -- len addr char | get the next character)
                DUP 32 >= OVER 127 <= AND IF
                        EMIT    ( len addr char -- len addr | and print it)
                ELSE
                        BASE @ SWAP HEX
                        ." \x" 0 .R
                        BASE !
                THEN
                SWAP 1-         ( len addr -- addr len-1    | subtract one from length )
        REPEAT
        2DROP           ( len addr -- )
;

: ?HIDDEN
        1+              ( skip over the link pointer )
        @               ( get the flags/length byte )
        F_HIDDEN AND    ( mask the F_HIDDEN flag and return it (as a truth value) )
;

: ?IMMEDIATE
        1+              ( skip over the link pointer )
        @               ( get the flags/length byte )
        F_IMMED AND     ( mask the F_IMMED flag and return it (as a truth value) )
;

: BODYLEN
        \ **TODO**
;

: SEE
        BL WORD FIND    ( find the dictionary entry to decompile )

        CR

        0= IF
                ." Word '" COUNT TYPE ." ' not found in dictionary."
                EXIT
        THEN

        DUP DUP BODYLEN + SWAP >LINK

        DUP LINK> @ CASE
                DOCOL OF
                        \ Colon definition
                        [CHAR] : EMIT SPACE DUP 1+ .NAME SPACE
                        DUP ?IMMEDIATE IF ." IMMEDIATE " THEN CR
                ENDOF
                DOVAR OF
                        \ Variable definition
                        ." Variable " DUP 1+ .NAME CR
                        2DROP EXIT
                ENDOF
                DOCON OF
                        \ Constant definition
                        ." Constant " DUP 1+ .NAME CR
                        2DROP EXIT
                ENDOF

                \ Unknown codeword
                ." Primitive or word with unrecognized codeword." CR 
                DROP 2DROP EXIT
        ENDCASE

        4 SPACES

        LINK> >BODY            ( get the data address, ie. points after DOCOL | end-of-word start-of-data )

        ( now we start decompiling until we hit the end of the word )
        BEGIN           ( end start )
                2DUP >
        WHILE
                DUP @           ( end start codeword )

                CASE
                ['] LIT OF                ( is it LIT ? )
                        1+ DUP @                ( get next word which is the integer constant )
                        .                       ( and print it )
                ENDOF
                ['] LITSTRING OF          ( is it LITSTRING ? )
                        [CHAR] S EMIT [CHAR] " EMIT SPACE ( print S"<space> )
                        1+ DUP @                ( get the length word )
                        SWAP 1+ SWAP            ( end start+1 length )
                        2DUP TYPE               ( print the string )
                        [CHAR] " EMIT SPACE          ( finish the string with a final quote )
                        +                       ( end start+1+len, aligned )
                        1-                     ( because we're about to add 4 below )
                ENDOF
                ['] 0BRANCH OF            ( is it 0BRANCH ? )
                        ." 0BRANCH ( "
                        1+ DUP @               ( print the offset )
                        .
                        ." ) "
                ENDOF
                ['] BRANCH OF             ( is it BRANCH ? )
                        ." BRANCH ( "
                        1+ DUP @               ( print the offset )
                        .
                        ." ) "
                ENDOF
                ['] ['] OF                  ( is it ['] ? )
                        ." ['] "
                        1+ DUP @               ( get the next codeword )
                        >NAME                    ( and force it to be printed as a dictionary entry )
                        .NAME SPACE
                ENDOF
                ['] EXIT OF               ( is it EXIT? )
                        ( We expect the last word to be EXIT, and if it is then we don't print it
                          because EXIT is normally implied by ;.  EXIT can also appear in the middle
                          of words, and then it needs to be printed. )
                        2DUP                    ( end start end start )
                        1+                     ( end start end start+1 )
                        <> IF                   ( end start | we're not at the end )
                                ." EXIT "
                        THEN
                ENDOF
                                        ( default case: )
                        DUP                     ( in the default case we always need to DUP before using )
                        >NAME                    ( look up the codeword to get the dictionary entry )
                        .NAME SPACE               ( and print it )
                ENDCASE

                1+             ( end start+1 )
        REPEAT

        [CHAR] ; EMIT CR

        2DROP           ( restore stack )
;

