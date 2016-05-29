\ Display dictionary contents

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

: WORDS
        CR
        LATEST @        ( start at LATEST dictionary entry )
        BEGIN
                ?DUP            ( while link pointer is not null )
        WHILE
                DUP ?HIDDEN NOT IF      ( ignore hidden words )
                        DUP 1+ .NAME         ( but if not hidden, print the word )
                        SPACE
                THEN
                @               ( dereference the link pointer - go to previous word )
        REPEAT
        CR
;
