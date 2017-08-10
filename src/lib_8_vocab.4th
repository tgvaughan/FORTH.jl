\ Vocabulary management

\ Forget word and everything defined after it in compilation dict
: FORGET
        BL WORD CURRENT @ FINDVOCAB ( find the word, gets the dictionary entry address )

        0= if
                drop exit
        then

        >link

        dup @ current @ 1+ !      ( set LATEST to point to the previous word )
;

\ Mark word as hidden
: HIDE ( -- )
        BL WORD FIND DROP >NAME
        DUP @ F_HIDDEN OR SWAP !
;

: ?HIDDEN
        1+              ( skip over the link pointer )
        @               ( get the flags/length byte )
        F_HIDDEN AND    ( mask the F_HIDDEN flag and return it (as a truth value) )
;

\ Display name of word
: .NAME ( cfa -- )
        DUP @           ( get the flags/length byte )
        F_LENMASK AND   ( mask out the flags - just want the length )

        BEGIN
                DUP 0>          ( length > 0? )
        WHILE
                SWAP 1+         ( addr len -- len addr+1 )
                DUP @           ( len addr -- len addr char | get the next character)
                DUP 32 >= IF
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

\ Create new vocabulary
: VOCABULARY
        create 0 ,
does>
        body> context #context @ 1- + !
;

: DEFINITIONS
        context #context @ 1- + @ current !
;

\ Define root vocabulary (always available)
vocabulary ROOT

: ONLY
        1 #context !
        root 
        2 #context !
        root 
;

: PREVIOUS
        #context @
        1 <= abort" Cannot empty search order stack!"

        1 #context -!
;

: ALSO
        context #context @ + dup 1- @ swap !
        1 #context +!
;

also root definitions

    : FORTH forth ;

    \ Display search order and compilation dictionary
    : ORDER

            \ Search order
            context #context @ 1- + context swap
            do
                i @ >name .name space
            -1 +loop

            \ Current (definitions)
            5 spaces
            current @ >name .name
    ;

    \ Display transient vocabulary contents
    : WORDS
            cr
            context #context @ 1- + @
            1+ @
            BEGIN
                    ?DUP            ( while link pointer is not 0 )
            WHILE
                    DUP ?HIDDEN NOT IF      ( ignore hidden words )
                            DUP 1+ .NAME         ( but if not hidden, print the word )
                            SPACE
                    THEN
                    @               ( dereference the link pointer - go to previous word )
            REPEAT
            CR
    ;

only forth also definitions
