\ Vocabulary management

: FORGET
        BL WORD FIND >LINK  ( find the word, gets the dictionary entry address )
        DUP @ LATEST !      ( set LATEST to point to the previous word )
        H !                 ( and store H with the dictionary address )
;

: HIDE
        BL WORD FIND DROP >NAME
        DUP @ F_HIDDEN OR SWAP !
;

: VOCAB>LATEST ( vcfa -- vlatest )
        1+ @ @ ;

: ORDER

    \ Search order
    context #context @ 1- + context
    do
        i @ >name .name
    loop

    \ Current (definitions)
    9 emit
    current @ >name .name
;
