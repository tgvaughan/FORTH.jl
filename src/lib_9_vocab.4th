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

: ALSO
        context #context @ + dup 1- @ swap !
        1 #context +!
;

\ Create new vocabulary
: VOCABULARY
        create 0 ,
does>
        body> context #context @ 1- + !
;

: DEFINITIONS
        context @ current !
;

vocabulary root
also root definitions

: FORTH forth ;

: ONLY
        1 #context !
        root 
        2 #context !
;

\ only forth

\ Display search order and compilation dictionary
: ORDER

        \ Search order
        context #context @ 1- + context swap
        do
            i @ >name .name space
        -1 +loop

        \ Current (definitions)
        9 emit
        current @ >name .name
;
