\ Miscellaneous core words

: ROLL ( x_u x_u-1... x_0 u -- x_u-1 ... x_0 x_u )
        1+ DUP PICK SWAP    ( x_u x_u-1 ... x_0 x_u u+1 )
        PSP@ 1- SWAP - PSP@ 2- SWAP
        DO
            i 1+ @ i !
        LOOP
        SWAP DROP
;

( Compute absolute value. )
: ABS           ( n -- |n| )
        dup 0< if
                negate
        then
;

: MAX           ( n m -- max )
        2dup - 0< if
                swap drop
        else
                drop
        then
;

: MIN           ( n m -- max )
        2dup - 0> if
                swap drop
        else
                drop
        then
;

: FORGET
        BL WORD FIND >LFA   ( find the word, gets the dictionary entry address )
        DUP @ LATEST !      ( set LATEST to point to the previous word )
        H !                 ( and store H with the dictionary address )
;

: HIDE
        BL WORD FIND DROP >NAME
        DUP @ F_HIDDEN OR SWAP !
;

: UNUSED  ( -- cells )
        MEMSIZE HERE - ;
