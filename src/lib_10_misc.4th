\ Miscellaneous undefined core words

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

( Stractch pad. )
: PAD           ( -- addr )
        HERE 340 + ;

: UNUSED  ( -- cells )
        MEMSIZE HERE - ;
