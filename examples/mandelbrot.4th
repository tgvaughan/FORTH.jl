( --- Complex arithmetic --- )

( Location of floating point. )
: precision 10000 ;

: >scaled precision 10 / * swap precision * + ;

( Redefine multiplication.  Yay forth! )
: * precision */ ;

: c* ( x1 y1 x2 y2 -- x3 y3 )
        swap -rot               ( x1 x2 y1 y2 )
        2dup * negate           ( x1 x2 y1 y2 -y1y2 )
        4 pick 4 pick * +       ( x1 x2 y1 y2 (x1x2-y1y2))
        4 roll 2 roll *         ( x2 y1 (x1x2-y1y2) x1y2 )
        3 roll 3 roll * +       ( (x1x2-y1y2) (x1y2+x2y1) )
;

: c+ ( x1 y1 x2 y2 -- x3 y3 )
        rot +
        -rot +
        swap
;

: csq 2dup c* ;

: cmagsq ( x1 y1 -- mag )
        csq abs
;

( --- Mandelbrot set calculations  --- )

: iterate ( cr ci zr zi -- cr ci z'r z'i )
        csq c+
;

: inSet? ( cr ci -- res )
    
    100 0 do

        2swap 2dup 5 roll 5 roll
        iterate
        2dup cmagsq
        100 > if
            leave
        then

    loop
;

( Draw the Mandelbrot Set!)
: mandel ( x1 y1 x2 y2 -- )

;

( Clean up - hide non-standard multiplication def. )
hide *
