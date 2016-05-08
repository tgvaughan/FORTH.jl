: \ IMMEDIATE
        #TIB @ >IN !
; \ We can now comment!

\ BASIC DEFINITIONS  ----------------------------------------------------------------------

: / /MOD SWAP DROP ;
: MOD /MOD DROP ;
: */ -ROT * SWAP / ;

: NEGATE 0 SWAP - ;

: TRUE -1 ;
: FALSE 0 ;
: NOT 0= ;

\ Translate a number of cells into memory units
\ (in our case 1 cell = 1 memory unit)
: CELLS ;

\ Since the smallest unit of memory in our system is 64 bits and since strings
\ are stored as arrays of 64 bit integers, the character store/fetch words are
\ just aliases of the standard store/fetch words.
: C! ! ;
: C@ @ ;
: C, , ;

: DEPTH PSP@ PSP0 @ - ;

: '\n' 10 ;
: BL 32 ;

: LITERAL IMMEDIATE ['] LIT , , ;

: ' BL WORD FIND >CFA ;

: CHAR BL WORD 1+ @ ;
: [CHAR] IMMEDIATE
    CHAR
    ['] LIT , ,
;

: CR '\n' emit ;
: SPACE BL emit ;

: [COMPILE] IMMEDIATE
        BL WORD         \ get the next word
        FIND            \ find it in the dictionary
        >CFA            \ get its codeword
        ,               \ and compile that
;

: RECURSE IMMEDIATE
        LATEST @        \ LATEST points to the word being compiled at the moment
        >CFA            \ get the codeword
        ,               \ compile it
;

\ CONTROL STRUCTURES ----------------------------------------------------------------------

: IF IMMEDIATE
        ['] 0BRANCH ,     \ compile 0BRANCH
        HERE @          \ save location of the offset on the stack
        0 ,             \ compile a dummy offset
;

: THEN IMMEDIATE
        DUP
        HERE @ SWAP -   \ calculate the offset from the address saved on the stack
        SWAP !          \ store the offset in the back-filled location
;

: ELSE IMMEDIATE
        ['] BRANCH ,      \ definite branch to just over the false-part
        HERE @          \ save location of the offset on the stack
        0 ,             \ compile a dummy offset
        SWAP            \ now back-fill the original (IF) offset
        DUP             \ same as for THEN word above
        HERE @ SWAP -
        SWAP !
;

: BEGIN IMMEDIATE
        HERE @          \ save location on the stack
;

: UNTIL IMMEDIATE
        ['] 0BRANCH ,     \ compile 0BRANCH
        HERE @ -        \ calculate the offset from the address saved on the stack
        ,               \ compile the offset here
;

: AGAIN IMMEDIATE
        ['] BRANCH ,      \ compile BRANCH
        HERE @ -        \ calculate the offset back
        ,               \ compile the offset here
;

: WHILE IMMEDIATE
        ['] 0BRANCH ,     \ compile 0BRANCH
        HERE @          \ save location of the offset2 on the stack
        0 ,             \ compile a dummy offset2
;

: REPEAT IMMEDIATE
        ['] BRANCH ,      \ compile BRANCH
        SWAP            \ get the original offset (from BEGIN)
        HERE @ - ,      \ and compile it after BRANCH
        DUP
        HERE @ SWAP -   \ calculate the offset2
        SWAP !          \ and back-fill it in the original location
;

: UNLESS IMMEDIATE
        ['] NOT ,         \ compile NOT (to reverse the test)
        [COMPILE] IF    \ continue by calling the normal IF
;

: DO IMMEDIATE
        ['] LIT , -1 , [COMPILE] IF
        ['] >R , ['] >R ,
        ['] LIT , HERE @ 0 , ['] >R ,
        HERE @
;

: ?DO IMMEDIATE
        ['] 2DUP , ['] - , [COMPILE] IF
        ['] >R , ['] >R ,
        ['] LIT , HERE @ 0 , ['] >R ,
        HERE @
;

: I RSP@ 3 - @ ;

: J RSP@ 6 - @ ;

: ?LEAVE IMMEDIATE
        ['] 0BRANCH , 13 ,
        ['] R> , ['] RDROP , ['] RDROP ,
        ['] LIT ,  HERE @ 7 + , ['] DUP , ['] -ROT , ['] - , ['] SWAP , ['] ! ,
        ['] BRANCH ,
        0 ,
;

: LEAVE IMMEDIATE
        ['] LIT , -1 ,
        [COMPILE] ?LEAVE
;

: +LOOP IMMEDIATE

        trace

        ['] DUP , \ Store copy of increment

        ['] R> , ['] SWAP , ['] R> , ['] SWAP , ['] R> , ['] SWAP , ['] + , ['] 2DUP , ['] - ,
        ['] SWAP , ['] >R , ['] SWAP , ['] >R , ['] SWAP , ['] >R ,

        trace

        \ Condition differently depending on sign of increment
        ['] SWAP , ['] 0>= , [COMPILE] IF
            ['] 0<= ,
        [COMPILE] ELSE
            ['] 0> ,
        [COMPILE] THEN

        trace

        \ Branch back to begining of loop kernel
        ['] 0BRANCH , HERE @ - ,

        \ Clean up
        ['] RDROP , ['] RDROP , ['] RDROP ,

        trace

        \ Record address of loop end for any LEAVEs to use
        HERE @ SWAP !

        [COMPILE] ELSE
            ['] 2DROP , \ Clean up if loop was entirely skipped (?DO)
        [COMPILE] THEN
;

: LOOP IMMEDIATE
        ['] LIT , 1 ,
        [COMPILE] +LOOP
;


\ CASE ------------------------------------------------------------------------

: CASE IMMEDIATE
        0               \ push 0 to mark the bottom of the stack
;

: OF IMMEDIATE
        ['] OVER ,        \ compile OVER
        ['] = ,           \ compile =
        [COMPILE] IF      \ compile IF
        ['] DROP ,        \ compile DROP
;

: ENDOF IMMEDIATE
        [COMPILE] ELSE    \ ENDOF is the same as ELSE
;

: ENDCASE IMMEDIATE
        ['] DROP ,        \ compile DROP

        \ keep compiling THEN until we get to our zero marker
        BEGIN
                ?DUP
        WHILE
                [COMPILE] THEN
        REPEAT
;


\ COMMENTS ----------------------------------------------------------------------

: ( IMMEDIATE
        1               \ allowed nested parens by keeping track of depth
        BEGIN
                >IN @ #TIB @ >= IF      \ End of TIB?
                        QUERY       \ Get next line
                THEN

                TIB >IN @ + @ 1 >IN +!
                DUP [CHAR] ( = IF    \ open paren?
                        DROP            \ drop the open paren
                        1+              \ depth increases
                ELSE
                        [CHAR] ) = IF        \ close paren?
                                1-              \ depth decreases
                        THEN
                THEN
        DUP 0= UNTIL            \ continue until we reach matching close paren, depth 0
        DROP            \ drop the depth counter
;

( Some more complicated stack examples, showing the stack notation. )
: NIP ( x y -- y ) SWAP DROP ;
: TUCK ( x y -- y x y ) DUP -ROT ;
: PICK ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
        1+              ( add one because of 'u' on the stack )
        PSP@ SWAP -     ( add to the stack pointer )
        @               ( and fetch )
;
: ROLL ( x_u x_u-1... x_0 u -- x_u-1 ... x_0 x_u )
        1+ DUP PICK SWAP    ( x_u x_u-1 ... x_0 x_u u+1 )
        PSP@ 1- SWAP - PSP@ 2- SWAP
        DO
            i 1+ @ i !
        LOOP
        SWAP DROP
;

( With the looping constructs, we can now write SPACES, which writes n spaces to stdout. )
: SPACES        ( n -- )
        DUP 0> IF
            0 DO SPACE LOOP
        ELSE
            DROP
        THEN
;

( Standard words for manipulating BASE. )
: DECIMAL ( -- ) 10 BASE ! ;
: HEX ( -- ) 16 BASE ! ;

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

( PRINTING NUMBERS ---------------------------------------------------------------------- )

( This is the underlying recursive definition of U. )
: U.            ( u -- )
        BASE @ /MOD     ( width rem quot )
        ?DUP IF                 ( if quotient <> 0 then )
                RECURSE         ( print the quotient )
        THEN

        ( print the remainder )
        DUP 10 < IF
                [CHAR] 0             ( decimal digits 0..9 )
        ELSE
                10 -            ( hex and beyond digits A..Z )
                [CHAR] A
        THEN
        +
        EMIT
;

( This word returns the width (in characters) of an unsigned number in the current base )
: UWIDTH        ( u -- width )
        BASE @ /        ( rem quot )
        ?DUP IF         ( if quotient <> 0 then )
                RECURSE 1+      ( return 1+recursive call )
        ELSE
                1               ( return 1 )
        THEN
;

: U.R           ( u width -- )
        SWAP            ( width u )
        DUP             ( width u u )
        UWIDTH          ( width u uwidth )
        ROT            ( u uwidth width )
        SWAP -          ( u width-uwidth )
        ( At this point if the requested width is narrower, we'll have a negative number on the stack.
          Otherwise the number on the stack is the number of spaces to print.  But SPACES won't print
          a negative number of spaces anyway, so it's now safe to call SPACES ... )
        SPACES
        ( ... and then call the underlying implementation of U. )
        U.
;

: .R            ( n width -- )
        SWAP            ( width n )
        DUP 0< IF
                NEGATE          ( width u )
                1               ( save a flag to remember that it was negative | width n 1 )
                -ROT             ( 1 width u )
                SWAP            ( 1 u width )
                1-              ( 1 u width-1 )
        ELSE
                0               ( width u 0 )
                -ROT             ( 0 width u )
                SWAP            ( 0 u width )
        THEN
        SWAP            ( flag width u )
        DUP             ( flag width u u )
        UWIDTH          ( flag width u uwidth )
        ROT            ( flag u uwidth width )
        SWAP -          ( flag u width-uwidth )

        SPACES          ( flag u )
        SWAP            ( u flag )

        IF                      ( was it negative? print the - character )
                [CHAR] - EMIT
        THEN

        U.
;

: . 0 .R SPACE ;

: .S            ( -- )
        [CHAR] < EMIT DEPTH U. [CHAR] > EMIT SPACE
        PSP0 @ 1+
        BEGIN
                DUP PSP@ 2 - <=
        WHILE
                DUP @ .
                1+
        REPEAT
        DROP
;

: U. U. SPACE ;

( ? fetches the integer at an address and prints it. )
: ? ( addr -- ) @ . ;

( c a b WITHIN returns true if a <= c and c < b )
: WITHIN
        -ROT             ( b c a )
        OVER            ( b c a c )
        <= IF
                > IF            ( b c -- )
                        TRUE
                ELSE
                        FALSE
                THEN
        ELSE
                2DROP           ( b c -- )
                FALSE
        THEN
;


( STRINGS ---------------------------------------------------------------------- )


( Block copy, however, is important and novel: )
: CMOVE ( src dest length -- )

        DUP 0<= IF
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

( C, appends a byte to the current compiled word. )
: C,
        HERE @ C!
        1 HERE +!
;

: S" IMMEDIATE          ( -- addr len )
        STATE @ IF      ( compiling? )
                ['] LITSTRING ,   ( compile LITSTRING )
                HERE @          ( save the address of the length word on the stack )
                0 ,             ( dummy length - we don't know what it is yet )
                KEY DROP
                BEGIN
                        KEY             ( get next character of the string )
                        DUP [CHAR] " <>
                WHILE
                        C,              ( copy character )
                REPEAT
                DROP            ( drop the double quote character at the end )
                DUP             ( get the saved address of the length word )
                HERE @ SWAP -   ( calculate the length )
                1-              ( subtract 1 (because we measured from the start of the length word) )
                SWAP !          ( and back-fill the length location )
        ELSE            ( immediate mode )
                HERE @          ( get the start address of the temporary space )
                KEY DROP
                BEGIN
                        KEY
                        DUP [CHAR] " <>
                WHILE
                        OVER C!         ( save next character )
                        1+              ( increment address )
                REPEAT
                DROP            ( drop the final " character )
                HERE @ -        ( calculate the length )
                HERE @          ( push the start address )
                SWAP            ( addr len )
        THEN
;

: ." IMMEDIATE          ( -- )
        [COMPILE] S"    ( read the string, and compile LITSTRING, etc. )
        ['] TYPE ,      ( compile the final TYPE )
;

: .( 
        KEY DROP
        BEGIN
                KEY
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


( CONSTANTS AND VARIABLES ------------------------------------------------------ )

: CONSTANT
        BL WORD HEADER  ( make dictionary entry (the name follows CONSTANT) )
        DOCOL ,         ( append DOCOL (the codeword field of this word) )
        ['] LIT ,       ( append the codeword LIT )
        ,               ( append the value on the top of the stack )
        ['] EXIT ,      ( append the codeword EXIT )
;

: ALLOT         ( n -- )
        HERE +!         ( adds n to HERE, after this the old value of HERE is still on the stack )
;

: VARIABLE
        CREATE
        1 CELLS ALLOT   ( allocate 1 cell of memory, push the pointer to this memory )
;


: VALUE         ( n -- )
        BL WORD HEADER  ( make the dictionary entry (the name follows VALUE) )
        DOCOL ,         ( append DOCOL )
        ['] LIT ,       ( append the codeword LIT )
        ,               ( append the initial value )
        ['] EXIT ,      ( append the codeword EXIT )
;

: TO IMMEDIATE  ( n -- )
        BL WORD         ( get the name of the value )
        FIND            ( look it up in the dictionary )
        >PFA            ( get a pointer to the first data field (the 'LIT') )
        1+              ( increment to point at the value )
        STATE @ IF      ( compiling? )
                ['] LIT ,         ( compile LIT )
                ,               ( compile the address of the value )
                ['] ! ,           ( compile ! )
        ELSE            ( immediate mode )
                !               ( update it straightaway )
        THEN
;

( x +TO VAL adds x to VAL )
: +TO IMMEDIATE
        BL WORD         ( get the name of the value )
        FIND            ( look it up in the dictionary )
        >PFA            ( get a pointer to the first data field (the 'LIT') )
        1+              ( increment to point at the value )
        STATE @ IF      ( compiling? )
                ['] LIT ,         ( compile LIT )
                ,               ( compile the address of the value )
                ['] +! ,          ( compile +! )
        ELSE            ( immediate mode )
                +!              ( update it straightaway )
        THEN
;

( Fill u ints, starting at a, with the value b )
: FILL          ( a u b -- )
        -ROT OVER + SWAP ?DO
                DUP I !
        LOOP
        DROP
;

: ERASE         ( a u -- )
        0 FILL
;

( PRINTING THE DICTIONARY ------------------------------------------------------ )

: ID.
        1+              ( skip over the link pointer )
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
        LATEST @        ( start at LATEST dictionary entry )
        BEGIN
                ?DUP            ( while link pointer is not null )
        WHILE
                DUP ?HIDDEN NOT IF      ( ignore hidden words )
                        DUP ID.         ( but if not hidden, print the word )
                        SPACE
                THEN
                @               ( dereference the link pointer - go to previous word )
        REPEAT
        CR
;


( FORGET ---------------------------------------------------------------------- )

: FORGET
        BL WORD FIND    ( find the word, gets the dictionary entry address )
        DUP @ LATEST !  ( set LATEST to point to the previous word )
        HERE !          ( and store HERE with the dictionary address )
;

( DUMP ------------------------------------------------------------------------ )

\ TODO!


( DECOMPILER ------------------------------------------------------------------ )

: CFA>
        LATEST @        ( start at LATEST dictionary entry )
        BEGIN
                ?DUP            ( while link pointer is not null )
        WHILE
                2DUP SWAP       ( cfa curr curr cfa )
                < IF            ( current dictionary entry < cfa? )
                        NIP             ( leave curr dictionary entry on the stack )
                        EXIT
                THEN
                @               ( follow link pointer back )
        REPEAT
        DROP            ( restore stack )
        0               ( sorry, nothing found )
;

: SEE
        BL WORD 2DUP FIND     ( find the dictionary entry to decompile )

        ?DUP 0= IF
                ." Word '" TYPE ." ' not found in dictionary."
                EXIT
        THEN

        -ROT 2DROP

        ( Now we search again, looking for the next word in the dictionary.  This gives us
          the length of the word that we will be decompiling.  (Well, mostly it does). )
        HERE @          ( address of the end of the last compiled word )
        LATEST @        ( word last curr )
        BEGIN
                2 PICK          ( word last curr word )
                OVER            ( word last curr word curr )
                <>              ( word last curr word<>curr? )
        WHILE                   ( word last curr )
                NIP             ( word curr )
                DUP @           ( word curr prev (which becomes: word last curr) )
        REPEAT

        DROP            ( at this point, the stack is: start-of-word end-of-word )
        SWAP            ( end-of-word start-of-word )

        DUP >CFA @ CASE
                DOCOL OF
                        \ Colon definition
                        [CHAR] : EMIT SPACE DUP ID. SPACE
                        DUP ?IMMEDIATE IF ." IMMEDIATE " THEN CR
                ENDOF
                DOVAR OF
                        \ Variable definition
                        ." Variable " DUP ID. CR
                        2DROP EXIT
                ENDOF
                DOCON OF
                        \ Constant definition
                        ." Constant " DUP ID. CR
                        2DROP EXIT
                ENDOF

                \ Unknown codeword
                ." Primitive or word with unrecognized codeword." CR 
                DROP 2DROP EXIT
        ENDCASE

        ( begin the definition with : NAME [IMMEDIATE] )
        ( [CHAR] : EMIT SPACE DUP ID. SPACE
        DUP ?IMMEDIATE IF ." IMMEDIATE " THEN CR 4 )

        4 SPACES

        >PFA            ( get the data address, ie. points after DOCOL | end-of-word start-of-data )

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
                        CFA>                    ( and force it to be printed as a dictionary entry )
                        ID. SPACE
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
                        CFA>                    ( look up the codeword to get the dictionary entry )
                        ID. SPACE               ( and print it )
                ENDCASE

                1+             ( end start+1 )
        REPEAT

        [CHAR] ; EMIT CR

        2DROP           ( restore stack )
;


( MEMORY  ------------------------------------------------------------------ )

: UNUSED  ( -- cells )
        MEMSIZE HERE @ - ;
