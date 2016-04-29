: / /MOD SWAP DROP ;
: MOD /MOD DROP ;
: */ -ROT * SWAP / ;

: NEGATE 0 SWAP - ;

: TRUE -1 ;
: FALSE 0 ;
: NOT 0= ;

: CELLS ; \ Allow for slightly more portable code

: DEPTH PSP@ PSP0 @ - ;

: '\n' 10 ;
: BL 32 ;

: LITERAL IMMEDIATE ' LIT , , ;

: ':' [ CHAR : ] LITERAL ;
: ';' [ CHAR ; ] LITERAL ;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '<' [ CHAR < ] LITERAL ;
: '>' [ CHAR > ] LITERAL ;
: '"' [ CHAR " ] LITERAL ;
: 'A' [ CHAR A ] LITERAL ;
: '0' [ CHAR 0 ] LITERAL ;
: '-' [ CHAR - ] LITERAL ;
: '.' [ CHAR . ] LITERAL ;

: CR '\n' emit ;
: SPACE BL emit ;

: [COMPILE] IMMEDIATE
        WORD            \ get the next word
        FIND            \ find it in the dictionary
        >CFA            \ get its codeword
        ,               \ and compile that
;

: RECURSE IMMEDIATE
        LATEST @        \ LATEST points to the word being compiled at the moment
        >CFA            \ get the codeword
        ,               \ compile it
;

: DEBUGON TRUE DEBUG ! ;
: DEBUGOFF FALSE DEBUG ! ;

\ CONTROL STRUCTURES ----------------------------------------------------------------------

: IF IMMEDIATE
        ' 0BRANCH ,     \ compile 0BRANCH
        HERE @          \ save location of the offset on the stack
        0 ,             \ compile a dummy offset
;

: THEN IMMEDIATE
        DUP
        HERE @ SWAP -   \ calculate the offset from the address saved on the stack
        SWAP !          \ store the offset in the back-filled location
;

: ELSE IMMEDIATE
        ' BRANCH ,      \ definite branch to just over the false-part
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
        ' 0BRANCH ,     \ compile 0BRANCH
        HERE @ -        \ calculate the offset from the address saved on the stack
        ,               \ compile the offset here
;

: AGAIN IMMEDIATE
        ' BRANCH ,      \ compile BRANCH
        HERE @ -        \ calculate the offset back
        ,               \ compile the offset here
;

: WHILE IMMEDIATE
        ' 0BRANCH ,     \ compile 0BRANCH
        HERE @          \ save location of the offset2 on the stack
        0 ,             \ compile a dummy offset2
;

: REPEAT IMMEDIATE
        ' BRANCH ,      \ compile BRANCH
        SWAP            \ get the original offset (from BEGIN)
        HERE @ - ,      \ and compile it after BRANCH
        DUP
        HERE @ SWAP -   \ calculate the offset2
        SWAP !          \ and back-fill it in the original location
;

: UNLESS IMMEDIATE
        ' NOT ,         \ compile NOT (to reverse the test)
        [COMPILE] IF    \ continue by calling the normal IF
;

: DO IMMEDIATE
        ' LIT , -1 , [COMPILE] IF
        ' >R , ' >R ,
        ' LIT , HERE @ 0 , ' >R ,
        HERE @
;

: ?DO IMMEDIATE
        ' 2DUP , ' - , [COMPILE] IF
        ' >R , ' >R ,
        ' LIT , HERE @ 0 , ' >R ,
        HERE @
;

: I RSP@ 3 - @ ;

: J RSP@ 6 - @ ;

: ?LEAVE IMMEDIATE
        ' 0BRANCH , 13 ,
        ' R> , ' RDROP , ' RDROP ,
        ' LIT ,  HERE @ 7 + , ' DUP , ' -ROT , ' - , ' SWAP , ' ! ,
        ' BRANCH ,
        0 ,
;

: LEAVE IMMEDIATE
        ' LIT , -1 ,
        [COMPILE] ?LEAVE
;

: +LOOP IMMEDIATE
        ' DUP , \ Store copy of increment

        ' R> , ' SWAP , ' R> , ' SWAP , ' R> , ' SWAP , ' + , ' 2DUP , ' - ,
        ' SWAP , ' >R , ' SWAP , ' >R , ' SWAP , ' >R ,

        \ Condition differently depending on sign of increment
        ' SWAP , ' 0>= , [COMPILE] IF
            ' 0<= ,
        [COMPILE] ELSE
            ' 0> ,
        [COMPILE] THEN

        \ Branch back to begining of loop kernel
        ' 0BRANCH , HERE @ - ,

        \ Clean up
        ' RDROP , ' RDROP , ' RDROP ,

        \ Record address of loop end for any LEAVEs to use
        HERE @ SWAP !

        [COMPILE] ELSE
            ' 2DROP , \ Clean up if loop was entirely skipped (?DO)
        [COMPILE] THEN
;

: LOOP IMMEDIATE
        ' LIT , 1 ,
        [COMPILE] +LOOP
;

\ COMMENTS ----------------------------------------------------------------------

: ( IMMEDIATE
        1               \ allowed nested parens by keeping track of depth
        BEGIN
                KEY             \ read next character
                DUP '(' = IF    \ open paren?
                        DROP            \ drop the open paren
                        1+              \ depth increases
                ELSE
                        ')' = IF        \ close paren?
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
                '0'             ( decimal digits 0..9 )
        ELSE
                10 -            ( hex and beyond digits A..Z )
                'A'
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
                '-' EMIT
        THEN

        U.
;

: . 0 .R SPACE ;

: .S            ( -- )
        '<' EMIT DEPTH U. '>' EMIT SPACE
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

( Since the smallest unit of memory in our system is 64 bits and since strings
  are stored as arrays of 64 bit integers, the character store/fetch words are
  just aliases of the standard store/fetch words. )
: C! ! ;
: C@ @ ;

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
                ' LITSTRING ,   ( compile LITSTRING )
                HERE @          ( save the address of the length word on the stack )
                0 ,             ( dummy length - we don't know what it is yet )
                KEY DROP
                BEGIN
                        KEY             ( get next character of the string )
                        DUP '"' <>
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
                        DUP '"' <>
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
        STATE @ IF      ( compiling? )
                [COMPILE] S"    ( read the string, and compile LITSTRING, etc. )
                ' TELL ,        ( compile the final TELL )
        ELSE
                ( In immediate mode, just read characters and print them until we get
                  to the ending double quote. )
                KEY DROP
                BEGIN
                        KEY
                        DUP '"' = IF
                                DROP    ( drop the double quote character )
                                EXIT    ( return from this function )
                        THEN
                        EMIT
                AGAIN
        THEN
;

CR CR ."  --- Welcome to TimForth! ---" CR CR
