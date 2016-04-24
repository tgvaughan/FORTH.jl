: / /MOD SWAP DROP ;
: MOD /MOD DROP ;

: '\n' 10 ;
: BL 32 ;

: CR '\n' emit ;
: SPACE BL emit ;

: NEGATE 0 swap - ;

: TRUE -1 ;
: FALSE 0 ;
: NOT 0= ;

: LITERAL IMMEDIATE ' LIT , , ;

: ':'
    [
    CHAR :
    ]
    LITERAL
;

: ';' [ CHAR ; ] LITERAL ;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '"' [ CHAR " ] LITERAL ;
: 'A' [ CHAR A ] LITERAL ;
: '0' [ CHAR 0 ] LITERAL ;
: '-' [ CHAR - ] LITERAL ;
: '.' [ CHAR . ] LITERAL ;

\ While compiling, '[COMPILE] word' compiles 'word' if it would otherwise be IMMEDIATE.
: [COMPILE] IMMEDIATE
        WORD            \ get the next word
        FIND            \ find it in the dictionary
        >CFA            \ get its codeword
        ,               \ and compile that
;

\ RECURSE makes a recursive call to the current word that is being compiled.
\
\ Normally while a word is being compiled, it is marked HIDDEN so that references to the
\ same word within are calls to the previous definition of the word.  However we still have
\ access to the word which we are currently compiling through the LATEST pointer so we
\ can use that to compile a recursive call.
: RECURSE IMMEDIATE
        LATEST @        \ LATEST points to the word being compiled at the moment
        >CFA            \ get the codeword
        ,               \ compile it
;

\       CONTROL STRUCTURES ----------------------------------------------------------------------
\
\ So far we have defined only very simple definitions.  Before we can go further, we really need to
\ make some control structures, like IF ... THEN and loops.  Luckily we can define arbitrary control
\ structures directly in FORTH.
\
\ Please note that the control structures as I have defined them here will only work inside compiled
\ words.  If you try to type in expressions using IF, etc. in immediate mode, then they won't work.
\ Making these work in immediate mode is left as an exercise for the reader.

\ condition IF true-part THEN rest
\       -- compiles to: --> condition 0BRANCH OFFSET true-part rest
\       where OFFSET is the offset of 'rest'
\ condition IF true-part ELSE false-part THEN
\       -- compiles to: --> condition 0BRANCH OFFSET true-part BRANCH OFFSET2 false-part rest
\       where OFFSET if the offset of false-part and OFFSET2 is the offset of rest

\ IF is an IMMEDIATE word which compiles 0BRANCH followed by a dummy offset, and places
\ the address of the 0BRANCH on the stack.  Later when we see THEN, we pop that address
\ off the stack, calculate the offset, and back-fill the offset.
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

\ BEGIN loop-part condition UNTIL
\       -- compiles to: --> loop-part condition 0BRANCH OFFSET
\       where OFFSET points back to the loop-part
\ This is like do { loop-part } while (condition) in the C language
: BEGIN IMMEDIATE
        HERE @          \ save location on the stack
;

: UNTIL IMMEDIATE
        ' 0BRANCH ,     \ compile 0BRANCH
        HERE @ -        \ calculate the offset from the address saved on the stack
        ,               \ compile the offset here
;

\ BEGIN loop-part AGAIN
\       -- compiles to: --> loop-part BRANCH OFFSET
\       where OFFSET points back to the loop-part
\ In other words, an infinite loop which can only be returned from with EXIT
: AGAIN IMMEDIATE
        ' BRANCH ,      \ compile BRANCH
        HERE @ -        \ calculate the offset back
        ,               \ compile the offset here
;

\ BEGIN condition WHILE loop-part REPEAT
\       -- compiles to: --> condition 0BRANCH OFFSET2 loop-part BRANCH OFFSET
\       where OFFSET points back to condition (the beginning) and OFFSET2 points to after the whole piece of code
\ So this is like a while (condition) { loop-part } loop in the C language
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

\ UNLESS is the same as IF but the test is reversed.
\
\ Note the use of [COMPILE]: Since IF is IMMEDIATE we don't want it to be executed while UNLESS
\ is compiling, but while UNLESS is running (which happens to be when whatever word using UNLESS is
\ being compiled -- whew!).  So we use [COMPILE] to reverse the effect of marking IF as immediate.
\ This trick is generally used when we want to write our own control words without having to
\ implement them all in terms of the primitives 0BRANCH and BRANCH, but instead reusing simpler
\ control words like (in this instance) IF.
: UNLESS IMMEDIATE
        ' NOT ,         \ compile NOT (to reverse the test)
        [COMPILE] IF    \ continue by calling the normal IF
;

\       COMMENTS ----------------------------------------------------------------------
\
\ FORTH allows ( ... ) as comments within function definitions.  This works by having an IMMEDIATE
\ word called ( which just drops input characters until it hits the corresponding ).
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

(
        From now on we can use ( ... ) for comments.

        STACK NOTATION ----------------------------------------------------------------------

        In FORTH style we can also use ( ... -- ... ) to show the effects that a word has on the
        parameter stack.  For example:

        ( n -- )        means that the word consumes an integer (n) from the parameter stack.
        ( b a -- c )    means that the word uses two integers (a and b, where a is at the top of stack)
                                and returns a single integer (c).
        ( -- )          means the word has no effect on the stack
)

( Some more complicated stack examples, showing the stack notation. )
: NIP ( x y -- y ) SWAP DROP ;
: TUCK ( x y -- y x y ) DUP ROT ;
: PICK ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
        1+              ( add one because of 'u' on the stack )
        PSP@ SWAP -     ( add to the stack pointer )
        @               ( and fetch )
;

( With the looping constructs, we can now write SPACES, which writes n spaces to stdout. )
: SPACES        ( n -- )
        BEGIN
                DUP 0>          ( while n > 0 )
        WHILE
                SPACE           ( print a space )
                1-              ( until we count down to 0 )
        REPEAT
        DROP
;

( Standard words for manipulating BASE. )
: DECIMAL ( -- ) 10 BASE ! ;
: HEX ( -- ) 16 BASE ! ;

(
        PRINTING NUMBERS ----------------------------------------------------------------------

        The standard FORTH word . (DOT) is very important.  It takes the number at the top
        of the stack and prints it out.  However first I'm going to implement some lower-level
        FORTH words:

        U.R     ( u width -- )  which prints an unsigned number, padded to a certain width
        U.      ( u -- )        which prints an unsigned number
        .R      ( n width -- )  which prints a signed number, padded to a certain width.

        For example:
                -123 6 .R
        will print out these characters:
                <space> <space> - 1 2 3

        In other words, the number padded left to a certain number of characters.

        The full number is printed even if it is wider than width, and this is what allows us to
        define the ordinary functions U. and . (we just set width to zero knowing that the full
        number will be printed anyway).

        Another wrinkle of . and friends is that they obey the current base in the variable BASE.
        BASE can be anything in the range 2 to 36.

        While we're defining . &c we can also define .S which is a useful debugging tool.  This
        word prints the current stack (non-destructively) from top to bottom.
)

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


