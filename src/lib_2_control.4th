\ Flow control

: IF IMMEDIATE
        ['] 0BRANCH ,     \ compile 0BRANCH
        HERE          \ save location of the offset on the stack
        0 ,             \ compile a dummy offset
;

: THEN IMMEDIATE
        DUP
        HERE SWAP -   \ calculate the offset from the address saved on the stack
        SWAP !          \ store the offset in the back-filled location
;

: ELSE IMMEDIATE
        ['] BRANCH ,      \ definite branch to just over the false-part
        HERE          \ save location of the offset on the stack
        0 ,             \ compile a dummy offset
        SWAP            \ now back-fill the original (IF) offset
        DUP             \ same as for THEN word above
        HERE SWAP -
        SWAP !
;

: BEGIN IMMEDIATE
        HERE          \ save location on the stack
;

: UNTIL IMMEDIATE
        ['] 0BRANCH ,     \ compile 0BRANCH
        HERE -        \ calculate the offset from the address saved on the stack
        ,               \ compile the offset here
;

: AGAIN IMMEDIATE
        ['] BRANCH ,      \ compile BRANCH
        HERE -        \ calculate the offset back
        ,               \ compile the offset here
;

: WHILE IMMEDIATE
        ['] 0BRANCH ,     \ compile 0BRANCH
        HERE          \ save location of the offset2 on the stack
        0 ,             \ compile a dummy offset2
;

: REPEAT IMMEDIATE
        ['] BRANCH ,      \ compile BRANCH
        SWAP            \ get the original offset (from BEGIN)
        HERE - ,      \ and compile it after BRANCH
        DUP
        HERE SWAP -   \ calculate the offset2
        SWAP !          \ and back-fill it in the original location
;

: UNLESS IMMEDIATE
        ['] NOT ,         \ compile NOT (to reverse the test)
        [COMPILE] IF    \ continue by calling the normal IF
;

: DO IMMEDIATE
        ['] LIT , -1 , [COMPILE] IF
        ['] >R , ['] >R ,
        ['] LIT , HERE 0 , ['] >R ,
        HERE
;

: ?DO IMMEDIATE
        ['] 2DUP , ['] - , [COMPILE] IF
        ['] >R , ['] >R ,
        ['] LIT , HERE 0 , ['] >R ,
        HERE
;

: I RSP@ 3 - @ ;

: J RSP@ 6 - @ ;

: ?LEAVE IMMEDIATE
        ['] 0BRANCH , 13 ,
        ['] R> , ['] RDROP , ['] RDROP ,
        ['] LIT ,  HERE 7 + , ['] DUP , ['] -ROT , ['] - , ['] SWAP , ['] ! ,
        ['] BRANCH ,
        0 ,
;

: LEAVE IMMEDIATE
        ['] LIT , -1 ,
        [COMPILE] ?LEAVE
;

: +LOOP IMMEDIATE

        ['] DUP , \ Store copy of increment

        ['] R> , ['] SWAP , ['] R> , ['] SWAP , ['] R> , ['] SWAP , ['] + , ['] 2DUP , ['] - ,
        ['] SWAP , ['] >R , ['] SWAP , ['] >R , ['] SWAP , ['] >R ,

        \ Condition differently depending on sign of increment
        ['] SWAP , ['] 0>= , [COMPILE] IF
            ['] 0<= ,
        [COMPILE] ELSE
            ['] 0> ,
        [COMPILE] THEN

        \ Branch back to begining of loop kernel
        ['] 0BRANCH , HERE - ,

        \ Clean up
        ['] RDROP , ['] RDROP , ['] RDROP ,

        \ Record address of loop end for any LEAVEs to use
        HERE SWAP !

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
