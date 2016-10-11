\ Parenthetic comments

: ( IMMEDIATE
        1               \ allowed nested parens by keeping track of depth
        BEGIN
                >IN @ #IB @ >= IF      \ End of TIB?
                        QUERY-INPUT    \ Get next line
                THEN

                IB >IN @ + @ 1 >IN +!
                DUP [CHAR] ( = IF       \ open paren?
                        DROP            \ drop the open paren
                        1+              \ depth increases
                ELSE
                        [CHAR] ) = IF   \ close paren?
                                1-      \ depth decreases
                        THEN
                THEN
        DUP 0= UNTIL            \ continue until we reach matching close paren, depth 0
        DROP            \ drop the depth counter
;

