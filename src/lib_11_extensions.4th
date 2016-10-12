\ Non-core extension words

CREATE CODEBUFFER 1000 CELLS ALLOT
VARIABLE >CB
0 >CB !

: PARSE-CODE
    0 >CB !

    BEGIN
        >IN @ #IB @ >= IF   \ End of IB?
            '\n' CODEBUFFER >CB @ + !
            1 >CB +!
            SOURCE-ID 0= IF CR THEN
            QUERY-INPUT     \ Get next line
        ELSE
            BL CODEBUFFER >CB @ + !
            1 >CB +!
        THEN

        BL WORD COUNT
        2DUP ( addr n addr n)
        PAD SWAP CMOVE

        PAD OVER TOLOWER
        PAD OVER s" end-code" COMPARE
        0= IF
            2DROP EXIT
        THEN

        dup -rot ( n addr n )
        CODEBUFFER >CB @ + SWAP CMOVE
        >CB +!
    AGAIN
;

: CODE
    BL WORD HEADER
    PARSE-CODE
    CODEBUFFER >CB @ CREATE-PRIM ,
;
