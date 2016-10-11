\ Input stream definitions

: SOURCE-ID
    SOURCE-ID-VAR @ ;

: QUERY-INPUT
    SOURCE-ID ?DUP 0= IF
        QUERY
    ELSE
        QUERY-FILE
    THEN
;
