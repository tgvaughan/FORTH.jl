: \ IMMEDIATE
        #IB @ >IN !
; \ We can now comment!

\ Compile core definitions
\ (These files must be compiled in order!)

include lib_1_basic.4th
include lib_2_control.4th
include lib_3_input.4th
include lib_4_comments.4th
include lib_5_printnum.4th
include lib_6_strings.4th
include lib_7_variables.4th
include lib_8_vocab.4th
include lib_9_decompiler.4th
include lib_10_misc.4th

.(  done.) cr
