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
include lib_11_extensions.4th

: LICENSE
    CR
    ." This program is free software: you can redistribute it and/or modify" CR
    ." it under the terms of the GNU General Public License as published by" CR
    ." the Free Software Foundation, either version 3 of the License, or" CR
    ." (at your option) any later version." CR
    ." " CR
    ." This program is distributed in the hope that it will be useful," CR
    ." but WITHOUT ANY WARRANTY; without even the implied warranty of" CR
    ." MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the" CR
    ." GNU General Public License for more details." CR
    ." " CR
    ." You should have received a copy of the GNU General Public License" CR
    ." along with this program.  If not, see http://www.gnu.org/licenses/." CR
;

: WELCOME
    SKIP-WELCOME @ INVERT IF
        ." Welcome to forth.jl!" CR CR

        ." Copyright (C) 2016 Tim Vaughan" CR
        ." This program comes with ABSOLUTELY NO WARRANY; for details type 'license'" CR
        ." Type 'bye' or press Ctrl+D to exit." CR CR
    THEN
;
welcome
