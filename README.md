# forth.jl

A hobby implementation of a FORTH-like system atop the Julia scientific
computing language.  It will almost certainly never be useful for any purpose
besides, that which it has already fulfilled: forcing me to think quite
carefully about how forth works. 

This package owes a massive debt to the existence of the literate programming
project [JonesForth] (https://rwmj.wordpress.com/2010/08/07/jonesforth-git-repository/),
which I believe I've had more fun reading than any other piece of code. To a
large degree my package is simply a port of that project from x86 assembly +
forth to julia + forth, although the mapping is in a few places non-trivial due
to the fact that julia is a high level language.  A huge proportion (say 80%)
of the library code in src/lib.4th is directly copied from JonesForth.  (The
fact that it was possible to reuse this code was satisfying in its own right!)
I've added some additional core definitions and modified some of the others to
be a little bit closer to the behaviour of ANS forth (or at least FORTH 83).

There's quite a lot to say about the implementation, especially due to its
high-level grounding, but that will have to wait for another time.

## Installation

forth.jl is not (and probably will never be) a registered julia package.  To
install it, you will therefore need to use the following command:

    julia> Pkg.clone("https://github.com/tgvaughan/forth.jl")

## Usage

To start the interpreter/compiler running, simply enter the following at
the julia prompt:

    julia> import forth
    julia> forth.run()

The first thing the interpreter will do is compile the core definitions in
the library file.  Once this is complete you can start entering forth commands:

    : star 42 emit ;
     ok
    star
    * ok

Notice that unlike other forths, forth.jl echos a newline after reading each
line of standard input.  This is an unfortunate side-effect of the way that
I've implemented the primitive word KEY.  Hopefully I'll be able to fix this
in future.

There's an example Mandelbrot Set drawing program included in the examples
directory.  To run it, you'll have to locate this directory on your system (its
location depends on what OS you happen to be using and how julia is installed).
Once found, use the "INCLUDE" word to compile its definitions. For example, on
my system I can run the example in this way:

    include /home/tim/.julia/v0.4/forth/examples/mandelbrot.4th
    Enter 'mandel' to draw the Mandelbrot Set. ok
    mandel
                                                                                *                   
                                                                                                    
                                                                           **                       
                                                                        ********                    
                                                                       *********                    
                                                                         *****                      
                                                          ***     ********************              
                                                           ****************************** ****      
                                                          **********************************        
                                                       ***************************************      
                                                     ********************************************   
                                  **    *            *******************************************    
                                  *************    *********************************************    
                               ******************  ********************************************     
                               ******************* ********************************************     
         **    *     *  *******************************************************************         
                               ******************* *******************************************      
                                *****************  ********************************************     
                                  *************     ********************************************    
                                  **    *           ********************************************    
                                                     ********************************************   
                                                        **************************************      
                                                         ***********************************        
                                                           ****************************** ****      
                                                           **     ********************              
                                                                         *****                      
                                                                        *******                     
                                                                       *********                    
                                                                           **                       
    ok

## License

This package is free software and is distributed under version 3.0 of the GNU
General Public License, which may be found in the file LICENSE in this
directory.
