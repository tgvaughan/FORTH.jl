# forth.jl

A hobby implementation of a forth system atop the Julia scientific computing
language.  It will almost certainly never be useful for any purpose besides
that which it has already fulfilled: forcing me to think quite carefully about
how forth works. 

This package owes a massive debt to the existence of the literate programming
project [JonesForth] (https://rwmj.wordpress.com/2010/08/07/jonesforth-git-repository/),
which was an amazing read. To a large degree my package is simply a port of
that project from x86 assembly + forth to julia + forth, although the mapping
is in a few places non-trivial due to the fact that julia is a high level
language.  A huge proportion (say 80%) of the library code in src/lib.4th is
directly copied from JonesForth.  (The fact that it was possible to reuse this
code was satisfying in its own right!) I've added some additional core
definitions and modified some of the others with the eventual aim of F83
compliance (discussed below).

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

    : star 42 emit ;  ok
    star * ok

There's an example Mandelbrot Set drawing program included in the examples
directory.  To run it, use the `INCLUDE` word to open the file and compile its
definitions.  Although the exact location of the examples directory in your
filesystem is platform dependent, `INCLUDE` includes the forth.jl src/ directory
in its search path so the following should always work:

    include ../examples/mandelbrot.4th
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

To exit, enter ^D on a blank line or use the `BYE` word.

## FORTH-83 Compliance

One of my goals has been to have forth.jl contain as much of the
[F83 required word set](http://forth.sourceforge.net/standard/fst83/fst83-12.htm) 
as makes sense given the underlying VM. (Actually, my main goal goes a bit
beyond this: I want to be able to, with a couple of exceptions, be compatible
with the description of forth contained in the second edition of Leo Brodie's
book "Starting Forth".)  I'm fairly happy with my progress so far.  Of the
131 required F83 words, only 21 remain unimplemented.  These words fall into
three categories: those I intend to implement in the near future, those I may
possibly implement at some point, and those that I do not intend to ever implement
for reasons of obsolescence or incompatibility with the design of the VM.

### F83 Words to be implemented soon

    PAD

The word `PAD` is a simple oversite.

### F83 Words that may be implemented someday

    # #> #S -TRAILING <#

These words all have to do with number to string conversion, something I've
not been interested in enough yet to get on top of.

### F83 Words that won't be implemented

    D+ D< DNEGATE U< UM* UM/MOD BLOCK BUFFER FLUSH
    SAVE-BUFFERS UPDATE BLK HOLD LOAD FORTH-83

These words don't make sense to implement.  The double-length integer words are
useless, because the smallest unit of memory in our VM is a full 64 bit
integer.  For the same reason, there's no point in dealing with unsigned values
just to gain access to another bit.  The block I/O words don't make sense because
we have access to a filesystem via the OS.

## License

This package is free software and is distributed under version 3.0 of the GNU
General Public License, which may be found in the file LICENSE in this
directory.
