# bfcompiler

`bfcompiler` is a [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) to [NASM](https://nasm.us) (Netwide Assembler, a portable 80x86 assembler) compiler written in [Haskell](https://haskell.org).  
Given a Brainfuck file it creates a `*.nasm` file, which can be assembled into a 64-bit Linux executable.  
It is advised to run programs in an interpreter before compiling them, as `bfcompiler` does not check for syntax errors.

### Compiling `bfcompiler`

To compile `bfcompiler` the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) is required.  
Then, use a similar command:

    $ ghc -dynamic -threaded -O2 Main.hs -o bfcompiler

Compilation flags may be changed, for example with no dynamic GHC libraries present,  
a `-static` instead of `-dynamic` flag might be used.

### Example usage

    $ ls
    hello.bf  mandelbrot.bf  numwarp.bf
    $ cat hello.bf
    https://en.wikipedia.org/wiki/Brainfuck#Hello_World!
    ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
    $ ./bfcompiler *.bf
    $ ls
    hello.bf            hello.nasm     mandelbrot.bf
    mandelbrot.nasm     numwarp.bf     numwarp.nasm

The generated `*.nasm` files can be compiled and linked with:

    $ nasm -f elf64 hello.nasm
    $ ld -m elf_x86_64 hello.o -o hello
    $ ./hello
    Hello World!
