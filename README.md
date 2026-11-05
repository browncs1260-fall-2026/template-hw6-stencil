# Running Tests

To fully rebuild your code and run all tests
- ```dune clean```
- ```dune build```
- ```dune runtest```

## Running individual tests
You can run the interpreter on a file:

`dune exec bin/interp.exe -- <file.lisp>`

You can also run the compiler:

`dune exec bin/compile.exe -- <file.lisp> out`

The resulting `.s` file (containing assembly code) and `.exe` file (an
executable) will be in the `out/` directory. You can run the executable with

`./out/<file.lisp>.exe`

You can also tell the compiler to run the executable immediately after compiling
by adding `-r`, e.g.:

`dune exec bin/compile.exe -- <file.lisp> out -r`

# Running GDB

For more information, see the Lab 3 handout.

1. To compile an executable into the out directory

`dune exec ./bin/compile.exe -- examples/<file.lisp> out`

2. To launch GDB
   
`gdb out/<file.lisp.exe>`

3. Switch GDB to use the flavor of assembly we use in the course

`set disassembly-flavor intel`

4. To run your program

`run`

## Helpful GDB commands

- la asm        : Shows source assembly code
- la reg        : Shows register values
- x/d $rsp-8    : Prints the value at address 8 below the value stored in rsp as a decimal
- b \<label>    : Sets a breakpoint at the label
- si            : Steps to the next instruction
- c             : Continues program execution
- ctrl+l        : Resets visual interface if it gets messed up
- focus cmd/asm : Switches focus of arrow keys to command window or assembly window
