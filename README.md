# Compiler

## Compilation and Testing Instructions

To run the project and generate the assembly code to be executed in SPIM, use the following command:
  ocamlbuild test.byte ./tests/<test_name>.test > prog.asm && spim load "prog.asm"


To compile the project, use the following command:
  ocamlbuild build

To run a test on the compiled project, use the following command:

ocamlbuild test.byte  ./tests/<test_name>.test
  
If the ocamlbuild command is not found, run the following command:
  eval $(opam env)

## Language Description and Features

The language that I have developed is based on the C language with some differences. The syntax for conditions is If Then Else, and braces are not required. The syntax for loops is While Do, and braces are not required. The language features include integer and boolean data types, variable management, function calls, variable declarations, variable assignments, return statements, conditions, and loops.
