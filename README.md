Making an interpreter in Haskell for a simple programming language that was designed for a foundations of mathematics class (see Chapter 8 part C of notes). Was initally following [this](https://ruslanspivak.com/lsbasi-part1/) tutorial for how one would build an interpreter, then just did things I wanted to do.

Note that all data are natural numbers, so 'subtraction' is actually the monus operator: x monus y = x - y if x > y and 0 otherwise.

To evaluate a program on some arguments, in the terminal run `./interpreter path_to_program [1,2,3]`, where `path_to_program` is the file path to your program, and `[1,2,3]` should be replaced by your list of arguments, formatted in the same way (open square bracket, natural numbers separated by commas and no spaces, close square bracket).

Changes to the language as defined in the notes:
- The definition of a routine does not need to include the local variables that the routine use, and its arguments when called can be any expressions, e.g. `F((x+y)*3,0,G(3,x))`.
- The insides of routines consist of various statements:
  - Return statements are written `return ...`, where `...` is any valid expression, and do what you'd expect. 
  - If statements are written `if (e) {...};`, where `e` is any valid expression, and `...` is a sequence of statements separated by semicolons. If the expression evaluates to something other than zero, the inner block is evaluated, otherwise it isn't. There can also be an else block, which executes if the expression does evaluate to zero: this is written `if (e) {...} else {...};`.
  - While statements are written `while (e) {...};`, where `e` is any valid expression, and `...` is a sequence of statements separated by semicolons. The inner block loops as long as `e` doesn't evaluate to zero.
  - Assignments are written `x := ...;`, where `...` is the value that will be assigned to `x`. This can be any arbitrary arithmetic expression involving natural numbers, previously defined variables, and routines defined anywhere in the program.
- Every statement in a block must end with a semicolon.
- All binary operators are right-associative, so `3 - 5 + 7` = `3 - (5 + 7)` = 0.
- You can use parentheses in arithmetic expressions like `(3 - 5) + 7`, which evaluates to 7.
- Routine and variable names must consist entirely of alphanumeric characters. Routine names must begin with an upper-case letter, and variable names must begin with a lower-case letter
