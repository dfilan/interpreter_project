Making an interpreter in Haskell for a simple programming language that was designed for a foundations of mathematics class (see Chapter 8 part C of notes). Was initally following [this](https://ruslanspivak.com/lsbasi-part1/) tutorial for how one would build an interpreter, then just did things I wanted to do.

Note that all data are natural numbers, so 'subtraction' is actually the monus operator: x monus y = x - y if x > y and 0 otherwise.

Changes to the language as defined in the notes:
- As in the text, routines have names that begin with capital letters, and the first needs to be called main. Unlike in the text, their definition does not need to include the local variables that they use, and their arguments can be any atomic expressions, so `F(x,0,G(3,x))` is a valid routine call, but `F(x+y,0,G(3,x))` is not.
- The insides of routines consist of various statements:
  - Return statements are written `return ...`, where `...` is any valid expression, and do what you'd expect. 
  - If statements are written `if (v) {...}`, where `v` is a variable name, and `...` is a sequence of statements separated by semicolons. If `v` is non-zero, the inner block is evaluated, otherwise it isn't. There can also be an else block, which executes if `v` is zero: this is written `if (v) {...} else {...}`.
  - While statements are written `while (v) {...}`, where `v` is a variable name, and `...` is a sequence of statements separated by semicolons. The inner block loops as long as `v` is non-zero.
  - Assignments are written `x := ...;`, where `...` is the value that will be assigned to `x`. This can be any arbitrary arithmetic expression involving natural numbers, previously defined variables, and routines defined anywhere in the program.
- All binary operators are right-associative, so `3 - 5 + 7` = `3 - (5 + 7)` = 0.
- You can use parentheses in arithmetic expressions like `(3 - 5) + 7`, which evaluates to 7.
