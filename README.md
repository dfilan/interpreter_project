Making an interpreter in Haskell for a simple programming language that was designed for a foundations of mathematics class (see Chapter 8 part C of notes). Was initally following [this](https://ruslanspivak.com/lsbasi-part1/) tutorial for how one would build an interpreter, then just did things I wanted to do.

Note that all data are natural numbers, so 'subtraction' is actually the monus operator: x monus y = x - y if x > y and 0 otherwise.

Changes to the language as defined in the notes:
- 'Programs' are one line. They consist of various statements:
  - Return statements are written `return x;`, where `x` is any variable name, and do what you'd expect. 
  - If statements are written `if (v) {...}`, where `v` is a variable name, and `...` is a sequence of statements separated by semicolons. If `v` is non-zero, the inner block is evaluated, otherwise it isn't.
  - While statements are written `while (v) {...}`, where `v` is a variable name, and `...` is a sequence of statements separated by semicolons. The inner block loops as long as `v` is non-zero.
  - Assignments are written `x := ...;`, where `...` is the value that will be assigned to `x`. This can be any arbitrary arithmetic expression involving natural numbers and previously defined variables.
- All binary operators are right-associative, so `3 - 5 + 7` = `3 - (5 + 7)` = 0.
- You can use parentheses in arithmetic expressions like `(3 - 5) + 7`, which evaluates to 7.
