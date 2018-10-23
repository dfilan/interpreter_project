Making an interpreter in Haskell for a simple programming language that was designed for a foundations of mathematics class (see Chapter 8 part C of notes). Was initally following [this](https://ruslanspivak.com/lsbasi-part1/) tutorial for how one would build an interpreter, then just did things I wanted to do.

Note that all data are natural numbers, so 'subtraction' is actually the monus operator: x monus y = x - y if x > y and 0 otherwise.

Changes to the language as defined in the notes:
- 'Programs' are one line. They begin with a variable name and a semicolon, and then consist of a sequence of variable assignments separated by semicolons. The interpreter outputs the value of the initially named variable after the assignments are made in order.
- All binary operators are right-associative, so 3 monus 5 + 7 = 3 monus (5 + 7) = 0.
- The assignment operator is written '='
- You can use parentheses in arithmetic expressions like (3 monus 5) + 7, which evaluates to 7.
