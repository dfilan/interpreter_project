Making an interpreter in Haskell for a simple programming language that was designed for a foundations of mathematics class (see Chapter 8 part C of notes). Was initally following [this](https://ruslanspivak.com/lsbasi-part1/) tutorial for how one would build an interpreter, then just did things I wanted to do.

Note that all data are natural numbers, so 'subtraction' is actually the monus operator: x monus y = x - y if x > y and 0 otherwise. Also, all binary operators are right-associative, so 3 monus 5 + 7 = 3 monus (5 + 7) = 0.
