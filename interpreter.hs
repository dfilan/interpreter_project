-- simple interpreter thing.
-- supports natural numbers, addition, multiplication, natural number
-- subtraction (m monus n = max(m-n,0)), parentheses, and variable assignment.
-- these operations are right-associative: x + y + z = x + (y + z)

import Tokenise
import Evaluate

-- take in input. convert it to tokens, then evaluate those tokens as a program.
-- then print the output

main = do
    input <- getLine
    let eTokens = stringToTokens input
        val     = eTokens >>= evalTokens
    putStrLn $ show val
