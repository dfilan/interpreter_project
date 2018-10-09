-- simple interpreter thing.
-- supports natural numbers, addition, multiplication, and natural number
-- subtraction (monus(m,n) = max(m-n,0)).
-- these operations are right-associative: x + y + z = x + (y + z)

import Tokenise
import Evaluate

-- take in input. convert it to tokens, then evaluate those tokens as a program.
-- then print the output
main = do
    input <- getLine
    let mTokens = stringToTokens input
        val     = mTokens >>= evalTokens
    putStrLn $ show val
