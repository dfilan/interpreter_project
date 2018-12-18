-- simple interpreter thing.
-- supports natural numbers, addition, multiplication, natural number
-- subtraction (m monus n = max(m-n,0)), parentheses, and variable assignment.
-- these operations are right-associative: x + y + z = x + (y + z)

import Text.Read
import System.Environment
import Numeric.Natural

import Types
import Tokenise
import Parse
import Evaluate

readNatList :: String -> Eval [Natural]
readNatList = readEither

readProgram :: String -> Eval Program
readProgram str = (stringToTokens str) >>= progify

runProgram :: String -> Eval [Natural] -> Eval Natural
runProgram str eNats = do {
  nats <- eNats;
  prog <- readProgram str;
  evalProg nats prog;
  }

-- take in input. convert it to tokens, then evaluate those tokens as a program.
-- then print the output

main = do
    args          <- getArgs
    programString <- readFile $ head args
    putStrLn $ show $ runProgram programString $ readNatList (args!!1)
