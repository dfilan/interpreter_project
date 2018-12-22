{-# LANGUAGE LambdaCase #-}

-- simple interpreter thing.
-- supports natural numbers, addition, multiplication, natural number
-- subtraction (m monus n = max(m-n,0)), parentheses, and variable assignment.
-- these operations are right-associative: x + y + z = x + (y + z)

import Types
import ParsePrograms
import Evaluate

import Text.Read

import Text.Parsec
import Text.Parsec.String

import System.Environment

import Numeric.Natural

readNatList :: String -> Eval [Natural]
readNatList = readEither

main :: IO ()
main = do {
    args    <- getArgs;
    program <- parseFromFile prog $ head args;
    case program of {
      Left err -> print err;
      Right pr -> print $ (readNatList (args!!1)) >>= (evalProg pr);
      };
    }
