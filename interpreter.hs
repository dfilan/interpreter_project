{-# LANGUAGE LambdaCase #-}

-- simple interpreter thing.
-- supports natural numbers, addition, multiplication, natural number
-- subtraction (m monus n = max(m-n,0)), parentheses, and variable assignment.
-- these operations are right-associative: x + y + z = x + (y + z)

import Text.Read
import System.Environment
import Numeric.Natural
import Text.Parsec
import Text.Parsec.String

import Types
import ParsePrograms
import Evaluate

readNatList :: String -> Eval [Natural]
readNatList = readEither

-- errorToEval :: Either ParseError a -> Eval a
-- errorToEval = \case{
--   Left err -> Left $ show err;
--   Right x  -> Right x;
--   }

-- runProgram :: Program -> Eval [Natural] -> Eval Natural
-- runProgram program eNats = do {
--   naturals <- eNats;
--   evalProg program naturals;
--   }

main :: IO ()
main = do {
    args    <- getArgs;
    program <- parseFromFile prog $ head args;
    case program of {
      Left err -> print err;
      Right pr -> print $ (readNatList (args!!1)) >>= (evalProg pr);
      }
    -- programString <- readFile $ head args;
    -- putStrLn $ show $ runProgram programString $ readNatList (args!!1);
    -- putStrLn $ show programString;
    }
