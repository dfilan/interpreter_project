-- simple calculator thing

import Numeric.Natural
import Data.Char

-- defining a data type for the operations we can have
data OpType = Plus

stringifyOp :: OpType -> String
stringifyOp Plus = "+"

instance Show OpType where
  show op = stringifyOp op

-- defining a data type for the types of tokens we can have, where EOF means
-- end of file.
data Token = Nat Natural | Op OpType | EOF

stringifyToken :: Token -> String
stringifyToken (Nat x) = "Token(Nat," ++ show x ++ ")"
stringifyToken (Op x)  = "Token(Op," ++ show x ++ ")"
stringifyToken EOF     = "Token(EOF,EOF)"

instance Show Token where
  show token = stringifyToken token

-- data types for input strings and positions
type Input = String
type Position = Int

-- lexical analyser. take a string and a position, get a token and the next
-- position
getToken :: String -> Position -> (Token, Position)
getToken str pos
    | pos > len - 1 = (EOF, nextPos)
    | isDigit char  = (Nat $ fromIntegral $ digitToInt char, nextPos)
    | char == '+'   = (Op Plus, nextPos)
    where char    = str !! pos
          len     = length str
          nextPos = pos + 1
