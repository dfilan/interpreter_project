-- simple calculator thing

import Numeric.Natural
import Data.Char

-- defining a data type for the operations we can have
data OpType = Plus deriving (Eq)

stringifyOp :: OpType -> String
stringifyOp Plus = "+"

instance Show OpType where
  show op = stringifyOp op

-- takes an operator type and returns the operation that it represents
opTypeFunc :: OpType -> Natural -> Natural -> Natural
opTypeFunc Plus = (+)

-- defining a data type for tokens, where EOF means end of file.
data Token = Nat Natural | Op OpType | EOF deriving (Eq)

stringifyToken :: Token -> String
stringifyToken (Nat x) = "Token Nat " ++ show x
stringifyToken (Op x)  = "Token Op " ++ show x
stringifyToken EOF     = "Token EOF"

instance Show Token where
  show token = stringifyToken token

-- data types for input strings and positions
type Input = String
type Position = Int

-- take the input and a position, get a token and the next position.
getToken :: Input -> Position -> (Token, Position)
getToken str pos
    | pos > len - 1 = (EOF, pos + 1)
    | isDigit char  = (Nat $ fromIntegral $ digitToInt char, pos + 1)
    | char == '+'   = (Op Plus, pos + 1)
    where char = str !! pos
          len  = length str

-- what we should do:
-- take a string. get a sequence of tokens.
-- then, if there are two natural number tokens separated by an op, apply the
-- op to the natural numbers

-- turn input into a list of tokens
stringToTokens :: Input -> [Token]
stringToTokens str = stringToTokens' str 0

stringToTokens' :: Input -> Position -> [Token]
stringToTokens' str n
    | token == EOF = [EOF]
    | otherwise    = token : stringToTokens' str nextPos
    where token   = fst $ getToken str n
          nextPos = snd $ getToken str n

-- takes a sequence of tokens, and if they form an expression, then compute what
-- they're supposed to compute
-- expression -> natural, op, natural
expr :: [Token] -> Maybe Natural
expr [Nat x, Op f, Nat y, EOF] = Just $ (opTypeFunc f) x y
expr _                         = Nothing


-- take in input. convert it to tokens, then check what that's expressed as.
-- then, convert the result into a string, and print out that string.
main = do
    input <- getLine
    let tokens = stringToTokens input
        val    = show $ expr tokens
    putStrLn val
