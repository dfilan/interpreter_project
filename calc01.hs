-- simple calculator thing
-- TODO: handle invalid input correctly

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

-- take the input and a position, get a token and the next position. Fail if the
-- character doesn't represent a valid token.
getToken :: Input -> Position -> Maybe (Token, Position)
getToken str pos
    | pos > len - 1 = Just (EOF, pos + 1)
    | isDigit char  = Just (Nat $ fromIntegral $ digitToInt char, pos + 1)
    | char == '+'   = Just (Op Plus, pos + 1)
    | otherwise     = Nothing
    where char = str !! pos
          len  = length str

-- turn input into a list of tokens
stringToTokens :: Input -> [Maybe Token]
stringToTokens str = stringToTokens' str 0

stringToTokens' :: Input -> Position -> [Maybe Token]
stringToTokens' str n
    | token == EOF = [Just EOF]
    | otherwise    = token : stringToTokens' str nextPos
    where token   = fmap fst $ getToken str n
          nextPos = fmap snd $ getToken str n -- doesn't work, this returns a
                                              -- Maybe Position

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
