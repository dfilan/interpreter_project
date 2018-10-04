-- simple calculator thing
-- TODO: handle invalid input more nicely/monadically/functorially

import Numeric.Natural
import Data.Char

-- defining a data type for the operations we can have
data OpType = Plus | Times | Monus deriving (Eq)

stringifyOp :: OpType -> String
stringifyOp Plus  = "+"
stringifyOp Times = "*"
stringifyOp Monus = "-"

instance Show OpType where
  show op = stringifyOp op

-- takes an operator type and returns the operation that it represents
opTypeFunc :: OpType -> Natural -> Natural -> Natural
opTypeFunc Plus m n  = m + n
opTypeFunc Times m n = m * n
opTypeFunc Monus m n
    | m > n     = m - n
    | otherwise = 0

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
    | char == '*'   = Just (Op Times, pos + 1)
    | char == '-'   = Just (Op Monus, pos + 1)
    | otherwise     = Nothing
    where char = str !! pos
          len  = length str

-- turn input into a list of tokens
stringToTokens :: Input -> Maybe [Token]
stringToTokens str = stringToTokens' str $ Just 0

-- there's probably a better more monadic way of doing this but whatever
stringToTokens' :: Input -> Maybe Position -> Maybe [Token]
stringToTokens' _ Nothing = Nothing
stringToTokens' str (Just n)
    | token == Just EOF = Just [EOF]
    | otherwise         = helper token $ stringToTokens' str nextPos
    where token   = fmap fst $ getToken str n
          nextPos = fmap snd $ getToken str n

helper :: Maybe Token -> Maybe [Token] -> Maybe [Token]
helper maybeToken maybeList = fmap (:) maybeToken <*> maybeList

-- takes a sequence of tokens, and if they form an expression, then compute what
-- they're supposed to compute
-- expression -> natural, op, natural
expr' :: [Token] -> Maybe Natural
expr' [Nat x, Op f, Nat y, EOF] = Just $ (opTypeFunc f) x y
expr' _                         = Nothing

expr :: Maybe [Token] -> Maybe Natural
expr tokens = tokens >>= expr'

-- take in input. convert it to tokens, then check what that's expressed as.
-- then, convert the result into a string, and print out that string.
main = do
    input <- getLine
    let tokens = stringToTokens input
        val    = show $ expr tokens
    putStrLn val
