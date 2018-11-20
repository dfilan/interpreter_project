-- exports stringToTokens, which takes a string and turns it into a list of
-- tokens
module Tokenise
       ( stringToTokens
       ) where

import Numeric.Natural
import Data.Char
import Data.List

import Types

-- TODO: rename "mToken" etc.

-- take the input, get a token and the offest to next token. Fail if the
-- character doesn't represent a valid token.
getToken :: String -> Eval (Token, Int)
getToken str
    | str == []    = Right (EOF, 0)
    | isDigit char = readNat str
    | char == '+'  = Right (LPOp Plus, 1)
    | char == '-'  = Right (LPOp Monus, 1)
    | char == '*'  = Right (HPOp Times, 1)
    | char == ' '  = fmap (\(a,b) -> (a,b+1)) $ getToken $ tail str
    | char == ':'  = readAssign str
    | char == ';'  = Right (Sem, 1)
    | char == '('  = Right (Pal, 1)
    | char == ')'  = Right (Par, 1)
    | char == '{'  = Right (Kel, 1)
    | char == '}'  = Right (Ker, 1)
    | otherwise    = readAlphas str
    where char = head str

-- special function for reading ':=', the assignment operator
readAssign :: String -> Eval (Token, Int)
readAssign str
    | length str < 2                = colonError
    | first == ':' && second == '=' = Right (Assign, 2)
    | otherwise                     = colonError
    where first      = head str
          second     = head $ tail str
          colonError = Left "Typed colon without subsequent equals sign."

-- special function for reading alphabetical sections
readAlphas :: String -> Eval (Token, Int)
readAlphas str
    | length name == 0         = Left "Used disallowed character, or character\
                                       \ in disallowed context (e.g. equals\
                                       \ sign without a colon)."
    | isPrefixOf "if" name     = Right (If, 2)
    | isPrefixOf "while" name  = Right (While, 5)
    | isPrefixOf "return" name = Right (Return, 6)
    | otherwise                = Right (Var name, (length name))
  where name = getAlphas str

getAlphas :: String -> String
getAlphas str = takeWhile isAlpha str

-- special function for reading natural numbers
readNat :: String -> Eval (Token, Int)
readNat str = Right (Nat n, diff)
  where (n, diff) = readNat' str

readNat' :: String -> (Natural, Int)
readNat' str = makePair (fromIntegral . digitsToNum) length $ getDigits str

makePair :: (c -> a) -> (c -> b) -> c -> (a,b)
makePair f g x = (f x, g x)

getDigits :: String -> [Int]
getDigits str = map digitToInt $ takeWhile isDigit str

digitsToNum :: [Int] -> Int
digitsToNum = foldl (\acc n -> n + 10 * acc) 0

-- turn input into a list of tokens
stringToTokens :: String -> Eval [Token]
stringToTokens str
    | mToken == Right EOF = Right [EOF]
    | otherwise           = (helper mToken
                             $ ((fmap drop mNextPos) <*> (Right str)
                                >>= stringToTokens))
  where mToken   = fmap fst $ getToken str
        mNextPos = fmap snd $ getToken str

helper :: Eval a -> Eval [a] -> Eval [a]
helper maybeToken maybeList = fmap (:) maybeToken <*> maybeList
