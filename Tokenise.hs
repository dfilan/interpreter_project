-- exports stringToTokens, which takes a string and turns it into a list of
-- tokens
module Tokenise
       ( stringToTokens
       ) where

import Numeric.Natural
import Data.Char
import Data.List

import Types

-- take the input, get a token and the offest to next token. Fail if the
-- character doesn't represent a valid token.
getToken :: String -> Maybe (Token, Int)
getToken str
    | str == []    = Just (EOF, 0)
    | isDigit char = readNat str
    | char == '+'  = Just (LPOp Plus, 1)
    | char == '-'  = Just (LPOp Monus, 1)
    | char == '*'  = Just (HPOp Times, 1)
    | char == ' '  = fmap (\(a,b) -> (a,b+1)) $ getToken $ tail str
    | char == ':'  = readAssign str
    | char == ';'  = Just (Semi, 1)
    | char == '('  = Just (Pal, 1)
    | char == ')'  = Just (Par, 1)
    | otherwise    = readAlphas str
    where char = head str

-- special function for reading ':=', the assignment operator
readAssign :: String -> Maybe (Token, Int)
readAssign str
    | length str < 2                = Nothing
    | first == ':' && second == '=' = Just (Assign, 2)
    | otherwise                     = Nothing
    where first  = head str
          second = head $ tail str

-- special function for reading variable names
readAlphas :: String -> Maybe (Token, Int)
readAlphas str
    | length name == 0         = Nothing
    | isPrefixOf "return" name = Just (Return, 6)
    | otherwise                = Just (Var name, (length name))
    where name = getAlphas str

getAlphas :: String -> String
getAlphas str = takeWhile isAlpha str

-- special function for reading natural numbers
readNat :: String -> Maybe (Token, Int)
readNat str = Just (Nat n, diff)
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
stringToTokens :: String -> Maybe [Token]
stringToTokens str
    | mToken == Just EOF = Just [EOF]
    | otherwise          = helper mToken $ ((fmap drop mNextPos) <*> (Just str)
                                            >>= stringToTokens)
    where mToken   = fmap fst $ getToken str
          mNextPos = fmap snd $ getToken str

helper :: Maybe a -> Maybe [a] -> Maybe [a]
helper maybeToken maybeList = fmap (:) maybeToken <*> maybeList
