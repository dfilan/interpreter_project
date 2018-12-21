-- exports a bunch of parsers for things that are single tokens.
module Tokenise
       ( natural
       , varName
       , rutnName
       , lpop
       , hpop
       , assign
       , sem
       , pal
       , par
       , kel
       , ker
       , com
       , ifToken
       , elseToken
       , whileToken
       , returnToken
       , mainToken
       ) where

import Numeric.Natural

import Data.Char
import Data.List

import Control.Monad

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

import Types

-- helper functions
singleCharToken :: Token -> Char -> Parser Token
singleCharToken tok = ((>>) spaces) . (fmap (\_ -> tok)) . char

stringToken :: Token -> String -> Parser Token
stringToken tok = ((>>) spaces) . (fmap (\_ -> tok)) . string

stringToNatural :: String -> Natural
stringToNatural = fromIntegral . digitsToNum . (map digitToInt)

digitsToNum :: [Int] -> Int
digitsToNum = foldl (\acc n -> n + 10 * acc) 0

-- token parsers
natural :: Parser Natural
natural = spaces >> (stringToNatural <$> (many1 digit))

varName :: Parser VarName
varName = spaces >> ((liftM2 (:) lower (many alphaNum)))

rutnName :: Parser RutnName
rutnName = spaces >> (liftM2 (:) upper (many alphaNum))

lpop :: Parser LowPrioOp
lpop = plus <|> monus

plus :: Parser LowPrioOp
plus = spaces >> ((\_ -> Plus) <$> (char '+'))

monus :: Parser LowPrioOp
monus = spaces >> ((\_ -> Monus) <$> (char '-'))

hpop :: Parser HighPrioOp
hpop = spaces >> ((\_ -> Times) <$> (char '*'))

assign :: Parser Token
assign = stringToken Assign ":="

sem :: Parser Token
sem = singleCharToken Sem ';'

pal :: Parser Token
pal = singleCharToken Pal '('

par :: Parser Token
par = singleCharToken Par ')'

kel :: Parser Token
kel = singleCharToken Kel '{'

ker :: Parser Token
ker = singleCharToken Ker '}'

com :: Parser Token
com = singleCharToken Com ','

ifToken :: Parser Token
ifToken = stringToken If "if"

elseToken :: Parser Token
elseToken = stringToken Else "else"

whileToken :: Parser Token
whileToken = stringToken While "while"

returnToken :: Parser Token
returnToken = stringToken Return "return"

mainToken :: Parser Token
mainToken = stringToken Main "main"
