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
thenSpaces :: a -> Parser a
thenSpaces = \x -> (spaces >> return x)

singleCharToken :: a -> Char -> Parser a
singleCharToken tok = (=<<) thenSpaces . fmap (\_ -> tok) . char

stringToken :: a -> String -> Parser a
stringToken tok = (=<<) thenSpaces . fmap (\_ -> tok) . string

stringToNatural :: String -> Natural
stringToNatural = fromIntegral . digitsToNum . map digitToInt

digitsToNum :: [Int] -> Int
digitsToNum = foldl (\acc n -> n + 10 * acc) 0

-- token parsers
natural :: Parser Natural
natural = stringToNatural <$> many1 digit >>= thenSpaces

varName :: Parser VarName
varName = liftM2 (:) lower (many alphaNum) >>= thenSpaces

rutnName :: Parser RutnName
rutnName = liftM2 (:) upper (many alphaNum) >>= thenSpaces

lpop :: Parser LowPrioOp
lpop = plus <|> monus

plus :: Parser LowPrioOp
plus = singleCharToken Plus '+'

monus :: Parser LowPrioOp
monus = singleCharToken Monus '-'

hpop :: Parser HighPrioOp
hpop = singleCharToken Times '*'

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
ifToken = try $ stringToken If "if"

elseToken :: Parser Token
elseToken = try $ stringToken Else "else"

whileToken :: Parser Token
whileToken = try $ stringToken While "while"

returnToken :: Parser Token
returnToken = try $ stringToken Return "return"

mainToken :: Parser Token
mainToken = try $ stringToken Main "main"
