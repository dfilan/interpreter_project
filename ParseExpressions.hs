-- exports a parser for expressions, as well as the convenience functions
-- inParens and inBraces
module ParseExpressions
       ( expr
       , inParens
       , inBraces
       ) where

import Types
import Tokenise

import Text.Parsec
import Text.Parsec.String

-- helper functions

inParens :: Parser a -> Parser a
inParens = between pal par

inBraces :: Parser a -> Parser a
inBraces = between kel ker

-- parser for atoms

atom :: Parser Atom
atom = nat <|> var <|> rutnCall

nat :: Parser Atom
nat = fmap NatAtom natural

var :: Parser Atom
var = fmap VarAtom varName

rutnCall :: Parser Atom
rutnCall = do {
  r    <- rutnName;
  args <- inParens $ sepBy expr com;
  return $ RutnAtom r args;
  }

-- parser for terms
term :: Parser Term
term = (try combTerm) <|> atomTerm <|> exprTerm

atomTerm :: Parser Term
atomTerm = Trm <$> atom

combTerm :: Parser Term
combTerm = TrmComb <$> atom <*> hpop <*> term

exprTerm :: Parser Term
exprTerm = ParenTrm <$> (inParens expr)

-- parser for expressions
expr :: Parser Expression
expr = (try combExpr) <|> termExpr

termExpr :: Parser Expression
termExpr = Expr <$> term

combExpr :: Parser Expression
combExpr = ExprComb <$> term <*> lpop <*> expr
