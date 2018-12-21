{-# LANGUAGE LambdaCase #-}

-- exports prog, a parser that parses a list of (token, position) tuples
-- into a program.
module Parse
       ( prog
       ) where

import Types

import qualified Data.HashMap.Lazy as HM

import Text.Parsec.Prim
import Text.Parsec.Combinator

-- our type for parsers and tokens
type TokenParser a = Parsec [TokenPos] () a

-- a parser that parses single tokens that pass some test
-- will be used in subsequent parsers
singleToken :: (Token -> Maybe a) -> TokenParser a
singleToken test = token showToken posToken testToken
    where showToken = show . fst
          posToken  = snd
          testToken = test . fst

-- a parser for individual tokens
thisToken :: Token -> TokenParser Token
thisToken myToken = singleToken (\t -> if (t == myToken) then Just myToken
                                       else Nothing)

-- takes an open bracket, close bracket, and a parser, returns a parser that
-- applies that parser between the open and close bracket
inBrackets :: Token -> Token -> TokenParser a -> TokenParser a
inBrackets left right = between (thisToken left) (thisToken right)

-- parse between the contents of parentheses
inParens :: TokenParser a -> TokenParser a
inParens = inBrackets Pal Par

-- parse between the contents of braces
inBraces :: TokenParser a -> TokenParser a
inBraces = inBrackets Kel Ker

-- a parser for natural numbers or variables
natVar :: TokenParser Atom
natVar = singleToken (\case{
                         Nat n -> Just $ NatAtom n;
                         Var v -> Just $ VarAtom v;
                         _     -> Nothing;
                         })

varName :: TokenParser VarName
varName = singleToken (\case{
                          Var v -> Just v;
                          _     -> Nothing;
                          })

-- parser for routine names
rutnName :: TokenParser RutnName
rutnName = singleToken (\case{
                           Rutn r -> Just r;
                           _      -> Nothing;
                           })

-- parser for expressions followed by commas
expressionsCommas :: TokenParser [Expression]
expressionsCommas = sepBy expr (thisToken Com)

-- a parser for routine calls
rutnCall :: TokenParser Atom
rutnCall = do {
  rName       <- rutnName;
  expressions <- inParens expressionsCommas;
  return $ RutnAtom rName expressions;
  }

-- a parser for atoms
atom :: TokenParser Atom
atom = natVar <|> rutnCall

-- a parser for terms
term :: TokenParser Term
term = (try combTerm) <|> atomTerm <|> exprTerm

atomTerm :: TokenParser Term
atomTerm = do {
  firstAtom <- atom;
  return $ Trm firstAtom;
  }

combTerm :: TokenParser Term
combTerm = do {
  firstAtom <- atom;
  hpop      <- hpOp;
  nextTerm  <- term;
  return $ TrmComb firstAtom hpop nextTerm;
  }

hpOp :: TokenParser HighPrioOp
hpOp = singleToken (\case{
                       HPOp f -> Just f;
                       _      -> Nothing;
                       })

exprTerm :: TokenParser Term
exprTerm = do {
  expression <- inParens expr;
  return $ ParenTrm expression;
  }

-- a parser for expressions
expr :: TokenParser Expression
expr = (try combExpr) <|> termExpr

termExpr :: TokenParser Expression
termExpr = do {
  firstTerm <- term;
  return $ Expr firstTerm;
  }

combExpr :: TokenParser Expression
combExpr = do {
  firstTerm  <- term;
  op         <- lpOp;
  expression <- expr;
  return $ ExprComb firstTerm op expression;
  }

lpOp :: TokenParser LowPrioOp
lpOp = singleToken (\case{
                       LPOp f -> Just f;
                       _      -> Nothing;
                       })

-- parser for statements
stmt :: TokenParser Statement
stmt = assignment <|> iteStatement <|> whileStatement <|> returnStatement

assignment :: TokenParser Statement
assignment = do {
  assignedVar  <- varName;
  thisToken Assign;
  assignedExpr <- expr;
  return $ Assn assignedVar assignedExpr;
  }

iteStatement :: TokenParser Statement
iteStatement = (try hasElse) <|> justIfThen

hasElse :: TokenParser Statement
hasElse = do {
  thisToken If;
  branchExpr <- inParens expr;
  thenBlock  <- inBraces bloc;
  thisToken Else;
  elseBlock  <- inBraces bloc;
  return $ ITEStmt branchExpr thenBlock elseBlock;
  }

justIfThen :: TokenParser Statement
justIfThen = do {
  thisToken If;
  branchExpr <- inParens expr;
  thenBlock  <- inBraces bloc;
  return $ ITEStmt branchExpr thenBlock [];
  }

whileStatement :: TokenParser Statement
whileStatement = do {
  thisToken While;
  whileCond  <- inParens expr;
  whileBlock <- inBraces bloc;
  return $ WhileStmt whileCond whileBlock;
  }

returnStatement :: TokenParser Statement
returnStatement = do {
  thisToken Return;
  returnedExpr <- expr;
  return $ ReturnStmt returnedExpr;
  }

-- parser for blocks
bloc :: TokenParser Block
bloc = sepEndBy stmt (thisToken Sem);

-- parser for routines and their names
rutn :: TokenParser (RutnName, Routine)
rutn = do {
  r    <- rutnName;
  args <- inParens (sepBy varName (thisToken Com));
  body <- inBraces bloc;
  return (r,(args, body));
  }

-- parser for programs
prog :: TokenParser Program
prog = do {
  thisToken Main;
  (name, mainRoutine) <- rutn;
  pairList            <- many rutn;
  return $ (mainRoutine, makeRutnTable (name, mainRoutine) pairList);
  }

makeRutnTable :: (RutnName, Routine) -> [(RutnName, Routine)] -> RutnTable
makeRutnTable (name, routine) = foldl (flip $ uncurry HM.insert) (HM.singleton
                                                                  name
                                                                  routine)
