-- exports a parser for programs

module ParsePrograms
       ( prog
       ) where

import Types
import Tokenise
import ParseExpressions

import Text.Parsec
import Text.Parsec.String

import qualified Data.HashMap.Lazy as HM

-- parser for statements
stmt :: Parser Statement
stmt = assignment <|> iteStatement <|> whileStatement <|> returnStatement

assignment :: Parser Statement
assignment = do {
  assignedVar  <- varName;
  assign;
  assignedExpr <- expr;
  return $ Assn assignedVar assignedExpr;
  }

iteStatement :: Parser Statement
iteStatement = (try hasElse) <|> justIfThen

hasElse :: Parser Statement
hasElse = do {
  ifToken;
  branchExpr <- inParens expr;
  thenBlock  <- inBraces bloc;
  elseToken;
  elseBlock  <- inBraces bloc;
  return $ ITEStmt branchExpr thenBlock elseBlock;
  }

justIfThen :: Parser Statement
justIfThen = ifToken >> (ITEStmt <$> (inParens expr) <*> (inBraces bloc)
                         <*> return [])

whileStatement :: Parser Statement
whileStatement = whileToken >> (WhileStmt <$> (inParens expr)
                                <*> (inBraces bloc))

returnStatement :: Parser Statement
returnStatement = returnToken >> (ReturnStmt <$> expr)

-- parser for blocks
bloc :: Parser Block
bloc = sepEndBy stmt sem

-- parser for routines and their names
rutn :: Parser (RutnName, Routine)
rutn = do {
  r    <- rutnName;
  args <- inParens (sepBy varName com);
  body <- inBraces bloc;
  return (r, (args, body));
  }

-- parser for programs
prog :: Parser Program
prog = do {
  mainToken;
  (name, mainRoutine) <- rutn;
  pairList            <- many rutn;
  return $ (mainRoutine, makeRutnTable (name, mainRoutine) pairList);
  }

makeRutnTable :: (RutnName, Routine) -> [(RutnName, Routine)] -> RutnTable
makeRutnTable (name, routine) = foldl (flip $ uncurry HM.insert) (HM.singleton
                                                                  name
                                                                  routine)
