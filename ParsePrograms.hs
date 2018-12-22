{-# LANGUAGE TupleSections #-}

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
stmt = iteStatement <|> whileStatement <|> returnStatement <|> assignment

assignment :: Parser Statement
assignment = Assn <$> varName <*> (assign >> expr)

iteStatement :: Parser Statement
iteStatement = (try hasElse) <|> justIfThen

hasElse :: Parser Statement
hasElse = (ITEStmt <$> (ifToken >> inParens expr) <*> inBraces bloc
           <*> (elseToken >> inBraces bloc))

justIfThen :: Parser Statement
justIfThen = (ITEStmt <$> (ifToken >> inParens expr) <*> inBraces bloc
              <*> pure [])

whileStatement :: Parser Statement
whileStatement = WhileStmt <$> (whileToken >> inParens expr) <*> inBraces bloc

returnStatement :: Parser Statement
returnStatement = ReturnStmt <$> (returnToken >> expr)

-- parser for blocks
bloc :: Parser Block
bloc = sepEndBy stmt sem

-- parser for routines and their names
rutn :: Parser (RutnName, Routine)
rutn = (,) <$> rutnName <*> ((,) <$> (inParens $ sepBy varName com)
                             <*> inBraces bloc)

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
