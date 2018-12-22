{-# LANGUAGE LambdaCase #-}

-- Evaluates a list of tokens and a list of natural numbers by forming a program
-- out of it and then evaluating the program.
module Evaluate
       ( evalProg
       ) where

import Types

import Numeric.Natural

import Control.Applicative
import Control.Monad

import qualified Data.HashMap.Lazy as HM

-- evaluates an atom
evalAtom :: RutnTable -> ScopeTable -> Atom -> Eval Natural
evalAtom ruTab scTab = \case{
  NatAtom n       -> Right n;
  VarAtom v       -> varLookup v scTab;
  RutnAtom r args -> do {
    routine   <- rutnLookup r ruTab;
    argValues <- mapM (evalExpr ruTab scTab) args;
    evalRutn ruTab routine argValues;
    };
  }

-- look up things from scope tables, get something out that's Eval Natural
-- and not Maybe Natural
varLookup :: VarName -> ScopeTable -> Eval Natural
varLookup v tab = case (HM.lookup v tab) of {
  Just n  -> Right n;
  Nothing -> Left "Tried to look up value of non-existent variable.";
  }

-- look up a routine, returning something in Eval Routine rather than Maybe
-- Routine
rutnLookup :: RutnName -> RutnTable -> Eval Routine
rutnLookup r tab = case (HM.lookup r tab) of {
  Just rutn -> Right rutn;
  Nothing   -> Left "Tried to look up non-existent routine."
  }

-- evaluates a term
evalTerm :: RutnTable -> ScopeTable -> Term -> Eval Natural
evalTerm ruTab scTab = \case{
  Trm atom            -> evalAtom ruTab scTab atom;
  TrmComb atom f term -> (liftA2 (hpOpFunc f) (evalAtom ruTab scTab atom)
                          $ evalTerm ruTab scTab term);
  ParenTrm expr       -> evalExpr ruTab scTab expr;
  }

-- evaluate an expression
evalExpr :: RutnTable -> ScopeTable -> Expression -> Eval Natural
evalExpr ruTab scTab = \case{
  Expr term            -> evalTerm ruTab scTab term;
  ExprComb term f expr -> (liftA2 (lpOpFunc f) (evalTerm ruTab scTab term)
                           $ evalExpr ruTab scTab expr);
  }

-- evaluate a block
evalBloc :: RutnTable -> ScopeTable -> Block -> Eval Natural
evalBloc ruTab scTab = \case{
  (Assn v e):sts             -> do {
    newScTab <- updateScope ruTab scTab v e;
    evalBloc ruTab newScTab sts;
    };
  (ITEStmt e sts1 sts2):sts3 -> do {
    val <- evalExpr ruTab scTab e;
    case val of {
      0 -> evalBloc ruTab scTab $ sts2 ++ sts3;
      _ -> evalBloc ruTab scTab $ sts1 ++ sts3;
      };
    };
  (WhileStmt e sts1):sts2    -> do {
    val <- evalExpr ruTab scTab e;
    case val of {
      0 -> evalBloc ruTab scTab sts2;
      _ -> evalBloc ruTab scTab $ sts1 ++ (WhileStmt e sts1):sts2;
      };
    };
  (ReturnStmt e):sts         -> evalExpr ruTab scTab e;
  []                         -> Left "Tried to evaluate a block with no return\
                                      \ statement.";
  }

-- take an assignment and a scope, and update the scope with the assignment
-- but return an error if the RHS of the assignment is ill-defined
updateScope :: (RutnTable -> ScopeTable -> VarName -> Expression
                -> Eval ScopeTable)
updateScope ruTab scTab v = (fmap (\n -> HM.insert v n scTab)) . (evalExpr ruTab
                                                                  scTab)

-- evaluate a routine
evalRutn :: RutnTable -> Routine -> [Natural] -> Eval Natural
evalRutn ruTab (vars, block) args
 | length args == length vars = evalBloc ruTab (mkScTab vars args) block
 | otherwise                  = Left "Provided the wrong number of arguments to\
                                      \ a routine."

mkScTab :: [VarName] -> [Natural] -> ScopeTable
mkScTab vars nats = mkScTab' HM.empty $ zip vars nats

mkScTab' :: ScopeTable -> [(VarName, Natural)] -> ScopeTable
mkScTab' scTab = \case{
  []         -> scTab;
  pair:pairs -> mkScTab' (uncurry HM.insert pair scTab) pairs;
  }

-- to evaluate a program, you get the program and the list of naturals it will
-- be applied to, then evaluate the main routine on that list
evalProg :: Program -> [Natural] -> Eval Natural
evalProg (r, ruTab) args = evalRutn ruTab r args
