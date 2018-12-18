{-# LANGUAGE LambdaCase #-}

-- Evaluates a list of tokens and a list of natural numbers by forming a program
-- out of it and then evaluating the program.
module Evaluate
       ( evalProg
       ) where

import Numeric.Natural
import Control.Applicative
import Control.Monad
import qualified Data.HashMap.Lazy as HM

import Types
import Parse

-- evaluates an atom
evalAtom :: ScopeTable -> RutnTable -> Atom -> Eval Natural
evalAtom scTab ruTab = \case{
  NatAtom n       -> Right n;
  VarAtom v       -> varLookup v scTab;
  RutnAtom r args -> do {
    routine   <- rutnLookup r ruTab;
    argValues <- mapM (evalAtom scTab ruTab) args;
    evalRutn ruTab routine argValues
    };
  }

-- look up things from scope tables, get something out that's Eval Natural
-- and not Maybe Natural
varLookup :: VarName -> ScopeTable -> Eval Natural
varLookup v tab = case (HM.lookup v tab) of {
  Just n  -> Right n;
  Nothing -> Left "Tried to look up value of non-existent variable.";
  }

-- evaluates a term
evalTerm :: ScopeTable -> RutnTable -> Term -> Eval Natural
evalTerm scTab ruTab = \case{
  Trm atom            -> evalAtom scTab ruTab atom;
  TrmComb atom f term -> (liftA2 (hpOpFunc f) (evalAtom scTab ruTab atom)
                          $ evalTerm scTab ruTab term);
  ParenTrm expr       -> evalExpr scTab ruTab expr;
  }

-- evaluate an expression
evalExpr :: ScopeTable -> RutnTable -> Expression -> Eval Natural
evalExpr scTab ruTab = \case{
  Expr term            -> evalTerm scTab ruTab term;
  ExprComb term f expr -> (liftA2 (lpOpFunc f) (evalTerm scTab ruTab term)
                           $ evalExpr scTab ruTab expr);
  }

-- evaluate a block
evalBloc :: ScopeTable -> RutnTable -> Block -> Eval Natural
evalBloc scTab ruTab = \case{
  (Assn v e):sts          -> do {
    newScTab <- updateScope scTab ruTab (v,e);
    evalBloc newScTab ruTab sts
    };
  (IfStmt v sts1):sts2    -> do {
    val <- varLookup v scTab;
    case val of {
      0 -> evalBloc scTab ruTab sts2;
      _ -> evalBloc scTab ruTab $ sts1 ++ sts2;
      };
    };
  (WhileStmt v sts1):sts2 -> do {
    val <- varLookup v scTab;
    case val of {
      0 -> evalBloc scTab ruTab sts2;
      _ -> evalBloc scTab ruTab $ sts1 ++ (WhileStmt v sts1):sts2;
      };
    };
  (ReturnStmt v):sts      -> varLookup v scTab;
  NoOp:sts                -> evalBloc scTab ruTab sts;
  []                      -> Left "Tried to evaluate a block with no return\
                                   \ statement."
  }

-- take an assignment and a scope, and update the scope with the assignment
-- but return an error if the RHS of the assignment is ill-defined
updateScope :: (ScopeTable -> RutnTable -> (VarName, Expression)
                -> Eval ScopeTable)
updateScope scTab ruTab (v,e) = (\n -> HM.insert v n scTab) <$> (evalExpr scTab
                                                                 ruTab e)

-- evaluate a routine
evalRutn :: RutnTable -> Routine -> [Natural] -> Eval Natural
evalRutn ruTab (vars, block) args
 | length args == length vars = evalBloc (mkScTab vars args) ruTab block
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
evalProg :: [Natural] -> Program -> Eval Natural
evalProg args (r, ruTab) = evalRutn ruTab r args
