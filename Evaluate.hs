-- Evaluates a list of tokens by forming a program out of it and then evaluating
-- the program.
module Evaluate
       ( evalTokens
       ) where

import Numeric.Natural
import Control.Applicative
import Control.Monad
import qualified Data.HashMap.Lazy as HM

import Types
import Parse

-- evaluates an atom
evalAtom :: Atom -> ScopeTable -> Eval Natural
evalAtom (NatAtom n) _     = Right n
evalAtom (VarAtom v) scope = evalLookup $ HM.lookup v scope

evalLookup :: Maybe a -> Eval a
evalLookup (Just n) = Right n
evalLookup Nothing  = Left "Tried to look up value of non-existent variable."

-- evaluates a term
evalTerm :: Term -> ScopeTable -> Eval Natural
evalTerm (Trm atom) scope            = evalAtom atom scope
evalTerm (TrmComb atom f term) scope = (liftA2 (hpOpFunc f)
                                        (evalAtom atom scope)
                                        (evalTerm term scope))
evalTerm (ParenTrm expr) scope       = evalExpr expr scope

-- evaluate an expression
evalExpr :: Expression -> ScopeTable -> Eval Natural
evalExpr (Expr term) scope               = evalTerm term scope
evalExpr (ExprComb term lpOp expr) scope = (liftA2 (lpOpFunc lpOp)
                                            (evalTerm term scope)
                                            (evalExpr expr scope))

-- take an assignment and a scope, and update the scope with the assignment
-- but return an error if the RHS of the assignment is ill-defined
updateScope :: (VarName, Expression) -> ScopeTable -> Eval ScopeTable
updateScope (v, e) scope = (\n -> HM.insert v n scope) <$> (evalExpr e scope)

-- evaluates a program
evalProg :: Eval ScopeTable -> Program -> Eval Natural
evalProg _ []                             = Left "Tried to evaluate an empty\
                                                  \ program."
evalProg eScope ((Assn ass):sts)          = (evalProg (eScope >>= (updateScope
                                                                   ass))
                                             sts)
evalProg eScope ((IfStmt v sts1):sts2)    = case (eScope >>= (evalLookup .
                                                              (HM.lookup v))) of
                                             Left msg -> Left msg
                                             Right 0  -> evalProg eScope sts2
                                             Right _  -> (evalProg eScope
                                                          (sts1 ++ sts2))
evalProg eScope ((WhileStmt v sts1):sts2) = case (eScope >>= (evalLookup .
                                                              (HM.lookup v))) of
                                             Left msg -> Left msg
                                             Right 0  -> evalProg eScope sts2
                                             Right _  -> (evalProg eScope
                                                          (sts1 ++ (WhileStmt v
                                                                    sts1):sts2))
evalProg eScope ((ReturnStmt v):_)        = eScope >>= (evalLookup
                                                        . (HM.lookup v))
evalProg eScope (NoOp:ts)                 = evalProg eScope ts

-- evaluate the list of tokens by turning them into a program and then
-- evaluating that
evalTokens :: [Token] -> Eval Natural
evalTokens tokens = (progrify tokens) >>= (evalProg (Right HM.empty))
