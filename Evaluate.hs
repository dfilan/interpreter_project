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
evalAtom :: Atom -> ScopeTable -> Maybe Natural
evalAtom (NatAtom n) _     = Just n
evalAtom (VarAtom v) scope = HM.lookup v scope

-- evaluates a term
evalTerm :: Term -> ScopeTable -> Maybe Natural
evalTerm (Trm atom) scope            = evalAtom atom scope
evalTerm (TrmComb atom f term) scope = (liftA2 (hpOpFunc f)
                                        (evalAtom atom scope)
                                        (evalTerm term scope))
evalTerm (ParenTrm expr) scope       = evalExpr expr scope


-- evaluate an expression
evalExpr :: Expression -> ScopeTable -> Maybe Natural
evalExpr (Expr term) scope               = evalTerm term scope
evalExpr (ExprComb term lpOp expr) scope = (liftA2 (lpOpFunc lpOp)
                                            (evalTerm term scope)
                                            (evalExpr expr scope))

-- take an assignment and a scope, and update the scope with the assignment
-- but return nothing if the RHS of the assignment is ill-defined
updateScope :: (VarName, Expression) -> ScopeTable -> Maybe ScopeTable
updateScope (v, e) scope = fmap (\n -> HM.insert v n scope) (evalExpr e scope)

evalProg :: Maybe ScopeTable -> Program -> Maybe Natural
evalProg _ []                             = Nothing
evalProg mScope ((Assn ass):sts)          = (evalProg (mScope >>= (updateScope
                                                                   ass))
                                             sts)
evalProg mScope ((IfStmt v sts1):sts2)    = case (mScope >>= (HM.lookup v)) of
                                             Nothing -> Nothing
                                             Just 0  -> evalProg mScope sts2
                                             Just _  -> (evalProg mScope
                                                         (sts1 ++ sts2))
evalProg mScope ((WhileStmt v sts1):sts2) = case (mScope >>= (HM.lookup v)) of
                                             Nothing -> Nothing
                                             Just 0  -> evalProg mScope sts2
                                             Just _  -> (evalProg mScope
                                                         (sts1 ++ (WhileStmt v
                                                                   sts1):sts2))
evalProg mScope ((ReturnStmt v):_)        = mScope >>= (HM.lookup v)
evalProg mScope (NoOp:ts)                 = evalProg mScope ts

-- evaluate the list of tokens by turning them into a program and then
-- evaluating that
evalTokens :: [Token] -> Maybe Natural
evalTokens tokens = (progrify tokens) >>= (evalProg $ Just HM.empty)
