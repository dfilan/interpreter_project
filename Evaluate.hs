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


-- evaluate an expression
evalExpr :: Expression -> ScopeTable -> Maybe Natural
evalExpr (Expr term) scope               = evalTerm term scope
evalExpr (ExprComb term lpOp expr) scope = (liftA2 (lpOpFunc lpOp)
                                            (evalTerm term scope)
                                            (evalExpr expr scope))

-- take an assignment and a scope, and update the scope with the assignment
-- but return nothing if the RHS of the assignment is ill-defined
updateScope :: ScopeTable -> Assignment -> Maybe ScopeTable
updateScope scope (v, e) = fmap (\n -> HM.insert v n scope) (evalExpr e scope)

-- take a list of assignments and a scope, and return the scope after all the
-- assignments have been made
evalAssns :: [Assignment] -> ScopeTable -> Maybe ScopeTable
evalAssns assns scope = foldM updateScope scope assns
  
-- take a program and a scope, and return the value of the stated variable after
-- all the assignments have been made in order
evalProgram :: ScopeTable -> Program -> Maybe Natural
evalProgram scope (v, assns) = (evalAssns assns scope) >>= (HM.lookup v)

-- evaluate the list of tokens by turning them into a program and then
-- evaluating that
evalTokens :: [Token] -> Maybe Natural
evalTokens tokens = (progrify tokens) >>= (evalProgram HM.empty)
