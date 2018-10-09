-- all the types we'll be exporting to other files
-- note: the structure of some of these types closely relates to the grammar we
-- will allow
module Types
       ( LowPrioOp(..)
       , HighPrioOp(..)
       , lpOpFunc
       , hpOpFunc
       , VarName
       , ScopeTable
       , Connective(..)
       , Token(..)
       , Program
       , Assignment
       , Expression(..)
       , Term(..)
       , Atom(..)
       ) where

import Numeric.Natural
import qualified Data.HashMap.Lazy as HM

-- data types for operations by how much precedence they have
data LowPrioOp = Plus | Monus deriving (Eq)
data HighPrioOp = Times deriving (Eq)

showLowPrioOp :: LowPrioOp -> String
showLowPrioOp Plus  = "+"
showLowPrioOp Monus = "-"

instance Show LowPrioOp where
  show = showLowPrioOp

showHighPrioOp :: HighPrioOp -> String
showHighPrioOp Times = "*"

instance Show HighPrioOp where
  show = showHighPrioOp

-- functions to take operators and return the operations that they represent
lpOpFunc :: LowPrioOp -> Natural -> Natural -> Natural
lpOpFunc Plus m n  = m + n
lpOpFunc Monus m n
    | m > n     = m - n
    | otherwise = 0

hpOpFunc :: HighPrioOp -> Natural -> Natural -> Natural
hpOpFunc Times m n = m * n

-- defining a data type for variable names.
-- secretly, they're actually going to be strings that consist entirely of
-- alphabetic unicode characters
type VarName = String

-- while we're interpreting the program, we're going to keep a scope table
-- associating variable names with the values that they hold
type ScopeTable = HM.HashMap VarName Natural

-- defining a data type for "connective symbols"
data Connective = Equals | Semi deriving (Eq)

showCon :: Connective -> String
showCon Equals = "="
showCon Semi   = ";"

instance Show Connective where
  show = showCon

-- defining a data type for tokens, where EOF means end of file.
data Token = Nat Natural
           | LPOp LowPrioOp
           | HPOp HighPrioOp
           | Var VarName
           | Con Connective
           | EOF
           deriving (Eq, Show)

-- data types representing the grammar of programs that we accept
type Program = (VarName, [Assignment])
type Assignment = (VarName, Expression)
data Expression = Expr Term | ExprComb Term LowPrioOp Expression
                deriving (Eq, Show)
data Term = Trm Atom | TrmComb Atom HighPrioOp Term
          deriving (Eq, Show)
data Atom = NatAtom Natural | VarAtom VarName deriving (Eq, Show)
