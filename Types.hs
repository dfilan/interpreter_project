-- TODO (minor): allow else statements
-- TODO (minor): allow expressions to be returned

-- all the types we'll be exporting to other files
-- note: the structure of some of these types closely relates to the grammar we
-- will allow
module Types
       ( LowPrioOp(..)
       , HighPrioOp(..)
       , lpOpFunc
       , hpOpFunc
       , VarName
       , RutnName
       , ScopeTable
       , RutnTable
       , Token(..)
       , Program
       , Routine
       , Statement(..)
       , Expression(..)
       , Term(..)
       , Atom(..)
       , Eval
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
-- alphabetic unicode characters that don't start with upper or title case
type VarName = String

-- defining a data type for routine names.
-- secretly, they're actually going to be strings that consist entirely of
-- alphabetic unicode characters that start with *upper* (or title) case
type RutnName = String

-- while we're interpreting the program, we're going to keep a scope table
-- associating variable names with the values that they hold
type ScopeTable = HM.HashMap VarName Natural

-- we're also going to keep a routine table associating routine names with the
-- routines that they're associated with
type RutnTable = HM.HashMap RutnName Routine

-- defining a data type for tokens, where EOF means end of file.
data Token = Nat Natural
           | LPOp LowPrioOp
           | HPOp HighPrioOp
           | Var VarName
           | Rutn RutnName
           | Assign -- assignment sign, ':='
           | Sem -- semicolon, ;
           | Pal -- left paren, (
           | Par -- right paren, )
           | Kel -- left brace, {
           | Ker -- right brace, }
           | If
           | While
           | Return
           | Main
           | EOF
           deriving (Eq, Show)

-- data types representing the grammar of programs that we accept
type Program = (Routine, [Routine])
-- a program is a main routine and some auxiliary routines.
type Routine = (Natural, [Statement])
-- routines can be main or not, this is encoded in where they're located in the
-- program data structure. Right now, they have a number of arguments,
-- but once there are types there will be instead a list of types of arguments.
-- so maybe currently we can have an arbitrary list whose length is the number
-- of arguments?
-- inside them is their body, which is a list of statements.
-- once we have types, we'll have to say what type they return.
data Statement  = Assn (VarName, Expression)
                | IfStmt VarName [Statement]
                | WhileStmt VarName [Statement]
                | ReturnStmt VarName
                | NoOp
                deriving (Eq, Show)
data Expression = Expr Term | ExprComb Term LowPrioOp Expression
                deriving (Eq, Show)
-- below, ParenTrm Expression means an expression with parens on either side
data Term       = Trm Atom | TrmComb Atom HighPrioOp Term | ParenTrm Expression
                deriving (Eq, Show)
data Atom       = NatAtom Natural | VarAtom VarName | RutnAtom RutnName [Atom]
                deriving (Eq, Show)

-- data type that allows for errors that can show up during execution. Because
-- of the structure of the either type, somebody running an invalid program
-- will only see the first error that shows up during execution

type Eval a = Either String a
