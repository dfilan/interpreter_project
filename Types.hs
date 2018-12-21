-- TODO: add for loops
-- TODO: use chainl instead of recursion in my expression grammar
-- TODO?: get rid of 'head' from my code
-- TODO?: use List1 in cases where I promise I don't have an empty list
-- TODO: allow comments

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
       , TokenPos
       , Program
       , Block
       , Routine
       , Statement(..)
       , Expression(..)
       , Term(..)
       , Atom(..)
       , Eval
       ) where

import Numeric.Natural
import qualified Data.HashMap.Lazy as HM
import Text.Parsec.Pos

-- data types for operations by how much precedence they have
data LowPrioOp = Plus | Monus deriving (Eq, Show)
data HighPrioOp = Times deriving (Eq, Show)

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

-- defining a data type for tokens
data Token = Nat Natural
           | Var VarName
           | Rutn RutnName
           | LPOp LowPrioOp
           | HPOp HighPrioOp
           | Assign -- assignment sign, ':='
           | Sem -- semicolon, ';'
           | Pal -- left paren, '('
           | Par -- right paren, ')'
           | Kel -- left brace, '{'
           | Ker -- right brace, '}'
           | Com -- comma, ','
           | If
           | Else
           | While
           | Return
           | Main
           deriving (Eq, Show)

type TokenPos = (Token, SourcePos)

-- data types representing the grammar of programs that we accept

-- a program is a main routine and some auxiliary routines.
-- the main routine is kept in the first slot, the routine table in the second
-- slot stores both the main routine and all other routines.
type Program = (Routine, RutnTable)

-- a routine is a list of the routine's arguments, and then a block of
-- statements.
type Routine = ([VarName], Block)

-- we're also going to keep a routine table associating routine names with the
-- routines that they're associated with
type RutnTable = HM.HashMap RutnName Routine

-- a block is just a bunch of statements, one of which should probably be a
-- return statement
type Block = [Statement]

data Statement  = Assn VarName Expression
                | ITEStmt Expression Block Block
                | WhileStmt Expression Block
                | ReturnStmt Expression
                deriving (Eq, Show)

data Expression = Expr Term | ExprComb Term LowPrioOp Expression
                deriving (Eq, Show)

-- below, ParenTrm Expression means an expression with parens on either side
data Term       = Trm Atom | TrmComb Atom HighPrioOp Term | ParenTrm Expression
                deriving (Eq, Show)

data Atom       = NatAtom Natural
                | VarAtom VarName
                | RutnAtom RutnName [Expression]
                deriving (Eq, Show)

-- data type that allows for errors that can show up during execution. Because
-- of the structure of the either type, somebody running an invalid program
-- will only see the first error that shows up during execution
type Eval a = Either String a
