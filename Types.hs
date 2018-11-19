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
       , Token(..)
       , Program
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
-- alphabetic unicode characters
type VarName = String

-- while we're interpreting the program, we're going to keep a scope table
-- associating variable names with the values that they hold
type ScopeTable = HM.HashMap VarName Natural

-- defining a data type for tokens, where EOF means end of file.
data Token = Nat Natural
           | LPOp LowPrioOp
           | HPOp HighPrioOp
           | Var VarName
           | Assign -- assignment sign, ':='
           | Sem -- semicolon, ;
           | Pal -- left paren, (
           | Par -- right paren, )
           | Kel -- left brace, {
           | Ker -- right brace, }
           | If
           | While
           | Return
           | EOF
           deriving (Eq, Show)

-- data types representing the grammar of programs that we accept
type Program    = [Statement]
-- type Routine    = Routine { isMain  :: Bool
--                           , numArgs :: Natural
--                           , 
--                           }
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
data Atom       = NatAtom Natural | VarAtom VarName
                deriving (Eq, Show)

-- data type that allows for errors that can show up during execution. Because
-- of the structure of the either type, somebody running an invalid program
-- will only see the first error that shows up during execution

type Eval a = Either String a

-- newtype Eval a = Eval (Either String a)
--                deriving (Eq, Show)
                        
-- instance Functor Eval where
--     fmap f (Eval eval) = Eval (fmap f eval)
    
-- instance Applicative Eval where
--     pure v                      = Eval (Right v)
--     (Eval func) <*> (Eval eval) = case func of Left msg -> Eval (Left msg)
--                                                Right f  -> fmap f (Eval eval)

-- instance Monad Eval where
--     (Eval eval) >>= k = case eval of Left msg -> Eval (Left msg)
--                                      Right v  -> k v
--     return v          = Eval (return v)
--     fail msg          = Eval (Left msg)

-- A program is a list of routines, one of which is main.
-- Routines take a finite number of arguments, and return one real number
-- Atoms can be function calls
-- Routines have local scope


-- example routines:
-- main F(n) {
--        v := 1;
--        k := 1;
--        c := n - k;
--        while (c) {
--            k := k + 1;
--            v := v * k;
--            c := n - k;
--               };
--        return v;
-- }

-- G(n) {
--     v := 1;
--     if (n) {
--         x := n - 1;
--         w := G(x);
--         v := v*w;
--            };
--     return v;
--      }
