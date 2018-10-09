-- simple interpreter thing.
-- supports natural numbers, addition, multiplication, and natural number
-- subtraction (monus(m,n) = max(m-n,0)).
-- these operations are right-associative: x + y + z = x + (y + z)

import Numeric.Natural
import Data.Char
import Control.Applicative
import Control.Monad
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

-- take the input and a position, get a token and the next position. Fail if the
-- character doesn't represent a valid token.
getToken :: String -> Maybe (Token, Int)
getToken str
    | str == []    = Just (EOF, 0)
    | isDigit char = readNat str
    | char == '+'  = Just (LPOp Plus, 1)
    | char == '-'  = Just (LPOp Monus, 1)
    | char == '*'  = Just (HPOp Times, 1)
    | char == ' '  = fmap (\(a,b) -> (a,b+1)) $ getToken $ tail str
    | char == '='  = Just (Con Equals, 1)
    | char == ';'  = Just (Con Semi, 1)
    | otherwise    = readVarName str
    where char = head str

-- special function for reading variable names
readVarName :: String -> Maybe (Token, Int)
readVarName str
    | length name == 0 = Nothing
    | otherwise        = Just (Var name, (length name))
    where name = getVarName str

getVarName :: String -> VarName
getVarName str = takeWhile isAlpha str

-- special function for reading natural numbers
readNat :: String -> Maybe (Token, Int)
readNat str = Just (Nat n, diff)
    where (n, diff) = readNat' str

readNat' :: String -> (Natural, Int)
readNat' str = makePair (fromIntegral . digitsToNum) length $ getDigits str

makePair :: (c -> a) -> (c -> b) -> c -> (a,b)
makePair f g x = (f x, g x)

getDigits :: String -> [Int]
getDigits str = map digitToInt $ takeWhile isDigit str

digitsToNum :: [Int] -> Int
digitsToNum = foldl (\acc n -> n + 10 * acc) 0

-- turn input into a list of tokens
stringToTokens :: String -> Maybe [Token]
stringToTokens str
    | mToken == Just EOF = Just [EOF]
    | otherwise          = helper mToken $ ((fmap drop mNextPos) <*> (Just str)
                                            >>= stringToTokens)
    where mToken   = fmap fst $ getToken str
          mNextPos = fmap snd $ getToken str

helper :: Maybe a -> Maybe [a] -> Maybe [a]
helper maybeToken maybeList = fmap (:) maybeToken <*> maybeList

-- evaluates an atom
evalAtom :: Atom -> ScopeTable -> Maybe Natural
evalAtom (NatAtom n) _     = Just n
evalAtom (VarAtom v) scope = HM.lookup v scope

-- takes a sequence of tokens, and if they form a term, then see what term it is
termify :: [Token] -> Maybe Term
termify [Nat x]               = Just (Trm $ NatAtom x)
termify [Var v]               = Just (Trm $ VarAtom v)
termify ((Nat x):(HPOp f):ts) = fmap (TrmComb (NatAtom x) f) $ termify ts
termify ((Var v):(HPOp f):ts) = fmap (TrmComb (VarAtom v) f) $ termify ts
termify _                     = Nothing

-- evaluates a term
evalTerm :: Term -> ScopeTable -> Maybe Natural
evalTerm (Trm atom) scope            = evalAtom atom scope
evalTerm (TrmComb atom f term) scope = (liftA2 (hpOpFunc f)
                                        (evalAtom atom scope)
                                        (evalTerm term scope))

-- takes a sequence of tokens, and if they form an expression, see what
-- expression it is.
exprify :: [Token] -> Maybe Expression
exprify tokens
    | nextToken == EOF      = fmap Expr mTerm
    | nextToken == Con Semi = fmap Expr mTerm
    | otherwise             = ((fmap ExprComb mTerm) <*> (getLPOp nextToken)
                               <*> (exprify $ tail restTokens))
    where mTerm = termify $ takeWhile isTermStuff tokens
          restTokens = dropWhile isTermStuff tokens
          nextToken = head restTokens

isTermStuff :: Token -> Bool
isTermStuff (Nat n)  = True
isTermStuff (Var v)  = True
isTermStuff (HPOp f) = True
isTermStuff _        = False

getLPOp :: Token -> Maybe LowPrioOp
getLPOp (LPOp f) = Just f
getLPOp _        = Nothing

-- evaluate an expression
evalExpr :: Expression -> ScopeTable -> Maybe Natural
evalExpr (Expr term) scope               = evalTerm term scope
evalExpr (ExprComb term lpOp expr) scope = (liftA2 (lpOpFunc lpOp)
                                            (evalTerm term scope)
                                            (evalExpr expr scope))

-- take a sequence of tokens, and if they form a variable assignment, return
-- that assignment
assnify :: [Token] -> Maybe Assignment
assnify ((Var v):(Con Equals):ts) = fmap (\e -> (v, e)) $ exprify ts
assnify _                         = Nothing

-- take an assignment and a scope, and update the scope with the assignment
-- but return nothing if the RHS of the assignment is ill-defined
updateScope :: ScopeTable -> Assignment -> Maybe ScopeTable
updateScope scope (v, e) = fmap (\n -> HM.insert v n scope) (evalExpr e scope)

-- take a list of assignments and a scope, and return the scope after all the
-- assignments have been made
evalAssns :: [Assignment] -> ScopeTable -> Maybe ScopeTable
evalAssns assns scope = foldM updateScope scope assns

-- turn a list of tokens into a program
progrify :: [Token] -> Maybe Program
progrify list =
  case groupLines list of
   [Var v, Con Semi]:tList -> fmap (\as -> (v, as)) $ getListAssns tList
   _                       -> Nothing

groupLines :: [Token] -> [[Token]]
groupLines [] = []
groupLines ts = (takeUntil notSemiOrEOF ts):(groupLines (dropUntil
                                                         notSemiOrEOF ts))

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil pred list = (takeWhile pred list) ++ [head $ dropWhile pred list]

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil pred list = tail $ dropWhile pred list

notSemiOrEOF :: Token -> Bool
notSemiOrEOF t = notElem t [Con Semi, EOF]

getListAssns :: [[Token]] -> Maybe [Assignment]
getListAssns []    = Just []
getListAssns tList = case assnify $ head tList of
                      Nothing   -> Nothing
                      Just assn -> fmap ((:) assn) $ getListAssns $ tail tList
  
-- take a program and a scope, and return the value of the stated variable after
-- all the assignments have been made in order
evalProgram :: ScopeTable -> Program -> Maybe Natural
evalProgram scope (v, assns) = evalAssns assns scope >>= (HM.lookup v)

-- evaluate the list of tokens by turning them into a program and then
-- evaluating that
evalTokens :: [Token] -> Maybe Natural
evalTokens tokens = (progrify tokens) >>= (evalProgram HM.empty)

-- take in input. convert it to tokens, then evaluate those tokens as a program.
-- then print the output
main = do
    input <- getLine
    let mTokens = stringToTokens input
        val     = mTokens >>= evalTokens
    putStrLn $ show val
