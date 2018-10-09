-- simple interpreter thing.
-- supports natural numbers, addition, multiplication, and natural number
-- subtraction (monus(m,n) = max(m-n,0)).
-- these operations are right-associative: x + y + z = x + (y + z)

import Numeric.Natural
import Data.Char
import qualified Data.Map.Strict as Map

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
lowPrioOpFunc :: LowPrioOp -> Natural -> Natural -> Natural
lowPrioOpFunc Plus m n  = m + n
lowPrioOpFunc Monus m n
    | m > n     = m - n
    | otherwise = 0

highPrioOpFunc :: HighPrioOp -> Natural -> Natural -> Natural
highPrioOpFunc Times m n = m * n

-- defining a data type for variable names.
-- secretly, they're actually going to be strings that consist entirely of
-- alphabetic unicode characters
type VarName = String

-- defining a data type for "connective symbols"
data Connective = Equals | Semi deriving (Eq)

showCon :: Connective -> String
showCon Equals = "="
showCon Semi = ";"

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
type Program = [Assignment]
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
    | str == []     = Just (EOF, 0)
    | isDigit char  = readNat str
    | char == '+'   = Just (LPOp Plus, 1)
    | char == '-'   = Just (LPOp Monus, 1)
    | char == '*'   = Just (HPOp Times, 1)
    | char == ' '   = getToken $ tail str
    | char == '='   = Just (Con Equals, 1)
    | char == ';'   = Just (Con Semi, 1)
    | otherwise     = readVarName str
    where char = head str

-- special function for reading variable names
readVarName :: String -> Maybe (Token, Int)
readVarName str
    | length name == 0 = Nothing
    | otherwise        = Just (Var name, (length name))
    where name = getVarName str

getVarName :: String -> Int -> VarName
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
    | token == Just EOF = Just [EOF]
    | otherwise         = helper token $ stringToTokens $ drop nextPos str
    where token   = fmap fst $ getToken str
          nextPos = fmap snd $ getToken str

helper :: Maybe a -> Maybe [a] -> Maybe [a]
helper maybeToken maybeList = fmap (:) maybeToken <*> maybeList

-- TODO: fix the rest of this. Deal with maps that associate variable names with
-- values. figure out what happens if you assign to a variable that hasn't been
-- referenced yet - maybe create a global scope or something?

-- takes a sequence of tokens, and if they form a term, then see what term it is
termify :: [Token] -> Maybe Term
termify [Nat x]                  = Just (Trm x)
termify ((Nat x):(HPOp f):terms) = fmap (TrmComb x f) $ termify terms
termify _                        = Nothing

-- evaluates a term
evalTerm :: Term -> Natural
evalTerm (Trm n)            = n
evalTerm (TrmComb n f term) = (highPrioOpFunc f) n $ evalTerm term

-- takes a sequence of tokens, and if they form an expression, see what
-- expression it is.
exprify :: [Token] -> Maybe Expression
exprify tokens
    | nextToken == EOF = fmap Expr mTerm
    | otherwise        = ((fmap ExprComb mTerm) <*> (getLPOp nextToken)
                          <*> (exprify $ tail restTokens))
    where mTerm = termify $ takeWhile isTermStuff tokens
          restTokens = dropWhile isTermStuff tokens
          nextToken = head restTokens

isTermStuff :: Token -> Bool
isTermStuff (Nat n)  = True
isTermStuff (HPOp f) = True
isTermStuff _        = False

getLPOp :: Token -> Maybe LowPrioOp
getLPOp (LPOp f) = Just f
getLPOp _        = Nothing

-- evaluate an expression
evalExpr :: Expression -> Natural
evalExpr (Expr term) = evalTerm term
evalExpr (ExprComb term lpOp expr) = ((lowPrioOpFunc lpOp) (evalTerm term)
                                      (evalExpr expr))

-- evaluate list of tokens by turning them into an expression and then
-- evaluating that
evalTokens :: [Token] -> Maybe Natural
evalTokens = (fmap evalExpr) . exprify

-- take in input. convert it to tokens, then check what that's expressed as.
-- then, convert the result into a string, and print out that string.
main = do
    input <- getLine
    let mTokens = stringToTokens input
        val     = show $ mTokens >>= evalTokens
    putStrLn val
