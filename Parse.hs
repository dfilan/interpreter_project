-- exports progrify, which takes a list of tokens and returns a valid program
-- (as defined in Types).
module Parse
       ( progrify
       ) where

import Types

-- takes a sequence of tokens, and if they form a term, then see what term it is
termify :: [Token] -> Maybe Term
termify [Nat x]               = Just (Trm $ NatAtom x)
termify [Var v]               = Just (Trm $ VarAtom v)
termify ((Nat x):(HPOp f):ts) = fmap (TrmComb (NatAtom x) f) $ termify ts
termify ((Var v):(HPOp f):ts) = fmap (TrmComb (VarAtom v) f) $ termify ts
termify _                     = Nothing

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

-- take a sequence of tokens, and if they form a variable assignment, return
-- that assignment
assnify :: [Token] -> Maybe Assignment
assnify ((Var v):(Con Equals):ts) = fmap (\e -> (v, e)) $ exprify ts
assnify _                         = Nothing

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
