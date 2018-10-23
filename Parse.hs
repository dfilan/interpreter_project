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
termify ((Punct Pal):ts)      = fmap ParenTrm $ exprify $ getInParens 1 ts
termify _                     = Nothing

-- Note: this does something weird when the first argument is a negative number
getInParens :: Integer -> [Token] -> [Token]
getInParens _ []               = []
getInParens 0 ((Punct Pal):ts) = getInParens 1 ts
getInParens n ((Punct Pal):ts) = (Punct Pal):(getInParens (n+1) ts)
getInParens 1 ((Punct Par):ts) = []
getInParens n ((Punct Par):ts) = (Punct Par):(getInParens (n-1) ts)
getInParens n (t:ts)           = t:(getInParens n ts)

dropParens :: Integer -> [Token] -> [Token]
dropParens _ []               = []
dropParens 0 ((Punct Pal):ts) = dropParens 1 ts
dropParens n ((Punct Pal):ts) = dropParens (n+1) ts
dropParens 1 ((Punct Par):ts) = ts
dropParens n ((Punct Par):ts) = dropParens (n-1) ts
dropParens n (t:ts)           = dropParens n ts

-- takes a sequence of tokens, and if they form an expression, see what
-- expression it is.
exprify :: [Token] -> Maybe Expression
exprify tokens
    | restTokens == []         = fmap Expr mTerm
    | nextToken == EOF         = fmap Expr mTerm
    | nextToken == Punct Semi  = fmap Expr mTerm
    | otherwise                = ((fmap ExprComb mTerm) <*> (getLPOp nextToken)
                                  <*> (exprify $ tail restTokens))
    where mTerm      = termify $ takeFirstTerm tokens
          restTokens = dropFirstTerm tokens
          nextToken  = head restTokens

takeFirstTerm :: [Token] -> [Token]
takeFirstTerm ((Nat n):ts)     = (Nat n):(takeFirstTerm ts)
takeFirstTerm ((Var v):ts)     = (Var v):(takeFirstTerm ts)
takeFirstTerm ((HPOp f):ts)    = (HPOp f):(takeFirstTerm ts)
takeFirstTerm ((Punct Pal):ts) = ((Punct Pal):(getInParens 1 ts)
                                  ++ (Punct Par):(takeFirstTerm ts))
takeFirstTerm (_:ts)           = []
takeFirstTerm []               = []

dropFirstTerm :: [Token] -> [Token]
dropFirstTerm ((Nat n):ts)     = dropFirstTerm ts
dropFirstTerm ((Var v):ts)     = dropFirstTerm ts
dropFirstTerm ((HPOp f):ts)    = dropFirstTerm ts
dropFirstTerm ((Punct Pal):ts) = dropParens 1 ts
dropFirstTerm ts               = ts

getLPOp :: Token -> Maybe LowPrioOp
getLPOp (LPOp f) = Just f
getLPOp _        = Nothing

-- take a sequence of tokens, and if they form a variable assignment, return
-- that assignment
assnify :: [Token] -> Maybe Assignment
assnify ((Var v):(Punct Equals):ts) = fmap (\e -> (v, e)) $ exprify ts
assnify _                           = Nothing

-- turn a list of tokens into a program
progrify :: [Token] -> Maybe Program
progrify list =
  case groupLines list of
   [Var v, Punct Semi]:tList -> (fmap (\as -> (v, as)) $ getListAssns $
                                 dropEOF tList)
   _                         -> Nothing

groupLines :: [Token] -> [[Token]]
groupLines [] = []
groupLines ts = (takeUntil notSemiOrEOF ts):(groupLines (dropUntil
                                                         notSemiOrEOF ts))

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil pred list = (takeWhile pred list) ++ [head $ dropWhile pred list]

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil pred list = tail $ dropWhile pred list

notSemiOrEOF :: Token -> Bool
notSemiOrEOF t = notElem t [Punct Semi, EOF]

dropEOF :: [[Token]] -> [[Token]]
dropEOF []         = []
dropEOF ([EOF]:ts) = []
dropEOF (t:ts)     = t:(dropEOF ts)

getListAssns :: [[Token]] -> Maybe [Assignment]
getListAssns []    = Just []
getListAssns tList = case assnify $ head tList of
                      Nothing   -> Nothing
                      Just assn -> fmap ((:) assn) $ getListAssns $ tail tList
