-- exports progrify, which takes a list of tokens and returns a valid program
-- (as defined in Types).
module Parse
       ( progrify
       ) where

import Types

-- TODO: write getInBrackets and dropBrackets in nice divide style
-- TODO: rename "mTerm" etc.
-- TODO: throw error when brackets are mismatched

-- takes a sequence of tokens, and if they form a term, then see what term it is
termify :: [Token] -> Eval Term
termify [Nat x]               = Right (Trm $ NatAtom x)
termify [Var v]               = Right (Trm $ VarAtom v)
termify ((Nat x):(HPOp f):ts) = fmap (TrmComb (NatAtom x) f) $ termify ts
termify ((Var v):(HPOp f):ts) = fmap (TrmComb (VarAtom v) f) $ termify ts
termify (Pal:ts)              = fmap ParenTrm $ exprify $ getInParens 1 ts
termify _                     = Left "During interpretation, tried to make a\
                                      \ term out of something that is not a\
                                      \ term."

-- first Token argument is left bracket, next Token argument is right bracket
-- Note: this does something weird when the first integer argument is a negative
-- number
getInBrackets :: Token -> Token -> Integer -> [Token] -> [Token]
getInBrackets l r n ts
   | ts == []               = []
   | n == 0 && head ts == l = getInBrackets l r 1 $ tail ts
   | head ts == l           = l:(getInBrackets l r (n+1) $ tail ts)
   | n == 1 && head ts == r = []
   | head ts == r           = r:(getInBrackets l r (n-1) $ tail ts)
   | otherwise              = (head ts):(getInBrackets l r n $ tail ts)

-- getInBrackets _ _ _ []     = []
-- getInBrackets l r 0 (l:ts) = getInBrackets l r 1 ts
-- getInBrackets l r n (l:ts) = l:(getInBrackets l r (n+1) ts)
-- getInBrackets l r 1 (r:ts) = []
-- getInBrackets l r n (r:ts) = r:(getInBrackets l r (n-1) ts)
-- getInBrackets l r n (t:ts) = t:(getInBrackets l r n ts)

getInParens :: Integer -> [Token] -> [Token]
getInParens = getInBrackets Pal Par

getInBraces :: Integer -> [Token] -> [Token]
getInBraces = getInBrackets Kel Ker

-- first Token argument is left bracket, next Token argument is right bracket
dropBrackets :: Token -> Token -> Integer -> [Token] -> [Token]
dropBrackets l r n ts
   | ts == []               = []
   | n == 0 && head ts == l = dropBrackets l r 1 $ tail ts
   | head ts == l           = dropBrackets l r (n+1) $ tail ts
   | n == 1 && head ts == r = tail ts
   | head ts == r           = dropBrackets l r (n-1) $ tail ts
   | otherwise              = dropBrackets l r n $ tail ts

-- dropBrackets _ _ _ []     = []
-- dropBrackets l r 0 (l:ts) = dropBrackets l r 1 ts
-- dropBrackets l r n (l:ts) = dropBrackets l r (n+1) ts
-- dropBrackets l r 1 (r:ts) = ts
-- dropBrackets l r n (r:ts) = dropBrackets l r (n-1) ts
-- dropBrackets l r n (t:ts) = dropBrackets l r n ts

dropParens :: Integer -> [Token] -> [Token]
dropParens = dropBrackets Pal Par

dropBraces :: Integer -> [Token] -> [Token]
dropBraces = dropBrackets Kel Ker

-- takes a sequence of tokens, and if the initial segment forms an expression,
-- see what expression it is.
exprify :: [Token] -> Eval Expression
exprify tokens
    | restTokens == [] = fmap Expr mTerm
    | nextToken == EOF = fmap Expr mTerm
    | nextToken == Sem = fmap Expr mTerm
    | otherwise        = ((fmap ExprComb mTerm) <*> (getLPOp nextToken)
                          <*> (exprify $ tail restTokens))
    where mTerm      = termify $ takeFirstTerm tokens
          restTokens = dropFirstTerm tokens
          nextToken  = head restTokens

takeFirstTerm :: [Token] -> [Token]
takeFirstTerm ((Nat n):ts)  = (Nat n):(takeFirstTerm ts)
takeFirstTerm ((Var v):ts)  = (Var v):(takeFirstTerm ts)
takeFirstTerm ((HPOp f):ts) = (HPOp f):(takeFirstTerm ts)
takeFirstTerm (Pal:ts)      = Pal:(getInParens 1 ts) ++ Par:(takeFirstTerm ts)
takeFirstTerm (_:ts)        = []
takeFirstTerm []            = []

dropFirstTerm :: [Token] -> [Token]
dropFirstTerm ((Nat _):ts)  = dropFirstTerm ts
dropFirstTerm ((Var _):ts)  = dropFirstTerm ts
dropFirstTerm ((HPOp _):ts) = dropFirstTerm ts
dropFirstTerm (Pal:ts)      = dropParens 1 ts
dropFirstTerm ts            = ts

getLPOp :: Token -> Eval LowPrioOp
getLPOp (LPOp f) = Right f
getLPOp _        = Left "Tried to make a LPOp out of something that isn't one."

-- take a sequence of tokens, and if the initial segment forms a statement,
-- return that statement
stmtify :: [Token] -> Eval Statement
stmtify ((Var v):Assign:ts)            = fmap (\e -> Assn (v,e)) $ exprify ts
stmtify (If:Pal:(Var v):Par:Kel:ts)    = (fmap (\l -> IfStmt v l)
                                          $ progrify
                                          $ getInBraces 1 ts)
stmtify (While:Pal:(Var v):Par:Kel:ts) = (fmap (\l -> WhileStmt v l)
                                          $ progrify
                                          $ getInBraces 1 ts)
stmtify (Return:(Var v):_)             = Right (ReturnStmt v)
stmtify []                             = Right NoOp
stmtify _                              = Left "Tried to make a statement out of\
                                               \ something that isn't a\
                                               \ statement."

-- turn a list of tokens into a program
progrify ::  [Token] -> Eval [Statement]
progrify = (mapM stmtify) . group

-- group tokens by clumping all the ones that form a single statement together
group :: [Token] -> [[Token]]
group [] = []
group ts = (fst $ divideTokens ts):(group $ snd $ divideTokens ts)

-- take a list of tokens, return a tuple. first element is a list of tokens
-- comprising the first statement, second element is a list of all the other
-- tokens
divideTokens :: [Token] -> ([Token], [Token])
divideTokens []       = ([], [])
divideTokens (EOF:_)  = ([], [])
divideTokens (Sem:ts) = ([], ts)
divideTokens (Kel:ts) = (Kel:(getInBraces 1 ts) ++ [Ker], (dropBraces 1 ts))
divideTokens (t:ts)   = mapFst (\l -> t:l) $ divideTokens ts

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)
