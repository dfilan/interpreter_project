-- exports progrify, which takes a list of tokens and returns a valid program
-- (as defined in Types).
module Parse
       ( progrify
       ) where

import Types

-- takes a sequence of tokens, and if they form a term, then see what term it is
termify :: [Token] -> Eval Term
termify [Nat x]               = Right (Trm $ NatAtom x)
termify [Var v]               = Right (Trm $ VarAtom v)
termify ((Nat x):(HPOp f):ts) = (TrmComb (NatAtom x) f) <$> termify ts
termify ((Var v):(HPOp f):ts) = (TrmComb (VarAtom v) f) <$> termify ts
-- termify
termify (Pal:ts)              = ParenTrm <$> ((getInParens 1 ts) >>= exprify)
termify _                     = Left "During interpretation, tried to make a\
                                      \ term out of something that is not a\
                                      \ term."

-- first Token argument is left bracket, next Token argument is right bracket
-- first component of output is the material inside brackets,
-- second component is the list of tokens after the brackets. 
-- does something weird when the integer argument is a negative number
splitByBrackets :: Token -> Token -> Integer -> [Token] -> Eval ([Token],
                                                                 [Token])
splitByBrackets l r n ts
    | ts == []               = Left "Mismatched parentheses"
    | n == 0 && head ts == l = splitByBrackets l r 1 $ tail ts
    | head ts == l           = ((mapFst $ (:) l) <$> (splitByBrackets l r (n+1)
                                                      $ tail ts))
    | n == 1 && head ts == r = Right ([], tail ts)
    | head ts == r           = ((mapFst $ (:) r) <$> (splitByBrackets l r (n-1)
                                                      $ tail ts))
    | otherwise              = ((mapFst $ (:) $ head ts)
                                <$> (splitByBrackets l r n $ tail ts))

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

getInParens :: Integer -> [Token] -> Eval [Token]
getInParens n ts = fst <$> (splitByBrackets Pal Par n ts)

getInBraces :: Integer -> [Token] -> Eval [Token]
getInBraces n ts = fst <$> (splitByBrackets Kel Ker n ts)

dropParens :: Integer -> [Token] -> Eval [Token]
dropParens n ts = snd <$> (splitByBrackets Pal Par n ts)

dropBraces :: Integer -> [Token] -> Eval [Token]
dropBraces n ts = snd <$> (splitByBrackets Kel Ker n ts)

-- takes a sequence of tokens, and if the initial segment forms an expression,
-- see what expression it is.
exprify :: [Token] -> Eval Expression
exprify tokens
    | eRestTokens == Right [] = Expr <$> eTerm
    | eNextToken == Right EOF = Expr <$> eTerm
    | eNextToken == Right Sem = Expr <$> eTerm
    | otherwise               = ((ExprComb <$> eTerm)
                                <*> (eNextToken >>= getLPOp)
                                <*> (eRestTokens >>= (exprify . tail)))
  where eTerm       = (takeFirstTerm tokens) >>= termify
        eRestTokens = dropFirstTerm tokens
        eNextToken  = head <$> eRestTokens

splitByFirstTerm :: [Token] -> Eval ([Token], [Token])
splitByFirstTerm ((Nat n):ts)  = ((mapFst $ (:) (Nat n))
                                  <$> (splitByFirstTerm ts))
splitByFirstTerm ((Var v):ts)  = ((mapFst $ (:) (Var v))
                                  <$> (splitByFirstTerm ts))
splitByFirstTerm ((HPOp f):ts) = ((mapFst $ (:) (HPOp f))
                                  <$> (splitByFirstTerm ts))
splitByFirstTerm (Pal:ts)      = do
                               palConsContent <- (((:) Pal)
                                                  <$> (getInParens 1 ts))
                               parConsRecurse <- ((mapFst $ (:) Par)
                                                  <$> ((dropParens 1 ts)
                                                       >>= splitByFirstTerm))
                               (Right (mapFst ((++) palConsContent)
                                       parConsRecurse))
splitByFirstTerm ts            = Right ([], ts)

takeFirstTerm :: [Token] -> Eval [Token]
takeFirstTerm = (fmap fst) . splitByFirstTerm

dropFirstTerm :: [Token] -> Eval [Token]
dropFirstTerm = (fmap snd) . splitByFirstTerm

getLPOp :: Token -> Eval LowPrioOp
getLPOp (LPOp f) = Right f
getLPOp _        = Left "Tried to make a LPOp out of something that isn't one."

-- take a sequence of tokens, and if the initial segment forms a statement,
-- return that statement
stmtify :: [Token] -> Eval Statement
stmtify ((Var v):Assign:ts)            = (\e -> Assn (v,e)) <$> (exprify ts)
stmtify (If:Pal:(Var v):Par:Kel:ts)    = ((\l -> IfStmt v l)
                                          <$> ((getInBraces 1 ts) >>= progrify))
stmtify (While:Pal:(Var v):Par:Kel:ts) = ((\l -> WhileStmt v l)
                                          <$> ((getInBraces 1 ts) >>= progrify))
stmtify (Return:(Var v):_)             = Right (ReturnStmt v)
stmtify [Sem]                          = Right NoOp
stmtify [EOF]                          = Right NoOp
stmtify _                              = Left "Tried to make a statement out of\
                                               \ something that isn't a\
                                               \ statement."

-- turn a list of tokens into a program
progrify ::  [Token] -> Eval [Statement]
progrify ts = (group ts) >>= (mapM stmtify)

-- group tokens by clumping all the ones that form a single statement together
group :: [Token] -> Eval [[Token]]
group [] = Right []
group ts = ((:) <$> (fst <$> (splitByFirstStmt ts))
            <*> ((snd <$> (splitByFirstStmt ts)) >>= group))

-- take a list of tokens, return a tuple. first element is a list of tokens
-- comprising the first statement, second element is a list of all the other
-- tokens
splitByFirstStmt :: [Token] -> Eval ([Token], [Token])
splitByFirstStmt []       = Right ([], [])
splitByFirstStmt (EOF:_)  = Right ([EOF], [])
splitByFirstStmt (Sem:ts) = Right ([Sem], ts)
splitByFirstStmt (Kel:ts) = (,) <$> eBracedContents <*> (dropBraces 1 ts)
  where eBracedContents = (((flip (++) [Ker]) . ((:) Kel))
                           <$> (getInBraces 1 ts))
splitByFirstStmt (t:ts)   = (mapFst $ (:) t) <$> (splitByFirstStmt ts)
