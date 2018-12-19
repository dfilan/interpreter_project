{-# LANGUAGE LambdaCase, TupleSections #-}

-- exports progify, which takes a list of tokens and returns a valid program
-- (as defined in Types).
module Parse
       ( progify
       , rutnLookup
       ) where

import Types
import qualified Data.HashMap.Lazy as HM

-- takes a sequence of tokens, and if the initial segment forms an atom, sees
-- what atom it is
atomify :: [Token] -> Eval Atom
atomify = \case{
    (Nat n):_   -> Right (NatAtom n);
    (Var v):_   -> Right (VarAtom v);
    (Rutn r):ts -> RutnAtom r <$> (getInParens 0 ts >>= splitByCommas
                                   >>= mapM exprify);
    _           -> Left "Tried to read a non-atom as an atom. Perhaps\
                         \ something other than an atom was used as an\
                         \ argument to a routine?";
                           }

splitByCommas :: [Token] -> Eval [[Token]]
splitByCommas [] = Right []
splitByCommas ts = do {
  (first, second) <- splitByFirstComma ts;
  recurse         <- splitByCommas second;
  return (first:recurse);
  }

splitByFirstComma :: [Token] -> Eval ([Token], [Token])
splitByFirstComma []     = Right ([],[])
splitByFirstComma (t:ts) = case t of {
  Com -> Right ([],ts);
  Pal -> do {
    contents        <- getInParens 1 ts;
    rest            <- dropParens 1 ts;
    (first, second) <- splitByFirstComma rest;
    return ([Pal] ++ contents ++ [Par] ++ first, second);
    };
  t   -> mapFst ((:) t) <$> (splitByFirstComma ts);
  }

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

-- takes a sequence of tokens, and if the initial segment forms a term, then
-- sees what term it is
termify :: [Token] -> Eval Term
termify = \case{
  (Nat n):ts      -> case ts of {
    (HPOp f):_ -> (TrmComb (NatAtom n) f) <$> (termify $ tail ts);
    _          -> Right (Trm $ NatAtom n);
    };
  (Var v):ts      -> case ts of {
    (HPOp f):_ -> (TrmComb (VarAtom v) f) <$> (termify $ tail ts);
    _          -> Right (Trm $ VarAtom v);
    };
  (Rutn r):Pal:ts -> let input = ((Rutn r):Pal:ts)
                     in case dropParens 1 ts of {
                       Right ((HPOp f):_) -> do {
                          rest     <- dropParens 1 ts;
                          nextTerm <- termify $ tail rest;
                          rutnAtom <- atomify input;
                          return (TrmComb rutnAtom f nextTerm);
                          };
                       _                  -> Trm <$> atomify input;
                       };
  Pal:ts          -> ParenTrm <$> ((getInParens 1 ts) >>= exprify);
  _               -> Left "Tried to make a term out of something that is not a\
                           \term.";
               }

-- first Token argument is left bracket, next Token argument is right bracket
-- first component of output is the material inside brackets,
-- second component is the list of tokens after the brackets. 
-- does something weird when the integer argument is a negative number
splitByBrackets :: Token -> Token -> Integer -> [Token] -> Eval ([Token],
                                                                 [Token])
splitByBrackets l r n ts
    | ts == []               = Left "Mismatched brackets, or no brackets where\
                                     \ they were expected."
    | n == 0 && head ts == l = splitByBrackets l r 1 $ tail ts
    | head ts == l           = (mapFst $ (:) l) <$> splitBrackets (n+1)
    | n == 1 && head ts == r = Right ([], tail ts)
    | head ts == r           = (mapFst $ (:) r) <$> splitBrackets (n-1)
    | otherwise              = (mapFst $ (:) $ head ts) <$> splitBrackets n
    where splitBrackets = \n -> splitByBrackets l r n $ tail ts

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
    | eNextToken == Right Sem = Expr <$> eTerm
    | otherwise               = ((ExprComb <$> eTerm)
                                <*> (eNextToken >>= getLPOp)
                                <*> (eRestTokens >>= (exprify . tail)))
  where eTerm       = (takeFirstTerm tokens) >>= termify
        eRestTokens = dropFirstTerm tokens
        eNextToken  = head <$> eRestTokens

splitByFirstTerm :: [Token] -> Eval ([Token], [Token])
splitByFirstTerm []     = Right ([],[])
splitByFirstTerm (t:ts) = case t of {
  Nat n  -> consRecurse $ Nat n;
  Var v  -> consRecurse $ Var v;
  Rutn r -> consRecurse $ Rutn r;
  HPOp f -> consRecurse $ HPOp f;
  Pal    -> do {
    palConsContent <- ((:) Pal) <$> (getInParens 1 ts);
    parConsRecurse <- (mapFst $ (:) Par) <$> ((dropParens 1 ts)
                                              >>= splitByFirstTerm);
    Right (mapFst ((++) palConsContent) parConsRecurse);
    };
  _      -> Right ([], t:ts);
  }
    where consRecurse = (\t -> (mapFst $ (:) t) <$> (splitByFirstTerm ts))

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
stmtify = \case{
  (Var v):Assign:ts            -> (Assn v) <$> (exprify ts);
  If:Pal:(Var v):Par:Kel:ts    -> do {
    thenTokens <- getInBraces 1 ts;
    thenBlock  <- blocify thenTokens;
    afterThen  <- dropBraces 1 ts;
    elseBlock  <- case afterThen of {
      Else:ts' -> (getInBraces 0 ts') >>= blocify;
      _        -> Right [];
      };
    return $ ITEStmt v thenBlock elseBlock;
    };
  While:Pal:(Var v):Par:Kel:ts -> (WhileStmt v) <$> ((getInBraces 1 ts)
                                                     >>= blocify);
  Return:ts                    -> ReturnStmt <$> (exprify ts) ;
  _                            -> Left "Tried to make a statement out of\
                                        \ something that isn't a statement.";
               }

-- turn a list of tokens into a block
blocify ::  [Token] -> Eval Block
blocify ts = (group' ts) >>= (mapM stmtify)

-- filter the output of group to only include 'statements' that aren't just a
-- single sem
group' :: [Token] -> Eval [[Token]]
group' = (fmap $ filter $ (/=) [Sem]) . group

-- group tokens by clumping all the ones that form a single statement together
group :: [Token] -> Eval [[Token]]
group [] = Right []
group ts = ((:) <$> (fst <$> (splitByFirstStmt ts))
            <*> ((snd <$> (splitByFirstStmt ts)) >>= group))

-- take a list of tokens, return a tuple. first element is a list of tokens
-- comprising the first statement, second element is a list of all the other
-- tokens
splitByFirstStmt :: [Token] -> Eval ([Token], [Token])
splitByFirstStmt = \case{
  []     -> Right ([], []);
  Sem:ts -> Right ([Sem], ts);
  Kel:ts -> do {
    (braceContents, afterBraces) <- splitByBrackets Kel Ker 1 ts;
    let bracedContents = (Kel:braceContents) ++ [Ker]
    in case afterBraces of {
      Else:ts' -> do {
         (elseContents, afterElse) <- splitByBrackets Kel Ker 0 ts';
         return (bracedContents ++ [Else, Kel] ++ elseContents ++ [Ker],
                 afterElse)
         };
      _        -> return (bracedContents, afterBraces)
      };
    };
  t:ts   -> (mapFst $ (:) t) <$> (splitByFirstStmt ts);
  }

-- take a list of tokens, and if the initial segment forms a routine, return
-- that routine.
rutnify :: [Token] -> Eval (RutnName, Routine)
rutnify ((Rutn r):Pal:ts) = (r,) <$> eRutn
  where eArgList = getInParens 1 ts >>= splitByCommas >>= mapM getVarName
        eBlock   = dropParens 1 ts >>= getInBraces 0 >>= blocify
        eRutn    = (,) <$> eArgList <*> eBlock
rutnify _                 = Left "Tried to make a routine out of a segment that\
                                  \ didn't start with a routine name followed\
                                  \ by an open paren."

getVarName :: [Token] -> Eval VarName
getVarName [Var v] = Right v
getVarName _       = Left "Expected a variable name, didn't get one."

-- takes a list of tokens and returns a program
progify :: [Token] -> Eval Program
progify (Main:(Rutn r):ts) = do {
  table   <- progify' HM.empty ((Rutn r):ts);
  routine <- rutnLookup r table;
  return (routine, table);
  }
progify _                  = Left "Expected first routine to be main."

-- look up a routine, returning something in Eval Routine rather than Maybe
-- Routine
rutnLookup :: RutnName -> RutnTable -> Eval Routine
rutnLookup r tab = case (HM.lookup r tab) of {
  Just rutn -> Right rutn;
  Nothing   -> Left "Tried to look up non-existent routine."
  }

-- take a routine table and a list of tokens
-- get the first routine, rutnify it, put it into the routine table, then
-- progify' the rest with the updated routine table
-- do this until you get an empty string
progify' :: RutnTable -> [Token] -> Eval RutnTable
progify' rt [] = Right rt
progify' rt ts    = do {
  split       <- splitByFirstRutn ts;
  newRutnPair <- rutnify $ fst split;
  progify' (uncurry HM.insert newRutnPair rt) $ snd split
  }
                    
-- take a list of tokens, return a tuple. first element is a list of tokens
-- comprising the first routine, second element is a list of all the other
-- tokens
splitByFirstRutn :: [Token] -> Eval ([Token], [Token])
splitByFirstRutn = \case{
  []              -> Right ([], []);
  (Rutn r):Pal:ts -> do {
    parenSplit <- splitByBrackets Pal Par 1 ts;
    braceSplit <- splitByBrackets Kel Ker 0 $ snd parenSplit;
    return (([Rutn r, Pal] ++ (fst parenSplit) ++ [Par, Kel] ++ (fst braceSplit)
             ++ [Ker]),
            snd braceSplit)
    };
  _               -> Left "Error when dividing code into routines"
  }
