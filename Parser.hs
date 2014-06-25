-- Sisserou -- A linguistic toy based on System F Omega   --
--                                                        --
-- Copyright 2K14 DP Constructions                        --
--            and K.D.P.Ross <KDPRoss@gmail.com>          --
--                                                        --
-- This codebase is licensed for the following purposes   --
-- only:                                                  --
--                                                        --
-- - study of the code                                    --
--                                                        --
-- - use of the unaltered code to compile the interpreter --
--   for noncommercial educational and entertainment      --
--   purposes only                                        --
--                                                        --
-- - gratis redistribution of the code in entirety and in --
--   unaltered form for any aforementioned purpose        --
--                                                        --
-- The code may not be used for any other purposes,       --
-- including but not limited to:                          --
--                                                        --
-- - any commercial purpose                               --
--                                                        --
-- - use by any governmentally-affiliated organisation    --
--                                                        --
-- - connection to any external system for any useful     --
--   purpose whatsoever                                   --





module Parser where

import Text.ParserCombinators.HuttonMeijer
import Sisserou(Kind, Type, Exp, Pat, Kind(Star, KArr), Type(TVar, TAbs, TArr, TCons, TApp, TTup), Exp(Var, Abs, AbsT, App, AppT, Tup, Case, CVal, Fix), Pat(PVar, PVal, PTup), envEmpty, (|-), runTypingMonad, liftTypingMonad, (+>), (|==>), (|:-), Typing(TypeData, TypeError), Input(NewBind, NewType, NewCons, EvalExp), subst)

-- ===== Nicer Parser Combinators ===== --

-- Many of these are inspired by Peglet.

infixl 4 <*>, <=>, <?>, =*>, ==>, =?>, <*=, <==, <?=
infix 3 >>>

(>>>) :: Parser a -> (a -> b) -> Parser b
p >>> f = do x <- p
             return $ f x

(<*>), (<=>), (<?>) :: Parser a -> Parser b -> Parser (a, b)
p <*> q = do pR <- p
             qR <- q
             return (pR, qR)

p <=> q = p <*> spaces <*> q >>> (\ ((x, _), y) -> (x, y))

p <?> q = p <*> many (string " ") <*> q >>> (\ ((x, _), y) -> (x, y))

(=*>), (==>), (=?>) :: Parser a -> Parser b -> Parser b
(=*>) = (>>)

p ==> q = p =*> spaces =*> q

p =?> q = p =*> many (string " ") =*> q

(<*=), (<==), (<?=) :: Parser a -> Parser b -> Parser a
p <*= q = do x <- p
             q
             return x

p <== q = p <*= spaces <*= q

p <?= q = p <*= many (string " ") <*= q



-- ===== Utility Combinators ===== --

l = letter +++ digit +++ char '\''
i = (lower +++ digit) <*> many l >>> uncurry (:)
c = upper <*> many l >>> uncurry (:)

pParenthesised p = string "(" =?> p <?= string ")"

commaDelim = many (string " ") =*> string "," =*> spaces



-- ===== Type Parsing ===== --

pTyp   = pTArr +++ pTAbs +++ pTApp +++ pSTyp
pSTyp  = pTVar +++ pTCons +++ pParenthesised pTyp +++ pTTup
pTVar  = i >>> TVar
pTCons = c >>> TCons
pTAbs  = pParenthesised (i <== string ":" <=> pKind) <*> (pTAbs +++ pTArr +++ (spaces =*> pTyp)) >>> \ ((x, k), t) -> TAbs x k t
pTArr  = pParenthesised (string "- :" ==> pTyp) <*> (pTAbs +++ pTArr +++ (spaces =*> pTyp)) >>> uncurry TArr
pTApp  = pSTyp <*= string "[" <=> sepby1 pTyp commaDelim <== string "]" >>> uncurry (foldl TApp)
pTTup  = string "<" ==> pTyp <*= string "," <=> sepby1 pTyp commaDelim <== string ">" >>> TTup . uncurry (:)



-- ===== Expression Parsing ===== --

pExp        = pCase +++ pLet +++ pLetT +++ pFix +++ pAbsT +++ pAbs +++ pHexp
pHexp       = pApp +++ pSexp
pSexp       = pTuple +++ pParenthesised pExp +++ pVar +++ pConsNoArgs +++ pConsBoth +++ pConsTOnly +++ pConsVOnly
pVar        = i >>> Var
pCons       = c >>> (\ c -> CVal c [] [])
pLet        = string "let" ==> i <== string ":" <=> pTyp <== string "=" <=> pExp <== string "in" <=> pExp >>> \ (((x, t), e), e') -> App (Abs x t e') e
pLetT       = string "let" ==> i <== string ":" <=> pKind <== string "=" <=> pTyp <== string "in" <=> pExp >>> \ (((x, k), t), e) -> AppT (AbsT x k e) t
pCase       = string "case" ==> pHexp <== string "of" <== string "|" <=> sepby1 (pPat <== string "-" <=> pHexp) (spaces =*> string "|" =*> spaces) >>> uncurry Case
pAbsT       = pParenthesised (i <== string ":" <=> pKind) <*> (pAbs +++ pAbsT +++ (spaces =*> pExp)) >>> \ ((x, k), e) -> AbsT x k e
pAbs        = pParenthesised (i <== string ":" <=> pTyp) <*> (pAbs +++ pAbsT +++ (spaces =*> pExp)) >>> \ ((x, t), e) -> Abs x t e
argGroup1   = string "[" ==> sepby1 pExp commaDelim <== string "]" >>> map Right
argGroup2   = string "[" ==>  sepby1 pTyp commaDelim <== string "|]" >>> map Left
argGroup3   = string "[" ==> sepby1 pTyp commaDelim <== string "|" <=> sepby1 pExp commaDelim <== string "]" >>> \ (ts, es) -> map Left ts ++ map Right es
argGroup    = argGroup1 +++ argGroup2 +++ argGroup3
pApp        = pSexp <*> many1 argGroup >>> \ (e, ets) -> foldl autoapp e . concat $ ets
                where autoapp e (Left t)   = AppT e t
                      autoapp e (Right e') = App e e'
pTuple      = string "<" ==> pExp <*= string "," <=> sepby1 pExp commaDelim <== string ">" >>> Tup . uncurry (:)
pFix        = string "fix" ==> pParenthesised (i <== string ":" <=> pTyp) <*> (pAbs +++ pAbsT +++ (spaces =*> pExp)) >>> \ ((x, t), e) -> Fix x t e
pConsNoArgs = c <*= string "." >>> \ c -> CVal c [] []
pConsTOnly  = c <*= string "[" <=> sepby1 pTyp commaDelim <== string "|]" >>> \ (c, ts) -> CVal c ts []
pConsVOnly  = c <*= string "[" <=> sepby1 pExp commaDelim <== string "]" >>> \ (c, es) -> CVal c [] es
pConsBoth   = c <*= string "[" <=> sepby1 pTyp commaDelim <== string "|" <=> sepby1 pExp commaDelim <== string "]" >>> \ ((c, ts), es) -> CVal c ts es



-- ===== Pattern Parsing ===== --

pPat         = pPVar +++ pPConsNoArgs +++ pPConsBoth +++ pPConsTOnly +++ pPConsVOnly +++ pPTup
pPVar        = pParenthesised ((i +++ string "-") <== string ":" <=> pTyp) >>> uncurry PVar
pPConsNoArgs = c <*= string "." >>> \ c -> PVal c [] []
pPConsTOnly  = c <*= string "[" <=> sepby1 pTyp commaDelim <== string "|]" >>> \ (c, ts) -> PVal c ts []
pPConsVOnly  = c <*= string "[" <=> sepby1 pPat commaDelim <== string "]" >>> \ (c, ps) -> PVal c [] ps
pPConsBoth   = c <*= string "[" <=> sepby1 pTyp commaDelim <== string "|" <=> sepby1 pPat commaDelim <== string "]" >>> \ ((c, ts), ps) -> PVal c ts ps
pPTup        = string "<" ==> pPat <*= string "," <=> sepby1 pPat commaDelim <== string ">" >>> PTup . uncurry (:)



-- ===== Kind Parsing ===== --

pStar = string "*" >>> const Star
pKArr = pParenthesised (string "- :" ==> pKind) <*> (pKArr +++ (spaces =*> pKind)) >>> uncurry KArr
pKind = pStar +++ pKArr



-- ===== Input Parsing ===== --

pNewType = string "new" ==> string "type" ==> c <== string ":" <=> pKind >>> uncurry NewType
pNewCons = string "new" ==> string "cons" ==> c <== string ":" <=> pTyp >>> uncurry NewCons
pNewBind = i <== string ":" <=> pTyp <== string "=" <=> pExp >>> \ ((x, t), e) -> NewBind x t e
pEvalExp = pExp >>> EvalExp
pInput   = pNewType +++ pNewCons +++ pNewBind +++ pEvalExp
