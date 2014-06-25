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





module Eval where

import Sisserou(Kind, Type, Exp, Pat, Env, TypingMonad, liftTypingMonad, Kind(Star, KArr), Type(TVar, TAbs, TArr, TCons, TApp), Exp(Var, Abs, AbsT, App, AppT, Case, CVal, Fix, Close, Tup), Pat(PVar, PVal, PTup), envEmpty, (+>), (+++), (==>), subst)
import Control.Monad(zipWithM, sequence)

-- ===== Evaluator ===== --

eval :: Env Exp -> Exp -> TypingMonad Exp
eval g (Var x)                 = liftTypingMonad (g ==> x) >>= eval g
eval g e@(Abs _ _ _)           = return $ Close e g
eval g e@(AbsT _ _ _)          = return $ Close e g
eval g (App e e')              = do v' <- eval g e'
                                    v  <- eval g e
                                    case v of
                                      Close (Abs x _ e) g' -> eval ((x, v') +> g') e
                                      CVal c ts vs         -> return $ CVal c ts (vs ++ [ v' ])
                                      _                    -> fail $ "Expected a closure in function position of application; received `" ++ show v ++ "`."
eval g (AppT e t)              = do v <- eval g e
                                    case v of
                                      Close (AbsT x _ e) g' -> do e'  <- subst e x t
                                                                  g'' <- subst g' x t
                                                                  eval g'' e'
                                      CVal c ts []          -> return $ CVal c (ts ++ [ t ]) []
                                      _                     -> fail $ "Expected a closure in left argument of type application; received `" ++ show v ++ "`."
eval g (Tup es)                = mapM (eval g) es >>= return . Tup
eval g (Case e [])             = do v <- eval g e
                                    fail $ "Match failure for `" ++ show v ++ "`."
eval g (Case e ((p, e') : ls)) = do v <- eval g e
                                    case tryMatchPat p v of
                                      Just g' -> eval (g +++ g') e'
                                                 -- Here, it's absolutely essential that we have `g +++ g'`
                                                 -- (rather than `g' +++ g`): Else, pattern variables meant
                                                 -- to shadow previous bindings would end up 'in the wrong
                                                 -- place'. (See definition of `(+++)`.)
                                      Nothing -> eval g $ Case v ls
eval g (CVal c ts es)          = do vs <- mapM (eval g) es
                                    return $ CVal c ts vs
eval g e@(Fix x _ e')          = eval ((x, e) +> g) e'
eval _ e@(Close _ _)           = return e



-- ===== Pattern Matching ===== --

tryMatchPat :: Pat -> Exp -> Maybe (Env Exp)
tryMatchPat (PVar x _) v                           = return $ (x, v) +> envEmpty
tryMatchPat (PVal c _ ps) (CVal c' _ vs) | c == c' = zipWithM tryMatchPat ps vs >>= return . foldl (+++) envEmpty
tryMatchPat (PTup ps)     (Tup vs)                 = zipWithM tryMatchPat ps vs >>= return . foldl (+++) envEmpty
tryMatchPat _             _                        = fail "Match failure."
