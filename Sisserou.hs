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





module Sisserou where

import Data.List(intercalate, intersect, intersperse, nub)
import Control.Monad(foldM, zipWithM)
import Control.Arrow(second)

-- ===== Expression / Type Representation ===== --

data Kind = Star
          | KArr Kind Kind
  deriving Eq
data Type = TVar String
          | TAbs String Kind Type
          | TArr Type Type
          | TCons String
          | TTup [ Type ]
          | TApp Type Type
data Exp  = Var String
          | Abs String Type Exp
          | AbsT String Kind Exp
          | App Exp Exp
          | AppT Exp Type
          | Case Exp [ (Pat, Exp) ]
          | CVal String [ Type ] [ Exp ]
          | Tup [ Exp ]
          | Fix String Type Exp
          | Close Exp (Env Exp)
data Pat  = PVar String Type
          | PVal String [ Type ] [ Pat ]
          | PTup [ Pat ]

data Typing a = TypeError [ String ]
              | TypeData a
  deriving Show

data Input = NewType String Kind
           | NewCons String Type
           | NewBind String Type Exp
           | EvalExp Exp

instance Eq Type where
  TVar x        == TVar y        = x == y
  TAbs x1 k1 t1 == TAbs x2 k2 t2 = k1 == k2 && rtes
                                     where x'   = "[" ++ x1 ++ "." ++ x2 ++ "]"
                                           rtes = let body = do t1' <- subst t1 x1 $ TVar x'
                                                                t2' <- subst t2 x2 $ TVar x'
                                                                return (t1', t2')
                                                  in
                                                  case runTypingMonad body 0 of
                                                    TypeData (t1', t2') -> t1' == t2'
                                                    _                   -> False
  TArr t1 t1'   == TArr t2 t2'   = t1 == t2 && t1' == t2'
  TCons c       == TCons c'      = c == c'
  TApp t1 t1'   == TApp t2 t2'   = t1 == t2 && t1' == t2'
  TTup ts       == TTup ts'      = all id $ zipWith (==) ts ts'
  _             == _             = False



-- ===== Monad for Typing ===== --

-- Here's a state + failure monad.

type St = Integer

newtype TypingMonad a = TypingMonad (St -> Typing (St, a))

instance Monad TypingMonad where
  return x = TypingMonad $ \ st -> TypeData (st, x)

  (TypingMonad f) >>= g = TypingMonad $ \ st ->
                                          case f st of
                                            TypeData (st', x) -> let TypingMonad h = g x in
                                                                  h st'
                                            TypeError ss      -> TypeError ss

  fail s                = TypingMonad $ \ _ -> fail s

getSt :: TypingMonad St
getSt = TypingMonad $ \ st -> TypeData (st, st)

setSt :: St -> TypingMonad ()
setSt st = TypingMonad $ \ _ -> TypeData (st, ())

genSym :: TypingMonad String
genSym = do i <- getSt
            setSt $ i + 1
            return $ "g." ++ show i

runTypingMonad :: TypingMonad a -> St -> Typing a
runTypingMonad (TypingMonad f) st = case f st of
                                      TypeData (_, x) -> TypeData x
                                      TypeError ss    -> TypeError ss

instance Monad Typing where
  return = TypeData

  (TypeError ss) >>= _ = TypeError ss
  (TypeData x)   >>= f = f x

  fail s = TypeError [ s ]

liftTypingMonad :: Typing a -> TypingMonad a
liftTypingMonad (TypeData x)   = TypingMonad $ \ st -> TypeData (st, x)
liftTypingMonad (TypeError ss) = TypingMonad $ \ _ -> TypeError ss



-- ===== Environments ===== --

newtype Env a = Env [ (String, a) ]

instance Show a => Show (Env a) where
  show (Env [])  = "[]"
  show (Env xs)  = "[ " ++ (concat . intersperse ", " . map showOne) xs ++ " ]"
                     where showOne (x, v) = x ++ " : " ++ show v


infix 3 ==>
(==>) :: Env a -> String -> Typing a
Env g ==> x = case lookup x g of
                Just v  -> return v
                Nothing -> fail $ "No binding for variable `" ++ x ++ "`."

infixl 3 +++
(+++) :: Env a -> Env a -> Env a
Env xs +++ Env ys = Env $ ys ++ xs

infixr 4 +>
(+>) :: (String, a) -> Env a -> Env a
p +> Env ps = Env $ p : ps

envEmpty :: Env a
envEmpty = Env []

dom :: Env a -> [ String ]
dom (Env ps) = map fst ps

infix 2 |-, |:-, |==>



-- ===== Expression Typing ===== --

returnCheckKind :: Env Kind -> Type -> TypingMonad Type
returnCheckKind d t = do k <- liftTypingMonad $ d |:- t
                         case k of
                           Star -> return t
                           _    -> fail $ "Kind error: Terms with kind `" ++ show k ++ "` are not allowed."

(|-) :: (Env Kind, Env Type) -> Exp -> TypingMonad Type
(d, g)    |- Var x         = (liftTypingMonad $ g ==> x) >>= returnCheckKind d
(d, g)    |- Abs x t e     = do t' <- (d, (x, t) +> g) |- e
                                returnCheckKind d $ TArr t t'
(d, g)    |- AbsT x k e    = do t <- ((x, k) +> d, g) |- e
                                returnCheckKind d $ TAbs x k t
dg@(d, _) |- App e e'      = do t  <- dg |- e >>= reduce
                                t' <- dg |- e'
                                case t of
                                  TArr t1 t2 -> do eq <- t1 =~= t'
                                                   if eq
                                                      then returnCheckKind d t2
                                                      else fail $ "Type error: Expected type `" ++ show t1 ++ "`; received type `" ++ show t' ++ "`."
                                  _          -> fail $ "Type error: Cannot apply nonarrow type `" ++ show t ++ "`."
dg@(d, g) |- AppT e t'     = do t <- dg |- e >>= reduce
                                case t of
                                  TAbs x k t'' -> do k' <- liftTypingMonad $ d |:- t'
                                                     if k == k'
                                                        then subst t'' x t' >>= returnCheckKind d
                                                        else fail $ "Kind error: Expected kind `" ++ show k ++ "`; received kind `" ++ show k' ++ "`."
                                  _            -> fail $ "Attempted to apply type `" ++ show t ++ "` but it is not a type abstraction."
dg@(d, g) |- Case e ls     = dg |- e >>= \ t ->
                             let inferCase (p, e) = (do g' <- (d, g, t) |==> p
                                                        (d, g +++ g') |- e)
                             in
                             mapM inferCase ls >>= \ ts ->
                             case ts of
                               []     -> fail "General error: Empty `case` not allowed."
                               t : ts -> do eqs <- mapM (t =~=) ts
                                            if all id eqs
                                               then returnCheckKind d t
                                               else fail "Type error: `case` branches have different types."
(d, g)    |- Fix x t e     = do t' <- (d, (x, t) +> g) |- e
                                eq <- t =~= t'
                                if eq
                                   then returnCheckKind d t
                                   else fail $ "Fixpoint expression declared to have type `" ++ show t ++ "` but inferred to have type `" ++ show t' ++ "`."
(d, g)    |- CVal c [] []  = (liftTypingMonad $ g ==> c) >>= returnCheckKind d
dg        |- CVal c ts es  = dg |- foldl App (foldl AppT (CVal c [] []) ts) es
dg        |- Tup es        = mapM (dg |-) es >>= return .TTup
_         |- e             = fail $ "Can only type syntactically-denotable terms; received `" ++ show e ++ "`."


-- ===== Pattern Typing ===== --

(|==>) :: (Env Kind, Env Type, Type) -> Pat -> TypingMonad (Env Type)
(d, _, t) |==> PVar x t'    = do eq <- t =~= t'
                                 liftTypingMonad $ d |:- t
                                 if eq
                                    then if "-" == x
                                            then return envEmpty
                                            else return $ (x, t') +> envEmpty
                                    else fail $ "Pattern failure: Pattern `" ++ x ++ "` was declared to have type `" ++ show t' ++ "` but required to have type `" ++ show t ++ "`."
(d, g, t) |==> p@(PTup ps)  = do t' <- reduce t
                                 case t of
                                   TTup ts -> do gs <- zipWithM infer ps ts
                                                 let xs = concatMap dom gs
                                                 if length xs /= length (nub xs)
                                                    then fail $ "Multiply-bound variable in pattern `" ++ show p ++ "`."
                                                    else return . foldl1 (+++) $ gs
                                   _       -> fail $ "Pattern failure: Tuple pattern cannot have type `" ++ show t ++ "`."
                                   where infer p t = (d, g, t) |==> p
(d, g, t) |==> PVal c ts ps = liftTypingMonad (g ==> c) >>= \ tc ->
                              let (tps, vps, t') = flattenTypeBody tc [] []
                                  nTps           = length tps
                                  nTs            = length ts
                                  nVps           = length vps
                                  nPs            = length ps
                              in
                              if nTps /= nTs
                                 then fail $ "Pattern failure: For constructor `" ++ c ++ "`: Expected " ++ show nTps ++ " type parameters; received " ++ show nTs ++ "."
                                 else if nVps /= nPs
                                         then fail $ "Pattern failure: For constructor `" ++ c ++ "`: Expected " ++ show nVps ++ " value parameters; received " ++ show nPs ++ "."
                                         else let kindsMatch            = liftTypingMonad $ mapM_ checkKind trips
                                                  checkKind ((_, k), t) = do k' <- d |:- t
                                                                             if k == k'
                                                                                then return ()
                                                                                else fail $ "Pattern kind error: For constuctor `" ++ c ++ "` expected a type argument of kind `" ++ show k ++ "`; received one of kind `" ++ show k' ++ "`."
                                                  substs t              = foldM subst' t trips
                                                  subst' t ((x, _), t') = subst t x t'
                                                  trips                 = zip tps ts
                                                  infer t p             = (d, g, t) |==> p
                                                  join g g'             = case dom g `intersect` dom g' of
                                                                            [] -> return $ g +++ g'
                                                                            is -> fail $ "Pattern failure: The following variables are multiply bound: `" ++ intercalate ", " is ++ "`."
                                              in
                                              do kindsMatch
                                                 t'' <- substs t'
                                                 eq  <- t =~= t''
                                                 if eq
                                                    then do ts' <- mapM substs vps
                                                            gs  <- zipWithM infer ts' ps
                                                            foldM join envEmpty gs
                                                    else fail $ "Pattern failure: Expected result type `" ++ show t ++ "` constructor `" ++ c ++ "` produces result type `" ++ show t'' ++ "`."
                                where flattenTypeBody (TAbs x k t) tps []  = flattenTypeBody t  (tps ++ [ (x, k) ]) []
                                      flattenTypeBody (TArr t t')  tps vps = flattenTypeBody t' tps                 (vps ++ [ t ])
                                      flattenTypeBody t            tps vps = (tps, vps, t)




-- ===== Kinding Relation ===== --

(|:-) :: Env Kind -> Type -> Typing Kind
g |:- TVar x     = g ==> x
g |:- TAbs x k t = (x, k) +> g |:- t
g |:- TArr t t'  = do g |:- t
                      g |:- t'
g |:- TCons c    = g ==> c
                   -- Assumes that constructors are in env and cannot be
                   -- aliased by variables; since constructors will be
                   -- capitalised and variables decapitalised, this env punning
                   -- will be safe.
g |:- TTup ts    = do ks <- mapM (g |:-) ts
                      if all (Star ==) ks
                         then return Star
                         else fail $ "Kind error: Higher-kinded term not allowed in tuple type."
g |:- TApp t t'  = do k  <- g |:- t
                      k' <- g |:- t'
                      case k of
                        KArr k1 k2 -> if k1 == k'
                                         then return k2
                                         else fail $ "Kind error: Expected kind `" ++ show k1 ++ "`; received kind `" ++ show k' ++ "`."
                        _          -> fail $ "Kind error: `*` cannot be applied."



-- ===== Utilities Operating on Types ===== --

-- <something> [ x := t' ]
class Subst t where
  subst :: t -> String -> Type -> TypingMonad t

instance Subst Type where
  subst (TVar x)     y t  | x == y = return t
  subst (TAbs x k t) y t' | x /= y = if x `elem` freeVars t'
                                        then do x'  <- genSym
                                                t'' <- subst t x (TVar x')
                                                subst (TAbs x' k t'') y t'
                                             -- Avoid capture.
                                        else do t'' <- subst t y t'
                                                return $ TAbs x k t''
  subst (TArr t1 t2) x t           = do t1' <- subst t1 x t
                                        t2' <- subst t2 x t
                                        return $ TArr t1' t2'
  subst (TTup ts) x t              = mapM subst' ts >>= return . TTup
                                       where subst' t' = subst t' x t
  subst (TApp t1 t2) x t           = do t1' <- subst t1 x t
                                        t2' <- subst t2 x t
                                        return $ TApp t1' t2'
  subst t            _ _           = return t

flippedSubst :: Subst t => String -> Type -> t -> TypingMonad t
flippedSubst x t v = subst v x t

instance Subst Pat where
  subst (PVar x t) y t'     = do t'' <- subst t y t'
                                 return $ PVar x t''
  subst (PVal c ts ps) y t' = do ts' <- mapM (flippedSubst y t') ts
                                 ps' <- mapM (flippedSubst y t') ps
                                 return $ PVal c ts' ps'
  subst (PTup ps) y t'      = mapM subst' ps >>= return . PTup
                                where subst' p = subst p y t'

instance Subst Exp where
  subst (Abs x t e) y t'           = do t'' <- subst t y t'
                                        e'  <- subst e y t'
                                        return $ Abs x t'' e'
  subst (AbsT x k e) y t' | x /= y = do e' <- subst e y t'
                                        return $ AbsT x k e'
  subst (App e e') y t'            = do e''  <- subst e y t'
                                        e''' <- subst e' y t'
                                        return $ App e'' e'''
  subst (AppT e t) y t'            = do e'  <- subst e y t'
                                        t'' <- subst t y t'
                                        return $ AppT e' t''
  subst (Tup es) y t'              = mapM subst' es >>= return . Tup
                                       where subst' e = subst e y t'
  subst (Case e ls) y t'           = do e'  <- subst e y t'
                                        ls' <- mapM substAlt ls
                                        return $ Case e' ls'
                                        where substAlt (p, e) = do p' <- subst p y t'
                                                                   e' <- subst e y t'
                                                                   return (p', e')
  subst (CVal x ts es) y t'        = do ts' <- mapM (flippedSubst y t') ts
                                        es' <- mapM (flippedSubst y t') es
                                        return $ CVal x ts' es'
  subst (Fix x t e) y t'           = do t'' <- subst t y t'
                                        e'  <- subst e y t'
                                        return $ Fix x t' e'
  subst (Close e g) y t'           = do e' <- subst e y t'
                                        g' <- subst g y t'
                                        return $ Close e' g'
  subst e _ _                      = return e

instance Subst a => Subst (Env a) where
  subst (Env g) y t' = do g' <- mapM substOne g
                          return $ Env g'
                          where substOne (x, v) = do v' <- subst v y t'
                                                     return (x, v')

freeVars :: Type -> [ String ]
freeVars = nub . freeVarsBody
             where freeVarsBody (TVar x)     = [ x ]
                   freeVarsBody (TAbs y _ t) = filter (/= y) . freeVarsBody $ t
                   freeVarsBody (TArr t t')  = freeVarsBody t ++ freeVarsBody t'
                   freeVarsBody (TCons _)    = []
                   freeVarsBody (TApp t t')  = freeVarsBody t ++ freeVarsBody t'
                   freeVarsBody (TTup ts)    = concatMap freeVarsBody ts
reduce :: Type -> TypingMonad Type
reduce (TApp t1 t2) = do t1' <- reduce t1
                         t2' <- reduce t2
                         case t1' of
                           TAbs x _ t -> subst t x t2'
                           _          -> return $ TApp t1' t2'
reduce (TAbs x k t) = reduce t >>= return . TAbs x k
reduce (TArr t1 t2) = do t1' <- reduce t1
                         t2' <- reduce t2
                         return $ TArr t1' t2'
reduce (TTup ts)    = mapM reduce ts >>= return . TTup
reduce t            = return t

infixl 4 =~=
(=~=) :: Type -> Type -> TypingMonad Bool
t1 =~= t2 = do t1' <- reduce t1
               t2' <- reduce t2
               return $ t1' == t2'

inNormalForm :: Type -> Bool
inNormalForm (TAbs _ _ t) = inNormalForm t
inNormalForm (TArr _ t')  = noTLQuantifiers t'
                              where noTLQuantifiers (TAbs _ _ _) = False
                                    noTLQuantifiers (TArr _ t)   = noTLQuantifiers t
                                    noTLQuantifiers _            = True
inNormalForm _            = True



-- ===== Print Stuff ===== --

showCommas :: Show a => [ a ] -> String
showCommas = intercalate ", " . map show

instance Show Kind where
  show Star           = "*"
  show (KArr k1 Star) = "(- : " ++ show k1 ++ ") *"
  show (KArr k1 k2)   = "(- : " ++ show k1 ++ ")" ++ show k2

instance Show Type where
  show (TVar x)                     = x
  show (TAbs x k t@(TAbs _ _ _))    = "(" ++ x ++ " : " ++ show k ++ ")" ++ show t
  show (TAbs x k t@(TArr _ _))      = "(" ++ x ++ " : " ++ show k ++ ")" ++ show t
  show (TAbs x k t)                 = "(" ++ x ++ " : " ++ show k ++ ") " ++ show t
  show (TArr t t'@(TArr _ _))       = "(- : " ++ show t ++ ")" ++ show t'
  show (TArr t t')                  = "(- : " ++ show t ++ ") " ++ show t'
  show (TCons c)                    = c
  show (TTup ts)                    = "< " ++ showCommas ts ++ " >"
  show (t@(TApp _ _)) | isConsApp t = c ++ "[ " ++ showCommas ts ++ " ]"
                                        where isConsApp (TApp (TCons _) _)      = True
                                              isConsApp (TApp t _)              = isConsApp t
                                              isConsApp _                       = False
                                              flattenConsApp (TApp (TCons c) t) = (c, [ t ])
                                              flattenConsApp (TApp t t')        = let (c, ts) = flattenConsApp t in
                                                                                  (c, ts ++ [ t' ])
                                              flattenConsApp _                  = ("FAIL", [])
                                              (c, ts)                           = flattenConsApp t
  show (TApp t t')                  = "(" ++ show t ++ ")@[ " ++ show t' ++ " ]"

instance Show Exp where
  show (Var x)                   = x
  show (Abs x t e@(Abs _ _ _))   = "(" ++ x ++ " : " ++ show t ++ ")" ++ show e
  show (Abs x t e@(AbsT _ _ _))  = "(" ++ x ++ " : " ++ show t ++ ")" ++ show e
  show (Abs x t e)               = "(" ++ x ++ " : " ++ show t ++ ") " ++ show e
  show (AbsT x k e@(Abs _ _ _))  = "(" ++ x ++ " : " ++ show k ++ ")" ++ show e
  show (AbsT x k e@(AbsT _ _ _)) = "(" ++ x ++ " : " ++ show k ++ ")" ++ show e
  show (AbsT x k e)              = "(" ++ x ++ " : " ++ show k ++ ") " ++ show e
  show (App e e')                = showS e ++ "[ " ++ showS e' ++ " ]"
  show (AppT e t)                = showS e ++ "[ " ++ show t ++ " |]"
  show (Tup es)                  = "< " ++ showCommas es ++ " >"
  show (Case e ls)               = "case " ++ showS e ++ " of " ++ (intercalate " | " . map (\ (p, e) -> show p ++ " -> " ++ showS e) $ ls)
  show (CVal c [] [])            = c ++ "."
  show (CVal c ts [])            = c ++ "[ " ++ showCommas ts ++ " |]"
  show (CVal c [] es)            = c ++ "[ " ++ showCommas es ++ " ]"
  show (CVal c ts es)            = c ++ "[ " ++ showCommas ts ++ " | " ++ showCommas es ++ " ]"
  show (Fix x t e@(Abs _ _ _))   = "fix (" ++ x ++ " : " ++ show t ++ ")" ++ show e
  show (Fix x t e@(AbsT _ _ _))  = "fix (" ++ x ++ " : " ++ show t ++ ")" ++ show e
  show (Fix x t e)               = "fix (" ++ x ++ " : " ++ show t ++ ") " ++ show e
  show (Close _ _)               = "<closure>"

showS :: Exp -> String
showS t@(Var _)      = show t
showS t@(CVal _ _ _) = show t
showS t              = "(" ++ show t ++ ")"

instance Show Pat where
  show (PVar x t)     = "(" ++ x ++ " : " ++ show t ++ ")"
  show (PVal c [] []) = c ++ "."
  show (PVal c [] ps) = c ++ "[ " ++ showCommas ps ++ " ]"
  show (PVal c ts []) = c ++ "[" ++ showCommas ts ++ "|]"
  show (PVal c ts ps) = c ++ "[ " ++ showCommas ts ++ " | " ++ showCommas ps ++ " ]"
  show (PTup ps)      = "< " ++ showCommas ps ++ " >"

instance Show Input where
  show (NewType x k)   = "new type " ++ x ++ " : " ++ show k
  show (NewCons x t)   = "new cons " ++ x ++ " : " ++ show t
  show (NewBind x t e) = x ++ " : " ++ show t ++ " = " ++ show e
  show (EvalExp e)     = show e



-- ===== Eager Type Substitution ===== --

partialEvaluateTypes :: Env Kind -> Exp -> TypingMonad Exp
partialEvaluateTypes d (Abs x t e)           = partialEvaluateTypes d e >>=  return . Abs x t
partialEvaluateTypes d (AbsT x k e)          = partialEvaluateTypes ((x, k) +> d) e >>= return . AbsT x k
partialEvaluateTypes d (App e1 e2)           = do e1' <- partialEvaluateTypes d e1
                                                  e2' <- partialEvaluateTypes d e2
                                                  return $ App e1' e2'
partialEvaluateTypes d (AppT e t)            = do e' <- partialEvaluateTypes d e
                                                  case e' of
                                                    AbsT x k e -> do k' <- liftTypingMonad $ d |:- t
                                                                     if k == k'
                                                                        then subst e x t
                                                                        else fail $ "Kind error during type substitution: For type `" ++ show t ++ "` expected kind `" ++ show k ++ "`; inferred kind `" ++ show k' ++ "`."
                                                    _          -> return $ AppT e' t
partialEvaluateTypes d (Case e ls)           = do e' <- partialEvaluateTypes d e
                                                  let (ps, es) = unzip ls
                                                  es' <- mapM (partialEvaluateTypes d) es
                                                  return $ Case e (zip ps es')
partialEvaluateTypes d (CVal c ts es)        = mapM (partialEvaluateTypes d) es >>= return . CVal c ts
partialEvaluateTypes d (Tup es)              = mapM (partialEvaluateTypes d) es >>= return . Tup
partialEvaluateTypes d (Fix x t e)           = partialEvaluateTypes d e >>= return . Fix x t
partialEvaluateTypes _ e                     = return e
