{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Term where

import           Data.List
import           Text.Printf

infixr 5 :+:

data h :+: t = h :+: t deriving Show

class Term t v s | t -> v s where
  subterms :: t -> s
  var      :: t -> Maybe v
  binder   :: t -> Maybe v
  eq       :: t -> t -> Bool
  make     :: t -> s -> t

hom :: (Apply fs r, Lift r fs, MakeHom (t -> t) fs, Term t v r) => (t -> t) -> t -> t
hom f t =
  let s = subterms t in
  f $ make t $ apply s $ makeHom (hom f) (lift s)

class MakeHom0 f fs where
  makeHom0 :: f -> fs -> fs

instance MakeHom0 (f -> f) (a -> a) where
  makeHom0 f _ x = x

instance {-# OVERLAPPING #-} MakeHom0 (f -> f) (f -> f) where
  makeHom0 f _ x = f x

class MakeHom f fs where
  makeHom :: f -> fs -> fs

instance MakeHom0 f fs => MakeHom f fs where
  makeHom = makeHom0

instance {-# OVERLAPPING #-} (MakeHom0 f fs, MakeHom f gs) => MakeHom f (fs :+: gs) where
  makeHom f (fs :+: gs) = (makeHom0 f fs) :+: (makeHom f gs)

class Lift0 fs gs | fs -> gs where
  lift0 :: fs -> gs
  lift0 = undefined

instance Lift0 [f] (f -> f)

class Lift fs gs | fs -> gs where
  lift :: fs -> gs
  lift = undefined

instance Lift0 f f' => Lift f f'
instance {-# OVERLAPPING #-} (Lift0 f f', Lift g g') => Lift (f :+: g) (f' :+: g') where
  lift _ = undefined :+: undefined

class Apply fs t where
  apply :: t -> fs -> t

instance Apply (a -> a) [a] where
  apply x f = map f x

instance Apply g a => Apply (f :+: g) a where
  apply x (_ :+: g) = apply x g

instance {-# OVERLAPPING #-} Apply ((a -> a) :+: g) [a] where
  apply x (f :+: g) = map f x

instance {-# OVERLAPPING #-} (Apply (fs :+: gs) a, Apply (fs :+: gs) b) => Apply (fs :+: gs) (a :+: b) where
  apply (a :+: b) fsgs = (apply a fsgs) :+: (apply b fsgs)

class Eq v => FV t v | t -> v where
  fv :: t -> [v]

instance (FV s v, Term t v s, Eq v) => FV t v where
  fv t =
    let vars = fv $ subterms t in
    case var t of
      Just x  -> x : vars
      Nothing -> case binder t of
                   Just x  -> delete x vars
                   Nothing -> vars

instance {-# OVERLAPPING #-} FV t v => FV [t] v where
  fv t = concat $ map fv t

instance {-# OVERLAPPING #-} (FV h v, FV t v) => FV (h :+: t) v where
  fv (h :+: t) = fv h ++ fv t

type Renaming v = [(v, v)]

empty = Just []

class Eq v => Rename t v | t -> v where
  rename :: Maybe (Renaming v) -> t -> t -> Maybe (Renaming v)

instance (Rename s v, Term t v s, Eq v) => Rename t v where
  rename r t1 t2 =
    if eq t1 t2
    then rename r (subterms t1) (subterms t2) >>= (\ r ->
           let rename x y = case lookup x r of
                              Nothing -> Just $ if x == y then r else (x, y) : r
                              Just z  -> if z == y then Just r else Nothing
           in
           case (var t1, var t2) of
             (Just x, Just y) -> rename x y
             _                -> case (binder t1, binder t2) of
                                   (Just x, Just y) -> rename x y
                                   _                -> Just r
         )
    else Nothing

instance {-# OVERLAPPING #-} Rename t v => Rename [t] v where
  rename r t1 t2 = foldl (\ r (t1, t2) -> rename r t1 t2) r $ zip t1 t2

instance {-# OVERLAPPING #-} (Rename h v, Rename t v) => Rename (h :+: t) v where
  rename r (h1 :+: t1) (h2 :+: t2) = rename (rename r h1 h2) t1 t2

alpha :: (Rename t v, Term t v s) => t -> t -> Maybe (Renaming v)
alpha = rename empty

-------------------------------
-------------------------------


data Expr = Var String | Const Int | Bop String Expr Expr | D Def Expr deriving Show
data Def  = Def String Expr deriving Show

instance Term Def  String ([Expr] :+: [Def]) where
  var _            = Nothing
  binder (Def s _) = Just s
  eq  _ _          = True

  subterms (Def _ e) = [e] :+: []
  make     (Def s _) ([e] :+: []) = Def s e

instance Term Expr String ([Expr] :+: [Def]) where
  var (Var s) = Just s
  var  _      = Nothing

  binder _    = Nothing

  eq (Var     _) (Var     _) = True
  eq (Const   _) (Const   _) = True
  eq (Bop _ _ _) (Bop _ _ _) = True
  eq (D   _ _  ) (D   _ _  ) = True
  eq  _           _          = False

  subterms (Var _)     = [] :+: []
  subterms (Const _)   = [] :+: []
  subterms (Bop _ l r) = [l, r] :+: []
  subterms (D   d e)   = [e] :+: [d]

  make t@(Var _  ) _ = t
  make t@(Const _) _ = t
  make (Bop b _ _) ([l, r] :+: [] ) = Bop b l r
  make (D   _ _  ) ([e]    :+: [d]) = D d e

expr = Bop "+" (Var "a") (D (Def "b" (Const 1)) (Bop "+" (Const 0) (Var "b")))

{-
homhom f t =
  let t' = make t $ apply (subterms t) (homhom f :+: (id :: Def -> Def)) in
  f t'
-}

elim0 = hom (\ t -> case t of
                      Bop "+" e (Const 0) -> e
                      Bop "+" (Const 0) e -> e
                      _                   -> t
            )


{-
elim0 t =
  let t' = make t $ apply (subterms t) (elim0 :+: iddef) in
  case t' of
    Bop "+" e (Const 0) -> e
    Bop "+" (Const 0) e -> e
    _                   -> t'
-}

data T = V String | C T T

instance Term T String [T] where
  var (V x) = Just x
  var  _    = Nothing

  binder _   = Nothing

  eq (V _  ) (V _  ) = True
  eq (C _ _) (C _ _) = True
  eq  _         _        = False

  subterms (V _)     = []
  subterms (C t1 t2) = [t1, t2]

  make t@(V x) _ = t

f :: T -> T
f = id

data T1 v = T1A v | T1B (T1 v) (T1 v) | T1C (T2 v) (T2 v) deriving Show
data T2 v = T2A v | T2B (T2 v) (T2 v) | T2C (T1 v) (T1 v) deriving Show

id1 :: T1 String -> T1 String
id1 = id

id2 :: T2 String -> T2 String
id2 = id

f1 :: Apply fs ([T1 String] :+: [T2 String]) => T1 String -> fs -> T1 String
f1 (T1A v) f = T1A (printf "%s!" v)
f1 x       f = make x (apply (subterms x) f)

f2 :: T2 String -> (((T1 String -> T1 String) :+: (T1 String -> T2 String))) -> T2 String
f2 (T2A v) f = T2A (printf "%s!" v)
f2 x       f = x

instance Term (T1 v) v ([T1 v] :+: [T2 v]) where -- :+: Nil v) where
  var (T1A x) = Just x
  var  _      = Nothing

  binder _    = Nothing

  eq (T1A _  ) (T1A _  ) = True
  eq (T1B _ _) (T1B _ _) = True
  eq (T1C _ _) (T1C _ _) = True
  eq  _         _        = False

  subterms (T1A _)     = [] :+: []
  subterms (T1B t1 t2) = [t1, t2] :+: []
  subterms (T1C t1 t2) = [] :+: [t1, t2]

  make t@(T1A x) _                = t
  make (T1B _ _) ([t1, t2] :+: _) = T1B t1 t2
  make (T1C _ _) (_ :+: [t1, t2]) = T1C t1 t2

instance Term (T2 v) v ([T1 v] :+: [T2 v]) where -- :+: Nil v) where
  var (T2A x) = Just x
  var  _      = Nothing

  binder _    = Nothing

  eq (T2A _  ) (T2A _  ) = True
  eq (T2B _ _) (T2B _ _) = True
  eq (T2C _ _) (T2C _ _) = True
  eq  _         _        = False

  subterms (T2A _)     = [] :+: [] --
  subterms (T2B t1 t2) = [] :+: [t1, t2] -- :+: Nil
  subterms (T2C t1 t2) = [t1, t2] :+: [] -- :+: Nil

  make t@(T2A x) _ = t
  make (T2B _ _) (_ :+: [t1, t2]) = T2B t1 t2
  make (T2C _ _) ([t1, t2] :+: _) = T2C t1 t2

t   = T1C (T2A "1") (T2B (T2A "2") (T2C (T1A "0") (T1A "2")))
t1  = T1C (T2A "1") (T2B (T2A "2") (T2C (T1A "0") (T1A "1")))
t1' = T1C (T2A "10") (T2B (T2A "20") (T2C (T1A "40") (T1A "10")))
t2  = T2A "p"

_ = rename empty t t

_ = fv t
_ = fv [t]
_ = fv $ subterms t

foo :: ([T1 String] :+: [T2 String]) -> ([T1 String] :+: [T2 String])
foo = undefined


{-
class Term t v s | t -> v s where
  subterms :: t -> s
  var      :: t -> Maybe v

data Nil v s = Nil deriving Show
data h :+: t = [h] :+: t deriving Show

infixr 5 :+:

class TList t v s | t -> v s where
  fold :: (a -> v -> a) -> a -> t -> a

instance TList (Nil v s) v s where
  fold f a _ = a

instance (Term a v s, TList t v s) => TList (a :+: t) v s where
  fold f a (h :+: t) =
    let init = foldl (\ a h -> case var h of Nothing -> a ; Just v -> f a v) a h in
    --fold f init t
    init

data T1 v = T1A v | T1B (T1 v) (T1 v) | T1C (T2 v) (T2 v) deriving Show
data T2 v = T2A v | T2B (T2 v) (T2 v) | T2C (T1 v) (T1 v) deriving Show

newtype S v = S {out :: T1 v :+: T2 v :+: Nil v (S v)}

instance Term (T1 v) v (S v) where
  var (T1A x) = Just x
  var  _      = Nothing

  subterms (T1A _)     = S $ [] :+: [] :+: Nil
  subterms (T1B t1 t2) = S $ [t1, t2] :+: [] :+: Nil
  subterms (T1C t1 t2) = S $ [] :+: [t1, t2] :+: Nil

instance Term (T2 v) v (S v) where
  var (T2A x) = Just x
  var  _      = Nothing

  subterms (T2A _)     = S $ [] :+: [] :+: Nil
  subterms (T2B t1 t2) = S $ [] :+: [t1, t2] :+: Nil
  subterms (T2C t1 t2) = S $ [t1, t2] :+: [] :+: Nil


--freeVar t = fv' $ out (subterms t)

-}
{-
-}
{-
data ST = A Int | B String | C Char | D ST
-}

{-
instance Term ST () ([ST] :+: [Int] :+: [String] :+: [Char] :+: Nil) where
  subterms (A x) = []  :+: [x] :+: []  :+: []  :+: Nil
  subterms (B s) = []  :+: []  :+: [s] :+: []  :+: Nil
  subterms (C c) = []  :+: []  :+: []  :+: [c] :+: Nil
  subterms (D s) = [s] :+: []  :+: []  :+: []  :+: Nil
-}

{-
class HList Nil
class HList t =>
-}
{-
import Data.List
import Data.Maybe

class FreeTerm t v | t -> v where
  var      :: t -> Maybe v
  subterms :: t -> [t]
  eq       :: t -> t -> Bool
  make     :: t -> [t] -> t

hom :: FreeTerm t v => (t -> t) -> t -> t
hom f t = let t' = f t in make t' $ map (hom f) (subterms t')

fold :: FreeTerm t v => (a -> t -> a) -> a -> t -> a
fold f init t = f (foldl (fold f) init (subterms t)) t

fv :: (Eq v, FreeTerm t v) => t -> [v]
fv t = nub $ fold (\ vs t -> (maybeToList $ var t) ++ vs) [] t

type Renaming v = [(v, v)]

renaming :: (Eq v, FreeTerm t v) => t -> t -> Maybe (Renaming v)
renaming = undefined

data PTerm a b = V b | A a a

instance FreeTerm a b => FreeTerm (PTerm a b) b where
  var = undefined
  subterms = undefined
  eq = undefined
  make = undefined
-}

{-




class Term t where
  var     :: t -> Maybe String
  binds   :: t -> Maybe String
  sub     :: t -> [t]
  make    :: t -> [t] -> t
  makeVar :: String -> t
  rename  :: String -> t -> t
  eq      :: t -> t -> Bool

hom :: Term t => (t -> t) -> t -> t
hom f t = (\ t' -> make t' $ map (hom f) (sub t')) (f t)

fold :: Term t => (a -> t -> a) -> a -> t -> a
fold f init t = f (foldl (fold f) init (sub t)) t

fv :: Term t => t -> [String]
fv t = fold (\ vs t -> (vs \\ (maybeToList $ binds t)) `union` (maybeToList $ var t)) [] t

subst :: Term t => String -> t -> t -> t
subst x s t =
  case var t of
    Just y  -> if x == y then s else t
    Nothing -> case binds t of
                 Just y  -> if x == y
                            then t
                            else if elem y vs
                                 then let y' = head $ names \\ (vs ++ fv t) in
                                      subst x s $ rename y' $ make t (map (subst y $ makeVar y') $ sub t)
                                 else recurse
                 Nothing -> recurse
  where
    recurse = make t (map (subst x s) $ sub t)
    vs      = fv s
    names   = letters ++ [reverse $ a ++ n | n <- names, a <- letters] where
      letters = [[x] | x <- ['a'..'z']]

alpha :: Term t => t -> t -> Maybe [(String, String)]
alpha = alpha' (Just []) where
  alpha' f m n =
    if eq m n
    then case (binds m, binds n) of
           (Just mb, Just nb) -> recurse $ extend f mb nb
           _                  -> case (var m, var n) of
                                   (Just mv, Just nv) -> extend f mv nv
                                   _                  -> recurse f
    else Nothing
    where
      recurse f    = foldl (\ f (m', n') -> alpha' f m' n') f (zip (sub m) (sub n))
      extend f x y = f >>= (\ f -> case lookup x f of
                                     Nothing -> if notElem y (map snd f) then Just ((x, y) : f) else Nothing
                                     Just x' -> if x' == y then Just f else Nothing
                           )

data Lambda = Var String | App Lambda Lambda | Abs String Lambda deriving Show

instance Term Lambda where
  var   (Var s)   = Just s
  var    _        = Nothing

  binds (Abs x _) = Just x
  binds  _        = Nothing

  sub   (Abs _ l) = [l]
  sub   (App l m) = [l, m]
  sub    _        = []

  make (App _ _) [l, m] = App l m
  make (Abs x _) [l]    = Abs x l
  make v         _      = v

  makeVar = Var

  rename x (Var _)   = Var x
  rename x (Abs _ l) = Abs x l
  rename _  t        = t

  eq (Var _  ) (Var _  ) = True
  eq (Abs _ _) (Abs _ _) = True
  eq (App _ _) (App _ _) = True
  eq  _         _        = False

-}
