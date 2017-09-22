{-# LANGUAGE TupleSections #-}

import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe 

type X    = String -- Syntactic variables
type S    = Int    -- Semantic variables
type Name = String -- Names of variables/definitions

-- Terms
data Term v = V v | C String [Term v] deriving (Eq, Show)
type Tx     = Term X
type Ts     = Term S

-- Goals
data G = 
    Tx :=: Tx
  | G :/\: G
  | G :\/: G
  | Fresh  Name G 
  | Invoke Name [Tx] deriving Show

infixr 7 &&&
infixr 6 |||
infix  8 ===

(===) = (:=:)
(|||) = (:\/:)
(&&&) = (:/\:)

fresh xs g = foldr Fresh g xs
call       = Invoke

-- Definitions
type Def = (Name, [Name], G)

def = (,,)

-- Specification
type Spec = ([Def], G)

spec = (,)

-- States
type Iota  = X -> Ts
type Sigma = [(S, Ts)]
type Delta = [S]
type P     = String -> Def
type Gamma = (P, Iota, Delta)

-- Stream
data Stream a = Empty
              | Mature a (Stream a)
              -- we need this in case of left recursion (who would have known)
              | Immature (Stream a)
              deriving Show

takeS 0 _            = []
takeS n Empty        = []
takeS n (Mature a s) = a : takeS (n-1) s
takeS n (Immature s) = takeS n s

maybeToStream Nothing  = Empty
maybeToStream (Just a) = return a

instance Functor Stream where
  fmap _ Empty        = Empty
  fmap f (Mature a s) = Mature (f a) (fmap f s)
  fmap f (Immature s) = Immature (fmap f s)

instance Applicative Stream where
  pure a = Mature a Empty
  Empty        <*> _            = Empty
  (Mature f s) <*> Empty        = Empty
  (Immature s) <*> x            = s <*> x
  (Mature f s) <*> (Mature x t) = Mature (f x) (s <*> t)
  s            <*> (Immature t) = s <*> t

instance Alternative Stream where
  empty = Empty
  Empty <|> s = s
  s <|> Empty = s
  Immature s <|> t = s <|> t
  s <|> Immature t = s <|> t
  Mature a s <|> t = Mature a (s <|> t)

instance Monad Stream where
  Empty >>= _ = mzero
  Mature x xs >>= g = g x `mplus` (xs >>= g)
  Immature x  >>= y = Immature $ x >>= y

instance MonadPlus Stream where
  mzero = Empty
  mplus (Mature h tl) y = Mature h $ y `mplus` tl
  mplus (Immature  x) y = Immature $ y `mplus` x
  mplus Empty         y = y

-- Unification
unify :: Maybe Sigma -> Ts -> Ts -> Maybe Sigma 
unify Nothing _ _ = Nothing
unify st@(Just subst) u v = 
  unify' (walk u subst) (walk v subst)  where
    unify' (V u) (V v) | u == v = Just subst
    unify' (V u) _              = Just $ (u, v) : subst
    unify' _ (V v)              = Just $ (v, u) : subst
    unify' (C a as) (C b bs) | a == b && length as == length bs = 
      foldl (\ st (u, v) -> unify st u v) st $ zip as bs
    unify' _ _ = Nothing
    walk x@(V v) s =
      case lookup v s of
        Nothing -> x
        Just t  -> walk t s
    walk u     _ = u

-- Syntactic terms -> semantic terms

---- Interpreting syntactic variables 
infix 7 <@>
(<@>) :: Iota -> Tx -> Ts
i <@> (V x)    = i x
i <@> (C c ts) = C c $ map (i<@>) ts

---- Extending variable interpretation
extend :: Iota -> X -> Ts -> Iota
extend i x ts y = if x == y then ts else i y 

-- Evaluation relation
eval :: Gamma -> Sigma -> G -> Stream (Sigma, Delta)
eval (p, i, d) s (t1 :=:  t2) = fmap (,d) (maybeToStream $ unify (Just s) (i <@> t1) (i <@> t2))
eval  env      s (g1 :\/: g2) = eval env s g1 `mplus` eval env s g2
eval env@(p, i, d) s (g1 :/\: g2) = 
  eval env s g1 >>= (\ (s', d') -> eval (p, i, d') s' g2)
eval (p, i, d) s (Fresh x g)  = eval (p, extend i x (V y), d') s g where
  y : d' = d 
eval env@(p, i, d) s (Invoke f as) = 
  let (_, fs, g) = p f in
  let i'         = foldl (\ i' (f, a) -> extend i' f $ i <@> a) i $ zip fs as in
  eval (p, i', d) s g

env0 :: P -> Gamma
env0 p = (p, (\_ -> undefined), [0..]) 

s0 :: Sigma
s0 = []

run :: Spec -> Stream Sigma
run (defs, goal) =
  let p n = fromJust $ find (\ (m, _, _) -> m == n) defs in
  fmap fst $ eval (env0 p) s0 goal

-- Tests
nil      = C "Nil"  []     
cons x y = C "Cons" [x, y] 
i    x   = C x      []

appendo =
  let x  = V "x"  in
  let y  = V "y"  in
  let xy = V "xy" in
  let h  = V "h"  in
  let t  = V "t"  in
  let ty = V "ty" in
  def "appendo" ["x", "y", "xy"] 
         ((x === nil &&& xy === y) ||| 
          (fresh ["h", "t", "ty"] 
             (x  === h `cons` t  &&&
              xy === h `cons` ty &&&
              call "appendo" [t, y, ty]
             )
          )
         )

reverso =
  let x  = V "x"  in
  let y  = V "y"  in
  let h  = V "h"  in
  let t  = V "t"  in
  let rt = V "rt" in
  def "reverso" ["x", "y"]
         ((x === nil &&& y === nil) |||
          (fresh ["h", "t", "rt"]
             (x === h `cons` t &&&
              call "appendo" [rt, h `cons` nil, y] &&&
              call "reverso" [t, rt] 
             )
          )
         )

toplevel n spec = takeS n $ (run spec)

main = 
  do
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [i "A" `cons` nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [i "A" `cons` nil, i "B" `cons` nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [ V "q", i "B" `cons` nil, i "A" `cons` (i "B" `cons` nil)])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [i "A" `cons` nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [i "A" `cons` (i "B" `cons` nil), V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [V "q", i "A" `cons` (i "B" `cons` nil)])))
    
  
