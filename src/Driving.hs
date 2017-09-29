{-# LANGUAGE TupleSections #-}

module Driving where

import Data.Foldable
import Data.List 
import Data.Maybe
import Syntax
import Stream
import qualified Eval as E

data Tree  = 
  Fail                         | 
  Success E.Sigma              | 
  Or      Tree Tree            | 
  Fresh   X S Tree             | 
  Rename  String [Ts] Renaming |
  Gen     Embedding Tree

type Stack = [(String, [Ts])]

-- Renaming
type Renaming = [(S, S)]
 
rename :: G S -> G S -> Maybe Renaming
rename g1 g2 = rename' (Just []) (g1, g2) where
  rename' r (s1 :=:  s2, t1 :=:  t2)                                      = renameTerms r [s1, s2] [t1, t2]
  rename' r (g1 :/\: g2, h1 :/\: h2)                                      = renameGoals r [g1, g2] [h1, h2]
  rename' r (g1 :\/: g2, h1 :\/: h2)                                      = renameGoals r [g1, g2] [h1, h2]
  rename' r (Invoke f as, Invoke g bs) | f == g && length as == length bs = renameTerms r as bs
  rename' _  _                                                            = Nothing
  renameTerm r (C m ms, C n ns) | m == n && length ms == length ns = renameTerms r ms ns
  renameTerm r (V x, V y) = r >>= (\r -> case lookup x r of
                                           Nothing -> Just $ (x, y) : r
                                           Just z  -> if z == y then Just r else Nothing
                                  )
  renameTerm  _ _       = Nothing
  renameTerms           = renames renameTerm
  renameGoals           = renames rename'
  renames     f r ms ns = foldl f r $ zip ms ns

-- Embedding
type Embedding = [(S, Ts)]

embed :: G S -> G S -> Maybe Renaming
embed g1 g2 = embedGoal True (Just []) (g1, g2) where 
  embedGoal _ r (t1 :=:   t2, q1 :=: q2  ) = embedTerms r [t1, t2] [q1, q2]
  embedGoal _ r (g1 :/\:  g2, h1 :/\: h2 ) = foldl (embedGoal False) r $ [(g1, h1), (g2, h2)]
  embedGoal _ r (g1 :\/:  g2, h1 :\/: h2 ) = foldl (embedGoal False) r $ [(g1, h1), (g2, h2)]
  embedGoal _ r (Invoke f fs, Invoke g gs) | f == g && length fs == length gs = embedTerms r fs gs
  embedGoal False r (g, g1 :/\: g2) = msum $ map (embedGoal False r . (g,)) [g1, g2]
  embedGoal False r (g, g1 :\/: g2) = msum $ map (embedGoal False r . (g,)) [g1, g2]
  embedGoal _ _  _ = Nothing 
  embedTerm r (V x, V y) | x == y = r
  embedTerm r (V x, V y) = r >>= (\ r -> case lookup x r of 
                                           Just z  -> if z == y then Just r else Nothing
                                           Nothing -> Just $ (x, y) : r
                                 )
  embedTerm r (C m ms, C n ns) | m == n && length ms == length ns = foldl embedTerm r $ zip ms ns
  embedTerm r (t     , C n ns)                = msum $ map (embedTerm r . (t,)) ns
  embedTerm _  _                              = Nothing
  embedTerms r ps qs | length ps == length qs = foldl embedTerm r $ zip ps qs
  embedTerms _ _  _                           = Nothing

generalize :: G S -> G S -> Embedding
generalize 

eval :: Stack -> E.Gamma -> E.Sigma -> G X -> Tree
eval cs e s g@(t1 :=: t2) = 
  case takeS 1 $ E.eval e s g of
    []       -> Fail
    [(s, _)] -> Success s
eval cs  e        s (g1 :\/: g2      ) = Or (eval cs e s g1) (eval cs e s g2)
eval cs (p, i, d) s (Syntax.Fresh x g) = Driving.Fresh x y $ eval cs (p, E.extend i x (V y), d') s g where 
  y : d' = d
eval cs (p, i, d) s (Invoke f as) = 
  let (_, fs, g) = p f in
  let i'         = foldl (\ i' (f, a) -> E.extend i' f $ i E.<@> a) i $ zip fs as in
  let as' = map (E.substitute s . (i E.<@>)) as in
  case find (\ (g, bs) -> isJust $ rename (Invoke g bs) (Invoke f as')) cs of 
    Just (g, bs) -> Rename g bs $ fromJust (rename (Invoke g bs) (Invoke f as'))
    Nothing      -> case find (\ (g, bs) -> isJust $ embed (Invoke g bs) (Invoke f as')) cs of
                      Just (g, bs) -> undefined
                      Nothing      -> eval ((f , as') : cs) (p, i', d) s g  

drive :: Spec -> Tree
drive (defs, goal) = undefined
