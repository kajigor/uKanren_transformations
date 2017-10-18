{-# LANGUAGE TupleSections #-}

module Driving where

import Data.Function
import Data.Foldable
import Data.List 
import Data.Maybe
import Syntax
import Stream
import qualified Eval as E
import Test
import PrintTree

type Stack = [(String, [Ts])]

 
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

generalize :: [S] -> G S -> G S -> (G S, Generalizer, Generalizer, [S])
generalize d (Invoke f as) (Invoke g bs) = 
  let (msg, d')        = generalizeTerm d ([], []) (C "()" as, C "()" bs) in
  let (C _ cs, s1, s2) = refine msg in
  (Invoke f cs, s1, s2, d') where
  generalizeTerm vs s@(s1, s2) (C m ms, C n ns) | m == n && length ms == length ns =
    let (gs, (s1, s2), vs') = foldl (\ (gs, s, vs) ts -> 
                                          let ((g, s1, s2), vs') = generalizeTerm vs s ts in
                                          (g:gs, (s1, s2), vs')
                                    ) ([], s, vs) $ zip ms ns
    in  
    ((C m $ reverse gs, s1, s2), vs')
  generalizeTerm vs (s1, s2) (V x, V y) | x == y = ((V x, s1, s2), vs)  
  generalizeTerm (v:vs) (s1, s2) (t1, t2) = ((V v, (v, t1):s1, (v, t2):s2), vs)
  refine msg@(g, s1, s2) = 
    case filter ((>1) . length) $ groupBy group s1 of
      [] -> msg 
      gs -> foldl (\ acc g1 ->
                      let restriction = filter (\ (x, _) -> isJust $ lookup x g1) s2 in
                      case filter ((>1) . length) $ groupBy group restriction of
                        [] -> msg
                        g2 -> foldl (\ (g, s1, s2) ((x, _):bs) ->
                                       (
                                        E.substitute (map (\ (y, _) -> (y, V x)) bs) g, 
                                        filter (\ (x, _) -> isNothing $ lookup x bs) s1, 
                                        filter (\ (x, _) -> isNothing $ lookup x bs) s2
                                       ) 
                                    ) 
                                    msg 
                                    g2
                  ) 
                  msg
                  gs  
    where group x y = snd x == snd y

invoke :: Stack -> E.Gamma -> E.Sigma -> G S -> Tree 
invoke cs (p, i, d) s (Invoke f as') = 
  let (_, fs, g) = p f in
  case find (\ (g, bs) -> isJust $ rename (Invoke g bs) (Invoke f as')) cs of 
    Just (g, bs) -> Rename g bs $ fromJust (rename (Invoke g bs) (Invoke f as'))
    Nothing      -> case find (\ (g, bs) -> isJust $ embed (Invoke g bs) (Invoke f as')) cs of
                      Just (g, bs) -> let (msg, s1, s2, d') = generalize d (Invoke f as') (Invoke g bs) in
                                      (Gen s1 (invoke ((f, as'):cs) (p, i, d') s msg))
                      Nothing      -> eval ((f, as') : cs) (p, i, d) s g  

eval :: Stack -> E.Gamma -> E.Sigma -> G X -> Tree
eval cs e s g@(t1 :=: t2) = 
  case takeS 1 $ E.eval e s g of
    []       -> Fail
    [(s, _)] -> Success s
eval cs  e        s (g1 :\/: g2      ) = Or (eval cs e s g1) (eval cs e s g2)
eval cs (p, i, d) s (Syntax.Fresh x g) = PrintTree.Fresh x y $ eval cs (p, E.extend i x (V y), d') s g where 
  y : d' = d
eval cs (p, i, d) s (Invoke f as) = 
  let (_, fs, g) = p f in
  let i'         = foldl (\ i' (f, a) -> E.extend i' f $ i E.<@> a) i $ zip fs as in
  let as' = map (E.substitute s . (i E.<@>)) as in
  invoke cs (p, i', d) s (Invoke f as')

drive :: Spec -> Tree
drive (defs, goal) =
  let p n = fromJust $ find (\ (m, _, _) -> m == n) defs in
  eval [] (E.env0 p) E.s0 goal

f x y = C "f" [x, y]
g x y = C "g" [x, y]
t0 s = C s []
[a, b, c] = map t0 ["a", "b", "c"]
x = V 0

tree = drive ([Test.appendo'], fresh ["q"] (call "appendo'" [Test.nil, Test.nil, V "q"]))

main = printTree tree "tree.dot"
