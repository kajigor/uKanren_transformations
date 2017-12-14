{-# LANGUAGE TupleSections #-}

module Driving where

import Data.Function
import Data.Foldable
import Data.List 
import Data.Maybe
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map
import Syntax
import Stream
import qualified Eval as E
import Tree 
import List
import Debug.Trace

type TreeContext = (Set.Set Id, Map.Map Id [S], [Id])

emptyContext :: TreeContext
emptyContext = (Set.empty, Map.empty, [0..])

type Stack = [(Id, [G S])]

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

conj (a:as) = foldl (:/\:) a as 

renameGoals :: [G S] -> [G S] -> Maybe Renaming
renameGoals as bs = rename (conj as) (conj bs)

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

embedGoals :: [G S] -> [G S] -> Maybe Renaming
embedGoals as bs = embed (conj as) (conj bs)

generalize :: [S] -> (Generalizer, Generalizer) -> G S -> G S -> (G S, Generalizer, Generalizer, [S])
generalize d gg (g1 :/\: g2) (h1 :/\: h2) =
  let (i, s1' , s2' , d' ) = generalize d  gg         g1 h1 in
  let (j, s1'', s2'', d'') = generalize d' (s1', s2') g2 h2 in
  (i :/\: j, s1'', s2'', d'')
generalize d gg (g1 :\/: g2) (h1 :\/: h2) =
  let (i, s1' , s2' , d' ) = generalize d  gg         g1 h1 in
  let (j, s1'', s2'', d'') = generalize d' (s1', s2') g2 h2 in
  (i :\/: j, s1'', s2'', d'')
generalize d gg (t1 :=: t2) (r1 :=: r2) =
  let ((i, s1' , s2' ), d' ) = generalizeTerm d  gg         (t1, r1) in
  let ((j, s1'', s2''), d'') = generalizeTerm d' (s1', s2') (t2, r2) in
  (i :=: j, s1'', s2'', d'')
generalize d gg (Invoke f as) (Invoke g bs) = 
  let (msg, d')        = generalizeTerm d gg (C "()" as, C "()" bs) in
  let (C _ cs, s1, s2) = refine msg in
  (Invoke f cs, s1, s2, d') 

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

generalizeGoals :: [S] -> [G S] -> [G S] -> (G S, Generalizer, Generalizer, [S])
generalizeGoals s as bs = generalize s ([], []) (conj as) (conj bs)

substitute :: E.Sigma -> G S -> G S
substitute s (t1 :=: t2  ) = E.substitute s t1 :=: E.substitute s t2
substitute s (g1 :/\: g2 ) = substitute s g1 :/\: substitute s g2 
substitute s (g1 :\/: g2 ) = substitute s g1 :\/: substitute s g2
substitute s (Invoke f as) = Invoke f $ map (E.substitute s) as

weakCouple :: (G S, G S) -> Bool
weakCouple gs =
  case gs of
    (_ :=:  _  , _ :=:  _  ) -> True
    (_ :/\: _  , _ :/\: _  ) -> True
    (_ :\/: _  , _ :\/: _  ) -> True
    (Invoke _ _, Invoke _ _) -> True
    _                        -> False

fst' (x, _, _) = x
snd' (_, x, _) = x 
trd' (_, _, x) = x 

split :: [Zeta] -> [G S] -> ([Zeta], [Zeta])
split gs1 gs2 = 
  case elemIndex False $ map weakCouple $ zip (map trd' gs1) gs2 of
    Nothing -> splitAt (length gs2) gs1
    Just i  -> splitAt (i-1)        gs1

update :: (E.P, E.Delta) -> Def -> (E.P, E.Delta)
update (p, d) def = let (p', _, d') = E.update (p, (\ _ -> V 3), d) def in (p', d')

invoke :: TreeContext -> Stack -> E.Delta -> E.Sigma -> [Zeta] -> (TreeContext, Tree) 
invoke tc@(sr, args, (ids@(_1:_))) cs d s conjs = 
  -- HERE WE HAVE TO SUBSTITUTE INTO THE CURRENT GOAL
-- if _1 > 5 then (tc, Fail) 
-- else 
  let qqq = map (\(a, b, g) -> (a, b, substitute s g)) conjs in 
  let qqq_conjs = map trd' qqq in
  let p = snd' $ head conjs in 
  let g = conj qqq_conjs in 
  case find (\ (_, conjs') -> isJust $ renameGoals conjs' qqq_conjs) cs of 
    Just (di, conjs') ->
      let id:ids' = ids in
      let r       = fromJust (renameGoals conjs' qqq_conjs) in
      (
        (Set.insert di sr, Map.insert di (map fst r) args, ids'),
        Split id (Success s) (Rename di g r) g
      )
    Nothing -> 
      case find (\ (_, conjs') -> (isJust $ embedGoals conjs' qqq_conjs)) cs of
        Just (_, conjs') -> 
          if length qqq == length conjs'
          then
               let (msg, s1, s2, d') = generalizeGoals d qqq_conjs conjs' in
               let id:ids' = ids in
               let (tc', node) = eval (sr, args, ids') ((id, qqq_conjs):cs) d' (s1 `E.o` s) [] (i, p, msg) [] in
               (tc', Gen id s1 node g)
          else if length conjs' < length qqq 
               then let id:ids'        = ids in
                    let cs'            = (id, qqq_conjs):cs in
                    let (lh:lt, rh:rt) = split qqq conjs' in
                    let (tc' , node' ) = eval (sr, args, ids') cs' d s [] lh lt in
                    let (tc'', node'') = eval tc'              cs' d s [] rh rt in
                    (tc'', Split id node' node'' g)
               else error "Wow..."
        Nothing -> unfold tc cs d s qqq

{-
invoke :: TreeContext -> Stack -> E.Delta -> E.Sigma -> [Zeta] -> (TreeContext, Tree) 
invoke (sr, args, ids) cs d s askhg = -- (goal@(i, p, Invoke f as') : conjs) =
  -- HERE WE HAD TO SUBSTITUTE INTO CURRENT GOAL
  let qqq@(goal@(i,p,Invoke f as') : conjs) = map (\(a, b, Invoke f as) -> (a,b,Invoke f $ map (E.substitute s) as)) askhg in 
  trace ("\nInvoke: " ++ show (head ids) ++ ", " ++ show (map trd' $ goal : conjs) ++ "\n" ++ "\nQQQQ\n" ++ show (map trd' qqq)) $
  let (_, fs, g) = p f in
  case find (\ (_, conjs') -> isJust $ renameGoals conjs' (Invoke f as' : map trd' conjs)) cs of 
    Just (di, (Invoke g bs):conjs') ->
      trace ("\nFOUND renaming\n ") $
      let id:ids' = ids in
      let r       = fromJust (renameGoals (Invoke g bs : conjs') (Invoke f as' : map trd' conjs)) in
      ((Set.insert di sr, Map.insert di (map fst r) args, ids'),
       Split
         id
         (Success s) 
         (Rename di (conj (Invoke f as' : map trd' conjs)) r)
         (conj (Invoke f as' : map trd' conjs))
      )
    Nothing -> 
      trace ("\nTrying embedding...\n\nIn stack:\n" ++ show cs ++ "\nGoal:\n" ++ show (Invoke f as' : map trd' conjs) ) $
      case find (\ (_, conjs') -> (isJust $ embedGoals conjs' (Invoke f as' : map trd' conjs))) cs of
        Just (_, (Invoke g bs):conjs') -> 
          trace ("Found embedding\n") $
          if length conjs == length conjs' 
          then let x = (Invoke f as' : map trd' conjs)
                   y = (Invoke g bs  : conjs')
               in
               let (msg, s1, s2, d') = generalizeGoals d (Invoke f as' : map trd' conjs) (Invoke g bs : conjs') in
               trace ("HERE\n" ++ "x: " ++ show x ++ "\ny: " ++ show y ++ "\ng: " ++ show msg  ) $
               let id:ids' = ids in
               let (tc', node) = eval (sr, args, ids') ((id, (Invoke f as'):(map trd' conjs)):cs) d' (s1 `E.o` s) [] (i, p, msg) [] in
               (tc', Gen id s1 node (conj $ map trd' $ goal:conjs))
          else if length conjs' < length conjs 
               then trace ("In Split") $
                    let id:ids'       = ids in                    
                    let cs'           = (id, (Invoke f as'):(map trd' conjs)):cs in
                    let (left, right) = split ((i, p, Invoke f as') : conjs) (Invoke g bs : conjs') in
                    trace ("SPLITTED into \n" ++ show (map trd' left) ++ "\nAND\n" ++ show (map trd' right)) $
                    let lh:lt = left  in
                    let rh:rt = right in
                    let (tc' , node' ) = eval (sr, args, ids') cs' d s [] lh lt in
                    let (tc'', node'') = eval tc'              cs' d s [] rh rt in
                    (tc'', Split id node' node'' (conj $ map trd' $ goal:conjs))
               else error "Wow..."
        Nothing -> --if head ids > 7 
                   --then trace ("Failed\n") $ ((sr, args, ids), Fail)
                   --else
                     trace "Unfold branch\n" $ unfold (sr, args, ids) cs d s (goal:conjs)
-}
{-
 let (g', (p', i', d')) = trace (show g) (E.pre_eval (p, i, d) g) in
                   trace (show g') $
                   let id:ids' = ids in
                   let (tc', node) = eval (sr, args, ids') ((id, f, as', map trd' conjs) : cs) d' s [] (i', p', g') conjs in
                   (tc', Call id node (conj $ map trd' $ goal:conjs))
-}

type Zeta = (E.Iota, E.P, G S)

eval :: TreeContext -> Stack -> E.Delta -> E.Sigma -> [Zeta] -> Zeta -> [Zeta]  -> (TreeContext, Tree)
eval tc cs d s prev (i, p, Let def g) conjs = 
  let (p', d') = update (p, d) def in
  eval tc cs d' s prev (i, p', g) conjs  
eval tc cs d s prev g@(i, p, t1 :=: t2) conjs =
  case takeS 1 $ E.eval (p, i, d) s (trd' g) of
    []       -> (tc, Fail)
    [(s, _)] -> case conjs of
                  []       -> case prev of
                                [] -> unfold tc cs d s []  
                                _  -> invoke tc cs d s $ reverse prev 
                  g':conj' -> eval tc cs d s prev g' conj' 
eval tc cs e s prev g@(i, p, g1 :\/: g2) conjs = 
  let (tc',  node' ) = eval tc  cs e s prev (i, p, g1) conjs in
  let (tc'', node'') = eval tc' cs e s prev (i, p, g2) conjs in 
  (tc'', Or node' node'' (conj $ map trd' $ (reverse prev) ++ g:conjs))
eval tc cs e s prev (i, p, g1 :/\: g2) conjs = eval tc cs e s prev (i, p, g1) ((i, p, g2):conjs)
eval tc cs e s prev g@(i, p, Invoke _ _) (g':conjs') = eval tc cs e s (g:prev) g' conjs'
eval tc cs e s prev g@(i, p, Invoke _ _) []          = invoke tc cs e s (reverse $ g:prev)

unfold :: TreeContext -> Stack -> E.Delta -> E.Sigma -> [Zeta] -> (TreeContext, Tree)
unfold tc _ _ s []            = (tc, Success s)
unfold (sr, args, ids) cs e s conjs =
  let cs_conjs     = map (\ (_, _, Invoke f as) -> Invoke f $ map (E.substitute s) as) conjs in
  let (e', conjs') = foldl (\ (d, conj) (i, p, Invoke f as) ->
                               let (_, fs, g) = p f in
                               let i'         = foldr (\ (f, a) i' -> E.extend i' f a) i $ zip fs as in  
                               let (g', (p', i'', d')) = E.pre_eval (p, i', d) g in
                               (d', (i'', p', g'):conj)
                           ) (e, []) (reverse conjs)
  in
  let id:ids'      = ids    in  
  let h:t          = conjs' in
  let (tc', node)  = eval (sr, args, ids') ((id, cs_conjs):cs) e' s [] h t in
  (tc', Call id node ( conj $ (Invoke (show s) [] ) :  map trd' conjs))

drive :: G X -> (TreeContext, Tree)
drive goal =
  let (goal', (g', i', d')) = E.pre_eval E.env0 goal in
  eval emptyContext [] d' E.s0 [] (i', g', goal') []


tc = drive (appendo -- (fresh ["p", "q", "r"] -- (call "appendo" [V "p", V "q", V "r"]))
              (fresh ["q", "r", "s", "t", "p"] 
                 (call "appendo" [V "q", V "r", V "s"] &&& 
                  call "appendo" [V "s", V "t", V "p"])
              )
           )

tree = snd $ tc

