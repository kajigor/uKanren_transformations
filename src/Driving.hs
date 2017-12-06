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
--import Debug.Trace
import List

trace _ = id

type TreeContext = (Set.Set Id, Map.Map Id [S], [Id])

emptyContext :: TreeContext
emptyContext = (Set.empty, Map.empty, [0..])

type Stack = [(Id, String, [Ts], [G S])]

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
{-
  do r <-  rename (conj as) (conj bs)
     case filter (\ (x, y) -> x /= y) r of
       [] -> Nothing
       _  -> return r 
-}

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
  embedTerms r ps qs | length ps == length qs = trace ("Embed terms\n") $ foldl embedTerm r $ zip ps qs
  embedTerms _ _  _                           = trace ("Embed terms (Nothing)\n") $ Nothing

embedGoals :: [G S] -> [G S] -> Maybe Renaming
embedGoals as bs = trace ("Embed goals\n") $ embed (conj as) (conj bs)

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

split :: [Zeta] -> [G S] -> ([Zeta], [Zeta])
split gs1 gs2 = 
  case elemIndex False $ map weakCouple $ zip (map snd gs1) gs2 of
    Nothing -> splitAt (length gs2) gs1
    Just i  -> splitAt (i-1)        gs1

type Epsilon = (E.P, E.Delta) -- WRONG! Should be Delta -> (E.P, E.Iota)

update :: Epsilon -> Def -> Epsilon
update (p, d) def = let (p', _, d') = E.update (p, (\ _ -> V 3), d) def in (p', d')

invoke :: TreeContext -> Stack -> Epsilon -> E.Sigma -> Zeta -> [Zeta] -> (TreeContext, Tree) 
invoke (sr, args, ids) cs (p, d) s goal@(i, Invoke f as') conjs =
  trace ("Invoke\n") $
  let (_, fs, g) = p f in
  case find (\ (_, g, bs, conjs') -> isJust $ renameGoals (Invoke g bs : conjs') (Invoke f as' : map snd conjs)) cs of 
    Just (di, g, bs, conjs') ->
      let id:ids' = ids in
      let r       = fromJust (renameGoals (Invoke g bs : conjs') (Invoke f as' : map snd conjs)) in      
      ((Set.insert di sr, Map.insert di (map fst r) args, ids'),      
       Split
         id
         (Success s) 
         (Rename di (conj (Invoke f as' : conjs')) r)
         (conj (Invoke f as' : conjs'))
      )
    Nothing -> 
      trace ("Trying embedding...\n") $
      case find (\ (_, g, bs, conjs') -> (isJust $ embedGoals (Invoke g bs : conjs') (Invoke f as' : map snd conjs))) cs of
        Just (_, g, bs, conjs') -> 
          trace ("Found embedding\n") $
          if length conjs == length conjs' 
          then let x = (Invoke f as' : map snd conjs)
                   y = (Invoke g bs  : conjs')
               in
               let (msg, s1, s2, d') = generalizeGoals d (Invoke f as' : map snd conjs) (Invoke g bs : conjs') in
               trace ("HERE\n" ++ "x: " ++ show x ++ "\ny: " ++ show y ++ "\ng: " ++ show msg  ) $
               let id:ids' = ids in
               let (tc', node) = eval (sr, args, ids') ((id, f, as', map snd conjs):cs) (p, d') (s1 `E.o` s) [] (i, msg) [] in
               (tc', Gen id s1 node (conj $ map snd $ goal:conjs))
          else if length conjs' < length conjs 
               then trace ("In Split") $
                    let id:ids'       = ids in                    
                    let cs'           = (id, f, as', map snd conjs):cs in
                    let (left, right) = split ((i, Invoke f as') : conjs) (Invoke g bs : conjs') in
                    trace ("SPLITTED into \n" ++ show (map snd left) ++ "\nAND\n" ++ show (map snd right)) $
                    let lh:lt = left  in
                    let rh:rt = right in
                    let (tc' , node' ) = eval (sr, args, ids') cs' (p, d) s [] lh lt in
                    let (tc'', node'') = eval tc'              cs' (p, d) s [] rh rt in
                    (tc'', Split id node' node'' (conj $ map snd $ goal:conjs))
               else error "Wow..."
        Nothing -> let (g', (p', i', d')) = trace (show g) (E.pre_eval (p, i, d) g) in
                   trace (show g') $
                   let id:ids' = ids in
                   let (tc', node) = eval (sr, args, ids') ((id, f, as', map snd conjs) : cs) (p', d') s [] (i', g') conjs in
                   (tc', Call id node (conj $ map snd $ goal:conjs))

type Zeta = (E.Iota, G S)

eval :: TreeContext -> Stack -> Epsilon -> E.Sigma -> [Zeta] -> Zeta -> [Zeta]  -> (TreeContext, Tree)
eval tc cs e s prev   (i, Let def g) conjs = eval tc cs (update e def) s prev (i, g) conjs  
eval tc cs e@(p, d) s prev g@(i, t1 :=: t2) conjs =
  case takeS 1 $ E.eval (p, i, d) s (snd g) of
    []       -> (tc, Fail)
    [(s, _)] -> case conjs of
                  []       -> unfold tc cs e s $ reverse prev 
                  g':conj' -> eval tc cs e s prev g' conj' 
eval tc cs e s prev g@(i, g1 :\/: g2) conjs = 
  let (tc',  node' ) = eval tc  cs e s prev (i, g1) conjs in
  let (tc'', node'') = eval tc' cs e s prev (i, g2) conjs in 
  (tc'', Or node' node'' (conj $ map snd $ prev ++ g:conjs))
eval tc cs e s prev (i, g1 :/\: g2) conjs = eval tc cs e s prev (i, g1) ((i, g2):conjs)
eval tc cs e s prev g@(i, Invoke _ _) (g':conjs') = eval tc cs e s (g:prev) g' conjs'
eval tc cs e s prev g@(i, Invoke _ _) [] = unfold tc cs e s (reverse $ g:prev)

unfold :: TreeContext -> Stack -> Epsilon -> E.Sigma -> [Zeta] -> (TreeContext, Tree)
unfold tc _  _ s []            = (tc, Success s)
unfold (sr, args, ids) cs e s conjs =
  let cs_conjs                = map (\ (_, Invoke f as) -> Invoke f $ map (E.substitute s) as) conjs in
  let (Invoke f as):cs_conjs' = cs_conjs in     
  let (e', conjs')            = foldl (\ ((p, d), conj) (i, Invoke f as) ->
                                         let (_, fs, g) = p f in
                                         let (g', (p', i', d')) = trace (show g) (E.pre_eval (p, i, d) g) in
                                         ((p', d'), (i', g'):conj)
                                      ) (e, []) conjs
  in
  let id:ids'     = ids    in  
  let h:t         = conjs' in
  let (tc', node) = eval (sr, args, ids') ((id, f, as, cs_conjs'):cs) e' s [] h t in
  (tc', Call id node (conj $ map snd conjs))

{-
unfold tc cs (p, d) s ((i, Invoke f as) : conjs) =
  trace ("Eval invoke:\n") $ 
  let (_, fs, g) = p f in
  let i'         = foldl (\ i' (f, a) -> E.extend i' f a) i $ zip fs as in
  let as'        = map (E.substitute s) as in
  trace ("\nSubst:\n" ++ show s ++ "\n") $
  invoke tc cs (p, d) s (i', Invoke f as') $ map (\ (i, g) -> ((\ x -> E.substitute s $ i x), substitute s g)) conjs
-}

drive :: G X -> (TreeContext, Tree)
drive goal =
  trace (show goal) (
    let (goal', (g', i', d')) = E.pre_eval E.env0 goal in
    trace (show goal') ( 
    eval emptyContext [] (g', d') E.s0 [] (i', goal') []))

{-
test =
  do
    putStrLn $ show (fresh ["q"] (call "appendo" [ V "q", i "B" % nil, i "A" % (i "B" % nil)]))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [a % nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [a % b % nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo], fresh ["q"] (call "appendo" [ V "q", b % nil, a % b % nil])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [a % nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [a % b % nil, V "q"])))
    putStrLn $ show (toplevel 1 ([appendo, reverso], fresh ["q"] (call "reverso" [V "q", a % b % nil])))
-}


tc = drive (appendo -- (fresh ["p", "q", "r"] -- (call "appendo" [V "p", V "q", V "r"]))
              (fresh ["q", "r", "s", "t", "p"] 
                 (call "appendo" [V "q", V "r", V "s"] &&& 
                  call "appendo" [V "s", V "t", V "p"])
              )
           )

tree = snd $ tc

