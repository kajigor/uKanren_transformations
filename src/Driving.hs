{-# LANGUAGE TupleSections #-}

module Driving where

import           Control.Exception.Base
import           Control.Monad          (mplus)
import           Data.Foldable
import           Data.Function
import           Data.List
import qualified Data.Map.Strict        as Map
import           Data.Maybe
import qualified Data.Set               as Set
import           Debug.Trace
import qualified Eval                   as E
import           List
import           Stream
import           Syntax
import           Tree

type TreeContext = (Set.Set Id, Map.Map Id [S], [Id])

emptyContext = (Set.empty, Map.empty, [0..])

updateTc tc id as =
  let (sr, args, ids) = tc in
  (Set.insert id sr, Map.insert id as args, ids)

type Stack = [(Id, [G S])]

rename :: G S -> G S -> Maybe Renaming
rename g1 g2 = rename' (Just []) (g1, g2) where
  rename' r (s1 :=:  s2, t1 :=:  t2)                                      = renameTerms r [s1, s2] [t1, t2]
  rename' r (g1 :/\: g2, h1 :/\: h2)                                      = renameGoals r [g1, g2] [h1, h2]
  rename' r (g1 :\/: g2, h1 :\/: h2)                                      = renameGoals r [g1, g2] [h1, h2]
  rename' r (Invoke f as, Invoke g bs) | f == g && length as == length bs = renameTerms r as bs
--  rename' r (Zzz f, Zzz g) = rename' r (f, g)
  rename' _  _                                                            = Nothing
  renameTerm r (C m ms, C n ns) | m == n && length ms == length ns = renameTerms r ms ns
  renameTerm r (V x, V y) = r >>= (\r -> case lookup x r of
                                           Nothing -> Just $ (x, y) : r
                                           Just z  -> if z == y then Just r else Nothing
                                  )
 {- renameTerm r (V x, C n ns) = r >>= ( \r -> case lookup x r of
                                               Nothing -> Just $ (x, x) : r
                                               Just z -> Nothing
                                     ) -}
  renameTerm  _ _       = Nothing
  renameTerms           = renames renameTerm
  renameGoals           = renames rename'
  renames     f r ms ns = foldl f r $ zip ms ns

-- conj [] = error "Empty conjunction"
-- conj (a:as) = foldl (:/\:) a as

renameGoals :: [G S] -> [G S] -> Maybe Renaming
renameGoals as bs = rename (conj as) (conj bs)

-- Embedding
embed :: G S -> G S -> Maybe Renaming
embed g1 g2 = embedGoal (Just []) (g1, g2) where
  embedGoal r (g1 :/\:  g2, h1 :/\: h2 ) =
    msum [ foldl embedGoal r $ [(g1, h1), (g2, h2)]
         , embedGoal r (g1 :/\: g2, h1)
         , embedGoal r (g1 :/\: g2, h2)
         ]
--  embedGoal r (Zzz g, Zzz h) = embedGoal r (g, h)
--  embedGoal r (g, Zzz h) = embedGoal r (g, h)
  embedGoal r (Invoke f fs, Invoke g gs) | f == g && length fs == length gs = embedTerms r fs gs
  embedGoal r (Invoke _ _, g1 :/\: g2) = Nothing
  embedGoal r (g, g1 :/\: g2) = msum $ map (embedGoal r . (g,)) [g1, g2]
  embedGoal _ _ = Nothing

embedTerm r (V x, V y) | x == y = r
embedTerm r (V x, V y) = r >>= (\ r -> case lookup x r of
                                         Just z  -> if z == y then Just r else Nothing
                                         Nothing -> Just $ (x, y) : r
                               )
embedTerm r (C m ms, C n ns) | m == n && length ms == length ns = foldl embedTerm r $ zip ms ns
embedTerm r (t     , C n ns)                = msum $ map (embedTerm r . (t,)) ns
embedTerm _  _                              = Nothing
embedTerms r ps qs | length ps == length qs = foldl embedTerm r $ zip ps qs
embedTerms _ _  _  = Nothing

embedGoals :: [G S] -> [G S] -> Maybe Renaming
embedGoals as bs = coupleConj [] as bs where
  embedConj r as bs = mplus (coupleConj r as bs) (diveConj r as bs)

  coupleConj r [] [] = Just r
  coupleConj r ((Invoke f fs):as) ((Invoke g gs):bs) | f == g && length fs == length gs = embedTerms (Just r)  fs gs >>= \r -> embedConj r as bs
  coupleConj _ _ _ = Nothing

  diveConj r as' (b:bs') = embedConj r as' bs'
  diveConj _ _ _         = Nothing

-- tttt = embedGoals [Invoke "MM" (map V [3,5,1,7]), Invoke "S" (map V [4,5,6])] [Invoke "L" [V 3, V 5, C "T" []], Invoke "MM" (map V [8, 10, 5, 12]), Invoke "S" (map V [9, 10, 11])]
inv = Invoke
-- ((leo([V 3,         V 5, C "true" []])  /\ minmaxo([V 8 ,V 10,V 5, V 12])) /\ smallesto([V 9,V 10,V 11]))
-- (((gto([V 31,V 32,C "true" []]) /\ leo([C "S" [V 32],V 21,C "true" []])) /\ minmaxo([V 33,V 35,V 21,V 37])) /\ smallesto([V 34,V 35,V 36]))

tttt = embedGoals [inv "L" [V 3, V 5, C "T" []]  , inv "M" [V 8 ,V 10,V 5, V 12],          inv "S" [V 9,V 10,V 11]]
                  [inv "G" [V 31, V 32, C "T" []], inv "L" [C "S" [V 32], V 21, C "T" []], inv "M" [V 33,V 35,V 21,V 37], inv "S" [V 34,V 35,V 36]]
                  

--sub s [] = []
--sub s ((Invoke name as):conjs) = (Invoke name (\x -> lookup x s)) : (sub s conjs)

refine :: ([G S], Generalizer, Generalizer, [S]) ->  ([G S], Generalizer, Generalizer, [S])
refine msg@(g, s1, s2, d) =
  let similar1 = filter ((>1) . length) $ groupBy group s1 [] in
  let similar2 = filter ((>1) . length) $ groupBy group s2 [] in
  let toSwap = concat $ map (\((x,_):xs) -> map (\(y, _) -> (y, V x)) xs) similar1 in
  let newGoal = E.substituteConjs toSwap g in
  let s1' = filter (\(x,_) -> not $ elem x (concat $ map (\xs -> tail $ map fst xs ) similar1)) s1 in
  let s2' = filter (\(x,_) -> not $ elem x (concat $ map (\xs -> tail $ map fst xs ) similar2)) s2 in
  trace ("\nSimilar1: " ++ show similar1 ++ "\nSimilar2: " ++ show similar2 ++ "\nToSwap: "  ++ show toSwap ++ "\nNewGoal: " ++ show newGoal ++ "\nS1': " ++ show s1' ++ "\nS2': " ++ show s2' ++ "\n") $
  (newGoal, s1', s2', d)
  {-
 let grouped =  filter ((>1) . length) $ groupBy group s1 [] in
  trace ("\nIn refine\ns1: " ++ show s1 ++ "\ngrouped: " ++ show grouped) $
  case filter ((>1) . length) $ groupBy group s1 [] of
    [] -> msg
    gs -> foldl (\ _ g1 ->
                    let restriction = filter (\ (x, _) -> isJust $ lookup x g1) s2 in
                    trace ("\nRestriction: " ++ show restriction) $
                    case filter ((>1) . length) $ groupBy group restriction [] of
                      [] -> msg
                      g2 ->
                            trace ("\nmsg: "  ++ show ((\(x, y, z, _) -> (x, y, z)) msg) ++ "\ng2:  " ++ show g2) $

                            foldl (\ (g, s1, s2, d) ((x, _):bs) ->
                                     (
                                      -- let substituted = E.substituteConjs (map (\ (y, _) -> (y, V x)) bs) g in trace ("\nBefore: " ++ show g ++ "\nAfter:  " ++ show substituted ) substituted,
                                      trace ("\nAND THIS IS BS:  " ++ show bs ++ "\nAND THIS IS G:   " ++ show g  ++ "\nAND THIS IS BS': " ++ show (map (\ (y, _) -> (y, V x)) bs) ++ "\n") $ E.substituteConjs (map (\ (y, _) -> (y, V x)) bs) g,
                                      filter (\ (x, _) -> isNothing $ lookup x bs) s1,
                                      filter (\ (x, _) -> isNothing $ lookup x bs) s2,
                                      d
                                     )
                                  )
                                  msg
                                  g2
                )
                msg
                gs  -}
  where
    groupBy _ [] acc = acc
    groupBy p xs acc = let (similar, rest) = partition (\y -> p (head xs) y) xs in assert (similar /= []) $ groupBy p rest (similar : acc)
    group x y = snd x == snd y

{-
refine msg@(g, s1, s2) =
 let grouped =  filter ((>1) . length) $ groupBy group s1 [] in
  trace ("\nIn refine\ns1: " ++ show s1 ++ "\ngrouped: " ++ show grouped) $
  case filter ((>1) . length) $ groupBy group s1 [] of
    [] -> msg
    gs -> foldl (\ acc g1 ->
                    let restriction = filter (\ (x, _) -> isJust $ lookup x g1) s2 in
                    trace ("\nRestriction: " ++ show restriction) $
                    case filter ((>1) . length) $ groupBy group restriction [] of
                      [] -> msg
                      g2 ->
                            trace ("\nmsg: "  ++ show msg ++ "\ng2:  " ++ show g2) $
                            foldl (\ (g, s1, s2) ((x, _):bs) ->
                                     (
                                      let substituted = E.substitute (map (\ (y, _) -> (y, V x)) bs) g in trace ("\nBefore: " ++ show g ++ "\nAfter:  " ++ show substituted ) substituted,
                                      filter (\ (x, _) -> isNothing $ lookup x bs) s1,
                                      filter (\ (x, _) -> isNothing $ lookup x bs) s2
                                     )
                                  )
                                  msg
                                  g2
                )
                msg
                gs
  where
    groupBy _ [] acc = acc
    groupBy p xs acc = let (similar, rest) = partition (\y -> p (head xs) y) xs in groupBy p rest (similar : acc)
    group x y = snd x == snd y
-}
generalize :: [S] -> (Generalizer, Generalizer) -> [G S] -> [G S] -> ([G S], Generalizer, Generalizer, [S])
generalize d gg gs hs =
  (\(x, (y,z), t) -> (reverse x, y, z, t)) $
  foldl (\ (goals, s, vs) gh ->
           let ((g, s1, s2), vs') = generalizeCall vs s gh in
           (g:goals, (s1, s2), vs')
        )
        ([], gg, d)
        (zip gs hs)

generalizeCall d gg (Invoke f as, Invoke g bs) =
  let ((C _ cs, s1, s2), d') = generalizeTerm d gg (C "()" as, C "()" bs) in
  ((Invoke f cs, s1, s2), d')


{-
generalize d gg (g1 :/\: g2) (h1 :/\: h2) =
  let (i, s1' , s2' , d' ) = generalize d  gg         g1 h1 in
  let (j, s1'', s2'', d'') = generalize d' (s1', s2') g2 h2 in
  (i :/\: j, s1'', s2'', d'')
generalize d gg (Invoke f as) (Invoke g bs) =
  let (msg, d')        = generalizeTerm d gg (C "()" as, C "()" bs) in
  let (C _ cs, s1, s2) = refine msg in
  (Invoke f cs, s1, s2, d')
-}

generalizeTerm vs s@(s1, s2) (C m ms, C n ns) | m == n && length ms == length ns =
  let (gs, (s1, s2), vs') = foldl (\ (gs, s, vs) ts ->
                                        let ((g, s1, s2), vs') = generalizeTerm vs s ts in
                                        (g:gs, (s1, s2), vs')
                                  ) ([], s, vs) $ zip ms ns
  in
  ((C m $ reverse gs, s1, s2), vs')
generalizeTerm vs (s1, s2) (V x, V y) | x == y = ((V x, s1, s2), vs)
generalizeTerm (v:vs) (s1, s2) (t1, t2) = ((V v, (v, t1):s1, (v, t2):s2), vs)


generalizeGoals :: [S] -> [G S] -> [G S] -> ([G S], Generalizer, Generalizer, [S])
generalizeGoals s as bs =
  let res@(msg_, s1_, s2_, _) = refine $ generalize s ([], []) as bs in
  -- trace ("Generalizing\nFirst:  " ++ show as ++ "\nSecond: " ++ show bs ++ "\nmsg: " ++ show msg_ ++ "\ns1:  " ++ show s1_ ++ "\ns2:  " ++ show s2_) $
  res


substitute :: E.Sigma -> G S -> G S
substitute s (t1 :=: t2  ) = E.substitute s t1 :=: E.substitute s t2
substitute s (g1 :/\: g2 ) = substitute s g1 :/\: substitute s g2
substitute s (g1 :\/: g2 ) = substitute s g1 :\/: substitute s g2
substitute s (Invoke f as) = Invoke f $ map (E.substitute s) as

weakCouple :: (G S, G S) -> Bool
weakCouple (Invoke f _, Invoke g _) | f == g = True
weakCouple _                        = False

fst' (x, _, _) = x
snd' (_, x, _) = x
trd' (_, _, x) = x

split :: [Zeta] -> [G S] -> ([Zeta], [Zeta])
split gs1 gs2 =
  case elemIndex False $ map weakCouple $ zip (map trd' gs1) gs2 of
    Nothing -> splitAt (length gs2) gs1
    Just i  -> splitAt i            gs1

update :: (E.P, E.Delta) -> Def -> (E.P, E.Delta)
update (p, d) def = let (p', _, d') = E.update (p, E.emptyIota, d) def in (p', d')


invoke :: TreeContext -> Stack -> E.Delta -> E.Sigma -> Generalizer -> [Zeta] -> (TreeContext, Tree, E.Delta)
invoke tc@(sr, args, ids) cs d s gen conjs =
  -- HERE WE HAVE TO SUBSTITUTE INTO THE CURRENT GOAL
 let qqq = map (\(a, b, g) -> (a, b, substitute s g)) conjs in
 let qqq_conjs = map trd' qqq in
  
 if length conjs > 3 -- head ids > 100
 then 
    case find (\ (_, conjs') -> (isJust $ embedGoals conjs' qqq_conjs)) cs of 
      Nothing     -> (tc, Prune [conj qqq_conjs], d)
      Just (_, j) -> (tc, Prune [conj qqq_conjs, conj j], d)
 else
  -- let qqq = map (\(a, b, g) -> (a, b, substitute s g)) conjs in
  -- let qqq_conjs = map trd' qqq in
  let p = snd' $ head conjs in
  let g = conj qqq_conjs in
  let id:ids' = ids in
  case find (\ (_, conjs') -> isJust $ renameGoals conjs' qqq_conjs) cs of
    Just (di, conjs') ->
      let r = fromJust (renameGoals conjs' qqq_conjs) in
      (
        updateTc tc di (map fst r),
        Rename di g s r s,
        d
      )
    Nothing ->
      case find (\ (_, conjs') -> (isJust $ embedGoals conjs' qqq_conjs)) cs of
        Just (_, conjs') ->
          if length qqq == length conjs'
          then
               let (msg_, s1, s2, d') = generalizeGoals d qqq_conjs conjs' in -- ADD GENERALIZER
               let msg = conj msg_ in
               let (tc', node, d'') = eval (sr, args, ids') ((id, qqq_conjs):cs) d' s s1 [] (fst' $ head conjs, p, msg) [] in
               (tc', Gen id s1 node msg s, d'')
          else if length conjs' < length qqq
               then let context            = (id, qqq_conjs):cs in
                    let gbp@(lh:lt, rh:rt) = split qqq conjs' in
                    let (tc' , node' , d' ) = eval (sr, args, ids') context d s gen [] lh lt in
                    let (tc'', node'', d'') = eval tc' context d' s gen [] rh rt in
                    -- trace ("\nconjs': " ++ show conjs' ++ "\nqqq:    " ++ show qqq_conjs ++ "\n\nsplit: " ++ show (map trd' $ fst gbp) ++ "\n       " ++ show (map trd' $ snd gbp)) $
                    (tc'', Split id node' node'' g s, d'')
               else error "Wow..."
        Nothing -> unfold tc cs d s gen qqq

type Zeta = (E.Iota, E.P, G S)

eval :: TreeContext -> Stack -> E.Delta -> E.Sigma -> Generalizer -> [Zeta] -> Zeta -> [Zeta]  -> (TreeContext, Tree, E.Delta)
eval tc cs d s gen prev (i, p, Let def g) conjs =
  let (p', d') = update (p, d) def in
  eval tc cs d' s gen prev (i, p', g) conjs
eval tc cs d s gen prev g@(i, p, t1 :=: t2) conjs =
  case takeS 1 $ E.eval (p, i, d) s (trd' g) of
    []       -> (tc, Fail, d)
    [(s, _)] -> case conjs of
                  []       -> case prev of
                                [] -> unfold tc cs d s gen []
                                _  -> invoke tc cs d s gen $ reverse prev
                  g':conj' -> eval tc cs d s gen prev g' conj'
eval tc cs d s gen prev g@(i, p, g1 :\/: g2) conjs =
  let (tc',  node' , d' ) = eval tc  cs d  s gen prev (i, p, g1) conjs in
  let (tc'', node'', d'') = eval tc' cs d' s gen prev (i, p, g2) conjs in
  (tc'', Or node' node'' (conj $ map trd' $ (reverse prev) ++ g:conjs) s, d'')
eval tc cs d s gen prev (i, p, g1 :/\: g2) conjs = eval tc cs d s gen prev (i, p, g1) ((i, p, g2):conjs)
--eval tc cs d s gen prev (i,p,Zzz g) conjs = eval tc cs d s gen prev (i,p,g) conjs
eval tc cs d s gen prev g@(_, _, Invoke _ _) (g':conjs') = eval tc cs d s gen (g:prev) g' conjs'
eval tc cs d s gen prev g@(_, _, Invoke _ _) []          = invoke tc cs d s gen (reverse $ g:prev)

unfold :: TreeContext -> Stack -> E.Delta -> E.Sigma -> Generalizer -> [Zeta] -> (TreeContext, Tree, E.Delta)
unfold tc _ d s _ []            = (tc, Success s, d)
unfold (sr, args, ids) cs e s gen conjs =
  let cs_conjs     = map (\ (_, _, Invoke f as) -> Invoke f $ map (E.substitute s) as) conjs in
  let (e', conjs') = foldl (\ (d, conj) (i, p, zyz@(Invoke f as)) ->
                               let (_, fs, g) = p f in
                               let i'         = foldl (\ interp (f, a) -> E.extend interp f a) i $ zip fs as in
                               let (g', (p', i'', d'), _) = E.pre_eval' (p, i', d) g in
                               (d', (i'', p', g'):conj)
                           ) (e, []) conjs
  in
  let id:ids'      = ids    in
  let h:t          = reverse conjs' in
  let (tc', node, d')  = eval (sr, args, ids') ((id, cs_conjs):cs) e' s gen [] h t in
  (tc', Call id node ( conj $ {- (Invoke (show s) [] ) : -} map trd' conjs) s, d')

drive :: G X -> (TreeContext, Tree, [Id])
drive goal =
  let (goal', (g', i', d'), args) = E.pre_eval' E.env0 goal in
  let (x, y, _) = eval emptyContext [] d' E.s0 [] [] (i', g', goal') [] in (x, y, reverse args)


tc' = drive (reverso $ fresh ["q", "r"] (call "reverso" [V "q", V "r"]))

tc = drive (appendo
              (fresh ["q", "r", "s", "t", "p"]
                 (call "appendo" [V "q", V "r", V "s"] &&&
                  call "appendo" [V "s", V "t", V "p"])
              )
           )

tc'' = drive (revAcco $ fresh ["q", "s"] (call "revacco" [V "q", nil, V "s"]))

tree = snd' $ tc

