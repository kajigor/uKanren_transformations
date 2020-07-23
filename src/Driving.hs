{-# LANGUAGE TupleSections #-}

module Driving where

import           Data.Foldable
import           Data.List              hiding (group, groupBy)
import qualified Data.Map.Strict        as Map
import           Data.Maybe
import qualified Data.Set               as Set
import qualified Eval                   as E
import           Generalization         (Generalizer, generalizeGoals)
import           Stream
import           Syntax
import           Tree
import           Util.Miscellaneous

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

renameGoals :: [G S] -> [G S] -> Maybe Renaming
renameGoals as bs = do
  a <- conj as
  b <- conj bs
  rename a b

{-embed :: G S -> G S -> Bool
embed g@(g1 :/\: g2) (h1 :/\: h2) = embed g1 h1 && embed g2 h2 || embed g h1 || embed g h2
embed g (g1 :/\: g2) = embed g g1 || embed g g2
embed (Invoke f fs) (Invoke g gs) | f == g && length fs == length gs = embedTerms fs gs
embed _ _ = False
-}
embedTerm :: Ts -> Ts -> Bool
embedTerm (V _) (V _) = True
embedTerm (C n ns) (C m ms) | n == m && length ms == length ns = and $ zipWith embedTerm ns ms
embedTerm t (C _ ns) = or $ zipWith embedTerm (repeat t) ns
embedTerm _ _ = False

embedTerms :: [Ts] -> [Ts] -> Bool
embedTerms ps qs | length ps == length qs = and $ zipWith embedTerm ps qs
embedTerms _ _   = False

embedGoals :: [G S] -> [G S] -> Bool
embedGoals gs hs = coupleConj gs hs || diveConj gs hs where
  coupleConj [] [] = True
  coupleConj (Invoke f fs : as) (Invoke g gs : bs) | f == g && length fs == length gs = embedTerms fs gs && embedConj as bs
  coupleConj _ _ = False

  embedConj as bs = coupleConj as bs || diveConj as bs

  diveConj as (b:bs) = embedConj as bs
  diveConj _ _       = False

-- Embedding
-- embed :: G S -> G S -> Maybe Renaming
-- embed g1' g2' = embedGoal (Just []) (g1', g2') where
--   embedGoal r (g1 :/\:  g2, h1 :/\: h2 ) =
--     msum [ foldl embedGoal r [(g1, h1), (g2, h2)]
--          , embedGoal r (g1 :/\: g2, h1)
--          , embedGoal r (g1 :/\: g2, h2)
--          ]
--   embedGoal r (Invoke f fs, Invoke g gs) | f == g && length fs == length gs = embedTerms r fs gs
--   embedGoal _ (Invoke _ _, _ :/\: _) = Nothing
--   embedGoal r (g, g1 :/\: g2) = msum $ map (embedGoal r . (g,)) [g1, g2]
--   embedGoal _ _ = Nothing
--
-- embedTerm :: Maybe Renaming -> (Term S, Term S) -> Maybe Renaming
-- embedTerm r (V x, V y) | x == y = r
-- embedTerm r (V x, V y) = r >>= (\ r' -> case lookup x r' of
--                                          Just z  -> if z == y then Just r' else Nothing
--                                          Nothing -> Just $ (x, y) : r'
--                                )
-- embedTerm r (C m ms, C n ns) | m == n && length ms == length ns = foldl embedTerm r $ zip ms ns
-- embedTerm r (t     , C _ ns) = msum $ map (embedTerm r . (t,)) ns
-- embedTerm _ _ = Nothing
--
-- embedTerms :: Maybe Renaming -> [Term S] -> [Term S] -> Maybe Renaming
-- embedTerms r ps qs | length ps == length qs = foldl embedTerm r $ zip ps qs
-- embedTerms _ _  _  = Nothing
--
-- embedGoals :: [G S] -> [G S] -> Maybe Renaming
-- embedGoals = embedConj [] where -- coupling restriction relaxed
--   embedConj r as' bs' = mplus (coupleConj r as' bs') (diveConj r as' bs')
--
--   coupleConj r [] [] = Just r
--   coupleConj r (Invoke f fs : as) (Invoke g gs : bs) | f == g && length fs == length gs =
--     embedTerms (Just r)  fs gs >>= \r' -> embedConj r' as bs
--   coupleConj _ _ _ = Nothing
--
--   diveConj r as' (_:bs') = embedConj r as' bs'
--   diveConj _ _ _         = Nothing

substitute :: E.Sigma -> G S -> G S
substitute s (t1 :=: t2  ) = E.substitute s t1 :=: E.substitute s t2
substitute s (g1 :/\: g2 ) = substitute s g1 :/\: substitute s g2
substitute s (g1 :\/: g2 ) = substitute s g1 :\/: substitute s g2
substitute s (Invoke f as) = Invoke f $ map (E.substitute s) as

weakCouple :: (G S, G S) -> Bool
weakCouple (Invoke f _, Invoke g _) | f == g = True
weakCouple _                        = False

split :: [Zeta] -> [G S] -> [[Zeta]]
split gs1 gs2 = filter (not . null) $ split' gs1 gs2 where
  split' gs1 gs2 = -- map (:[]) gs1
    -- filter (not . null) $
    let (coupled, gs1', gs2') = getCoupledPref gs1 gs2 in
    case gs2' of
      []    -> [coupled, gs1']
      (g:_) -> let (dived, rest) = getDivedPref gs1' g in
               let (hd : tl) = split' rest gs2' in
               (coupled ++ hd) : dived : tl
    where getCoupledPref gs1' gs2' =
            let ind = fromMaybe (length gs2') $ elemIndex False $ zipWith (curry weakCouple) (map trd3 gs1') gs2' in
            let (coupled, rest) = splitAt ind gs1' in
            (coupled, rest, drop ind gs2')
          getDivedPref gs g = span (\(_,_,x) -> not $ weakCouple (g, x)) gs


update :: (E.P, E.Delta) -> Def -> (E.P, E.Delta)
update (p, d) def = let (p', _, d') = E.update (p, E.emptyIota, d) def in (p', d')


invoke :: TreeContext -> Stack -> E.Delta -> E.Sigma -> Generalizer -> [Zeta] -> (TreeContext, Tree, E.Delta)
invoke tc@(sr, args, ids) cs d s gen conjs =
  -- HERE WE HAVE TO SUBSTITUTE INTO THE CURRENT GOAL
 let qqq = map (\(a, b, g) -> (a, b, substitute s g)) conjs in
 let qqq_conjs = map trd3 qqq in
  {-
 if length conjs > 3 -- head ids > 100
 then
    case find (\ (_, conjs') -> (embedGoals conjs' qqq_conjs)) cs of
      Nothing     -> trace "AHA" $ (tc, Prune [conj qqq_conjs], d)
      Just (_, j) -> (tc, Prune [conj qqq_conjs, Invoke "Embedding" [],  conj j], d)
 else-}
  -- let qqq = map (\(a, b, g) -> (a, b, substitute s g)) conjs in
  -- let qqq_conjs = map trd' qqq in
  let p = snd3 $ head conjs in
  let g = unsafeConj qqq_conjs in
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
      case find (\ (_, conjs') -> (embedGoals conjs' qqq_conjs)) cs of
        Just (_, conjs') ->
          if length qqq == length conjs'
          then
               let (msg_, s1, s2, d') = generalizeGoals d qqq_conjs conjs' in -- ADD GENERALIZER
               let msg = unsafeConj msg_ in
               let (tc', node, d'') = eval (sr, args, ids') ((id, qqq_conjs):cs) d' s s1 [] (fst3 $ head conjs, p, msg) [] in
               (tc', Gen id s1 node msg s, d'')
          else if length conjs' < length qqq
               then let context  = (id, qqq_conjs):cs in
                    let splitted = split qqq conjs' in
                    --trace ("\nSplitted " ++ show (map (map trd') splitted)) $
                    let (tc'', d'', nodes) = foldl (\(tc, d, nodes) x ->
                                                       let (tc', node', d') = eval tc context d s gen [] (head x) (tail x)
                                                       in  (tc', d', node' : nodes)
                                                   ) ((sr, args, ids'), d, []) splitted
                    in (tc'', Split id (reverse nodes) g s, d'') -- TODO check if reverse is ok!
               else error "Wow..."
        Nothing -> unfold tc cs d s gen qqq

type Zeta = (E.Iota, E.P, G S)

eval :: TreeContext -> Stack -> E.Delta -> E.Sigma -> Generalizer -> [Zeta] -> Zeta -> [Zeta]  -> (TreeContext, Tree, E.Delta)
eval tc cs d s gen prev g@(i, p, t1 :=: t2) conjs =
  case takeS 1 $ E.eval (p, i, d) s (trd3 g) of
    []       -> (tc, Fail, d)
    [(s, _)] -> case conjs of
                  []       -> case prev of
                                [] -> unfold tc cs d s gen []
                                _  -> invoke tc cs d s gen $ reverse prev
                  g':conj' -> eval tc cs d s gen prev g' conj'
eval tc cs d s gen prev g@(i, p, g1 :\/: g2) conjs =
  let (tc',  node' , d' ) = eval tc  cs d  s gen prev (i, p, g1) conjs in
  let (tc'', node'', d'') = eval tc' cs d' s gen prev (i, p, g2) conjs in
  (tc'', Or node' node'' (unsafeConj $ map trd3 $ (reverse prev) ++ g:conjs) s, d'')
eval tc cs d s gen prev (i, p, g1 :/\: g2) conjs = eval tc cs d s gen prev (i, p, g1) ((i, p, g2):conjs)
--eval tc cs d s gen prev (i,p,Zzz g) conjs = eval tc cs d s gen prev (i,p,g) conjs
eval tc cs d s gen prev g@(_, _, Invoke _ _) (g':conjs') = eval tc cs d s gen (g:prev) g' conjs'
eval tc cs d s gen prev g@(_, _, Invoke _ _) []          = invoke tc cs d s gen (reverse $ g:prev)

unfold :: TreeContext -> Stack -> E.Delta -> E.Sigma -> Generalizer -> [Zeta] -> (TreeContext, Tree, E.Delta)
unfold tc _ d s _ []            = (tc, Success s, d)
unfold (sr, args, ids) cs e s gen conjs =
  let cs_conjs     = map (\ (_, _, Invoke f as) -> Invoke f $ map (E.substitute s) as) conjs in
  let (e', conjs') = foldl (\ (d, conj) (i, p, zyz@(Invoke f as)) ->
                               let (Def _ fs g) = p f in
                               let i'           = foldl (\ interp (f, a) -> E.extend interp f a) i $ zip fs as in
                               let (g', (p', i'', d'), _) = E.preEval (p, i', d) g in
                               (d', (i'', p', g'):conj)
                           ) (e, []) conjs
  in
  let id:ids'      = ids    in
  let h:t          = reverse conjs' in
  let (tc', node, d')  = eval (sr, args, ids') ((id, cs_conjs):cs) e' s gen [] h t in
  (tc', Call id node ( unsafeConj $ {- (Invoke (show s) [] ) : -} map trd3 conjs) s, d')

drive :: G X -> (TreeContext, Tree, [Id])
drive goal =
  let (goal', (g', i', d'), args) = E.preEval E.env0 goal in
  let (x, y, _) = eval emptyContext [] d' E.s0 [] [] (i', g', goal') [] in (x, y, reverse args)
