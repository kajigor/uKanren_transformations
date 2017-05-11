module Driver where
import MuKanren hiding (mplus)
import Control.Monad
import Debug.Trace
import Data.List (find, delete)
import Data.Maybe (isJust)
import Data.Foldable (foldrM)

type ESubst = [(Var, Either AST Term)]

data Tree subst ast = Leaf (Maybe subst) | Node Int subst ast [Tree subst ast] | Up Int subst ast | G Int subst ESubst ast [Tree subst ast]

node = Node

instance (Show subst, Show ast) => Show (Tree subst ast) where
  show t = show' t 0
   where
    nspaces n = [' ' | _ <- [1..n]]
    show' (Leaf s) n = nspaces n ++ "L " ++ show s ++ "\n"
    show' (Node d s a ns) n = nspaces n ++ "N " ++ show d ++ " " ++ show s ++ " " ++ show a ++ "\n" ++
                              concatMap (\x -> show' x (n + 1)) ns
    show' (Up d s a) n = nspaces n ++ "U " ++ show d ++ " " ++ show s ++ " " ++ " " ++ show a ++  "\n"
    show' (G d s es a ch) n = nspaces n ++ "G " ++ show d ++ " " ++ show s ++ " " ++ show es ++ " " ++ show a ++ "\n" ++ concatMap (\x -> show' x (n+1)) ch

applySubst (Conj l r)    s        = Conj (applySubst l s) (applySubst r s)
applySubst (Disj l r)    s        = Disj (applySubst l s) (applySubst r s)
applySubst (Fresh f)     (s,c)    = applySubst (f $ var c) (s,c+1)
applySubst (Fun n a)     s        = Fun n $ applySubst a s
applySubst (Zzz a)       s        = Zzz $ applySubst a s
applySubst (Call a args) s'@(s,c) = Call (applySubst a s') (map (\x -> walk' x s) args)
applySubst (Uni l r)     (s,c)    = Uni (walk' l s) (walk' r s)
applySubst (GV v)        _        = GV v

rename t =
  let (t', _) = rename_ast t ([], 0) in t'
  where
    -- Fresh should be impossible at this stage
    rename_ast (Conj l r) s =
      let (l', s')  = rename_ast l s
          (r', s'') = rename_ast r s'
      in (Conj l' r', s'')
    rename_ast (Disj l r) s =
      let (l', s')  = rename_ast l s
          (r', s'') = rename_ast r s'
      in (Disj l' r', s'')
    rename_ast (Fun n a) s =
      let (a', s') = rename_ast a s
      in (Fun n a', s')
    rename_ast (Zzz a ) s =
      let (a', s') = rename_ast a s
      in (Zzz a', s')
    rename_ast (Call a args) s =
      let s' = s
          a' = a -- (a', s')    = rename_ast a s -- this loops. Doesn't seem necessary for now anyway
          (arg', s'') = foldl (\(xs, s) x -> let (x', s') = rename_t x s in (x' : xs, s'))
                              ([], s')
                                args
      in (Call a' $ reverse arg', s'')
    rename_ast (Uni l r ) s =
      let (l', s')  = rename_t l s
          (r', s'') = rename_t r s'
      in (Uni l' r', s'')
    rename_ast x s = (x, s)

    rename_t (Var v) s@(m, c) =
      case lookup v m of
        Nothing -> let v' = Var c in (v', ((v, v') : m, c+1))
        Just v' -> (v', s)
    rename_t (Pair l r) s =
      let (l', s')  = rename_t l s
          (r', s'') = rename_t r s'
      in (Pair l' r', s'')
    rename_t x s = (x, s)

renaming l r =
  eq (rename l) (rename r)

eq l r =
  eq' l' r'
  where
    l' = rename l
    r' = rename r
    eq' (Conj l l') (Conj r r') = eq' l r && eq' l' r'
    eq' (Disj l l') (Disj r r') = eq' l r && eq' l' r'
    eq' (Uni  l l') (Uni  r r') = l == r && l' == r'
    eq' (Zzz l)     (Zzz r)     = eq' l r
    eq' (Call (Fun l _) ls) (Call (Fun r _) rs) = l == r && foldl (\acc (l, r) -> acc && l == r) True (zip ls rs)
    eq' _ _ = False

instance Eq AST where
  (==) = eq

isCoupling l r =
  case (l,r) of
    (Uni  _ _, Uni  _ _) -> True
    (Conj _ _, Conj _ _) -> True
    (Disj _ _, Disj _ _) -> True
    (Call _ _, Call _ _) -> True
    (Zzz    _, Zzz    _) -> True
    _ -> False

embed l r = -- TODO this is wrong. We should construct renaming while we inspect terms
  let
      embedT :: Term -> Term -> [(Var, Var)] -> Maybe [(Var, Var)]
      embedT l r ren = coupleT l r ren `mplus` coupleT l r ren

      coupleT l r renaming =
        case (l,r) of
          -- terms should be embedded up to renaming
          (Var l, Var r) -> if l `elem` fst (unzip renaming)
                            then if (l,r) `elem` renaming then Just renaming else Nothing
                            else Just ((l,r):renaming)

          (Nil, Nil) -> Just renaming
          (Pair l l', Pair r r') -> embedT l r renaming >>= embedT l' r'
          (Atom l, Atom r) | l == r -> Just renaming
          _ -> Nothing

      diveT l r renaming =
        case (l,r) of
        (_, Var _) -> Nothing
        (_, Atom _) -> Nothing
        (_, Nil) -> Nothing
        (x, Pair l r) -> embedT x l renaming `mplus` embedT x r renaming
        _ -> Nothing

      embed' l r renaming = couple l r renaming `mplus` dive l r renaming

      couple l r renaming =
        case (l,r) of
          (Uni  l l', Uni  r r') -> embedT l r renaming >>= embedT l' r'
          (Conj l l', Conj r r') -> embed' l r renaming >>= embed' l' r'
          (Disj l l', Disj r r') -> embed' l r renaming >>= embed' l' r'
          (Call (Fun nl al) als, Call (Fun nr ar) ars) | nl == nr ->
            foldrM (\(l,r) ren -> embedT l r ren) renaming (zip als ars)
          (Zzz l, Zzz r) -> embed' l r renaming
          _ -> Nothing

--      embed' (Uni  l l') (Uni  r r') renaming = embedT l r renaming >>= embedT l' r'
--      embed' (Conj l l') (Conj r r') renaming =
--        let l'' = embed' l r renaming
--            r'' = case l'' of { Just l''' -> embed' l' r' l''' ; Nothing ->
--              trace "conj conj" $
--              trace ("left " ++ show l) $
--              trace ("right " ++ show l') $
--              Nothing }
--        in
----           trace "Top level conj" $
----           trace ("left " ++ show l'') $
----           trace ("right " ++ show r'') $
--           embed' l r renaming >>= embed' l' r'
--      embed' (Disj l l') (Disj r r') renaming = embed' l r renaming >>= embed' l' r'
--      embed' (Call (Fun nl al) als) (Call (Fun nr ar) ars) renaming | nl == nr =
--         foldrM (\(l,r) ren -> embedT l r ren) renaming (zip als ars)
--        --foldr (\(l, r) ren -> embedT l r ren) renaming (zip als ars) --        and (zipWith embedT als ars)
--      embed' (Zzz l) (Zzz r) renaming = embed' l r renaming

      -- diving rules
      dive l r renaming =
        case (l,r) of
          (_, Uni _ _) -> Nothing
          (l, Zzz r) -> embed' l r renaming
          (l, Conj r r') -> embed' l r renaming `mplus` embed' l r' renaming
          _ -> Nothing

--      embed' _ (Uni _ _) _ = Nothing
--      embed' l (Zzz r) renaming = embed' l r renaming
--      embed' l (Conj r r') renaming =
--        let _1 = embed' l r renaming
--            _2 = embed' l r' renaming
--            res = mplus (embed' l r renaming) (embed' l r' renaming)
--        in trace (show _1 ++ " " ++ show _2 ++ "\nres = " ++ show res) $ res -- mplus (embed' l r renaming) (embed' l r' renaming)
--      embed' l (Disj r r') renaming = mplus (embed' l r renaming) (embed' l r' renaming)
--
--      embed' _ _ _ = Nothing

      res = embed' l r []
      isJustRes = isJust res
  in
    trace ("left: " ++ show l) $
    trace ("right: " ++ show r) $
    trace ( "After all. Result: " ++ show res ++ "\nIsJust? " ++ show isJustRes) $ isJustRes
--    isJust $ embed' l r []

unfold _ Nothing = [(Nothing, Nothing)]

unfold x st@(Just st'@(s,c)) =
--  if c >= 6 then [(Nothing, Nothing)] else
  case x of
    GV _ -> [(Nothing, st)]
    Uni  l r -> [(Nothing, unify l r s >>= \s -> Just (s,c))]
    Disj (GV _) r -> [(Just r, st)]
    Disj l (GV _) -> [(Just l, st)]
    Disj l r -> unfold l st ++ unfold r st
    Fresh f  -> [(Just $ f (var c), Just (s,c+1))]
    Zzz a    -> [(Just a,st)]
    Fun _ a  -> [(Just a,st)]
    Call (Fun _ a) arg -> [(Just a,st)]
    Conj (GV _) r -> [(Just r, st)]
    Conj l (GV _) -> [(Just l, st)]
    Conj (Uni l l') (Uni r r') -> [(Nothing, unify l l' s >>= \s -> unify r r' s >>= \s -> Just (s,c))]
    Conj (Uni l l') r -> unfold r (unify l l' s >>= \s -> Just (s,c))
    Conj l (Uni r r') -> unfold l (unify r r' s >>= \s -> Just (s,c))
    Conj l r -> let l' = unfold l st
                in concatMap (\y -> case y of
                                     (Nothing, Nothing) -> [y]
                                     (Nothing, st@(Just _)) -> unfold r st
                                     (Just x', st@(Just _)) -> [(Just $ Conj x' r, st)]
                                     _ -> error "invalid substitution during unfolding")
                             l'

generalize :: AST -> AST -> Int -> (AST, ESubst, ESubst, Int)
generalize smaller bigger n =
  let generalizeT :: Term -> Term -> ESubst -> ESubst -> Int -> (Term, ESubst, ESubst, Int)
      generalizeT l r s1 s2 n =
        case (l,r) of
          (Pair l r, Pair l' r') ->
            let (l'', s1' , s2' , n' ) = generalizeT l l' s1  s2  n
                (r'', s1'', s2'', n'') = generalizeT r r' s1' s2' n'
            in (Pair l'' r'', s1'', s2'', n'')
          (Var v, Var u) | v == u -> (Var v, s1, s2, n)  -- TODO
          (Var _, Var _) ->
            (Var (n), (n, Right r) : s1, (n, Right l) : s2, n+1)
          (Var _, Pair _ _) ->
            (Var (n), (n, Right r) : s1, (n, Right l) : s2, n+1)
          (Atom _, Atom _) ->
            (l, s1, s2, n)
          (Nil, Nil) ->
            (l, s1, s2, n)
          _ -> error $ "Failed to generalize the following terms:\n" ++ show l ++ "\n" ++ show r ++ "\nThis is impossible due to embedding defenition"

      generalize' :: AST -> AST -> ESubst -> ESubst -> Int -> (AST, ESubst, ESubst, Int)
      generalize' smaller bigger s1 s2 n =
        trace ("Attempt to generalize.\nsmaller: " ++ show smaller ++ "\nbigger: " ++ show bigger) $
        case (smaller, bigger) of
          (Disj l r, Disj l' r') ->
            let (l'', s1', s2', n') = generalize' l l' s1 s2 n
                (r'', s1'', s2'', n'') = generalize' r r' s1' s2' n'
            in (Disj l'' r'', s1'', s2'', n'')
          (Conj l r, Conj l' r') ->
            let (l'', s1', s2', n') = generalize' l l' s1 s2 n
                (r'', s1'', s2'', n'') = generalize' r r' s1' s2' n'
            in (Conj l'' r'', s1'', s2'', n'')
          (Zzz s, Zzz b) ->
            let (g, s1', s2', n') = generalize' s b s1 s2 n
            in (Zzz g, s1', s2', n')
          (Call (Fun ns as) args, Call (Fun nb ab) argb) | ns == nb ->
            let (arg', s1', s2', n') = foldr (\(l, r) (prev, s1, s2, n) ->
                                               let (cur, s1', s2', n') = generalizeT l r s1 s2 n
                                               in (cur : prev, s1', s2', n')
                                             )
                                             ([], s1, s2, n)
                                             (zip args argb)
            in (Call (Fun ns as) arg', s1', s2', n')
          (Uni l r, Uni l' r') ->
            let (l'', s1', s2', n') = generalizeT l l' s1 s2 n
                (r'', s1'', s2'', n'') = generalizeT r r' s1' s2' n'
            in (Uni l'' r'', s1'', s2'', n'')
          (s, b) ->
            case find (\(x,t) -> (t == (Left s)) && ((lookup x s2) == (Just $ Left b))) s1 of
              Just (x,t) -> (GV x, s1, s2, n)
              Nothing -> let nv = n+1
                         in
--                            trace ("smaller: " ++ show s ++ "\nbigger: " ++ show b) $
                            (GV nv, (n, Left s) : s1, (n, Left b) : s2, nv)
  in generalize' smaller bigger [] [] n




--  generalise' t u fv s1 s2 = case find (\(x,t') -> t==t' && (lookup x s2 == Just u)) s1 of
--                              Just (x,t') -> (Free x,s1,s2)
--                              Nothing -> let x = rename (fv++fst(unzip s1)) "x"
--                                         in  (Free x,(x,t):s1,(x,u):s2)


drive ast =
  let tree = drive' 0 ast (Just emptyState) [] in tree
  where
    drive' _ _ Nothing _ = Leaf Nothing

    drive' n x st@(Just st'@(s,c)) ancestors =
--      if c >= 20 then Leaf Nothing else
      let parent = applySubst x st'
          ancestor = (parent, n)
      in
        case x of
          Uni  l r ->
            Leaf $ unify l r s >>= \s -> Just (s,c)
          Disj l r ->
            let l' = drive' (n+1) l st (ancestor : ancestors)
                r' = drive' (n+1) r st (ancestor : ancestors)
            in node n st' parent [l', r']
          Fresh f ->
            let a = drive' (n+1) (f $ var c) (Just (s,c+1)) (ancestors)
            in node n st' parent [a]
          Zzz a ->
            drive' n a st ancestors
          Fun _ a ->
            let a'= drive' (n+1) a st (ancestor : ancestors)
            in node n st' parent [a']
          Call (Fun name a) arg ->
            trace ("Calling function " ++ name ++ " with body " ++ show a ++ " and arguments " ++ show arg ) $
            let a' = drive' (n+1) a st (ancestor : ancestors)
                child =
--                        trace ("NODES " ++ show ancestors) $
                        case a' of
                          Node _ s'@(s,c) ast _ ->
--                            trace ("!!!!" ++ show ast) $
                            case find (\(x,n) -> renaming ast x) ancestors of -- let e = embed ast x in trace (show e) e) ancestors of
                              Just (_,y) -> Up y s' ast -- []
                              Nothing ->
                                case find (\(x,n) -> isCoupling x ast && embed x ast) ancestors of
                                  Just (x',y) ->
                                    let (x'', s1, _, varcount) = generalize x' ast c
                                    in G y s' s1 x'' [drive' (n+1) x'' (Just (s,varcount)) (ancestor:ancestors)]
                                  _ -> a'
                          _ -> a'
            in node n st' parent [child]
          Call _ _ ->
            Leaf Nothing
          Conj (Uni l l') (Uni r r') ->
            let st'' = unify l l' s >>= \s ->
                       unify r r' s >>= \s ->
                       Just (s,c)
            in node n st' parent [Leaf st'']
          Conj (Uni l l') r ->
            let r' = drive' (n+1) r (unify l l' s >>= \s -> Just (s,c)) (ancestor : ancestors)
            in node n st' parent [r']
          Conj l (Uni r r') ->
            let l' = drive' (n+1) l (unify r r' s >>= \s -> Just (s,c)) (ancestor : ancestors)
            in node n st' parent [l']
--          Conj (Disj l l') r ->
--            let ch = drive' (n+1) (Disj (Conj l r) (Conj l' r)) st (ancestor : ancestors)
--            in node n st' parent [ch]
--          Conj l (Disj r r') ->
--            let ch = drive' (n+1) (Disj (Conj l r) (Conj l r')) st (ancestor : ancestors)
--            in node n st' parent [ch]
          Conj l r  -> -- TODO debug this branch
--
            let unfolded = unfold x st
                children = map (\y ->
                                 case y of
                                   (Nothing, Nothing) -> Leaf Nothing
                                   (Nothing, st) -> Leaf st
                                   (Just ch, Just st) ->
                                     let ch' = applySubst ch st
                                     in
                                       trace ("Ancestors " ++ show ancestors) $
                                       case find (\(x,n) -> renaming ch' x) ancestors of -- let e = embed ast x in trace (show e) e) ancestors of
                                        Just (_,y) -> Up y st ch' -- []
                                        Nothing ->
                                          case find (\(x,n') -> isCoupling x ch' && embed x ch') ancestors of
                                            Just (x',y) ->
                                              let (x'', s1, _, varcount) = generalize x' ch' c
                                              in trace ("generalization " ++ show varcount) $
                                                 G y st s1 x'' [drive' (n+1) x'' (Just (s,varcount)) (ancestor:ancestors)]
                                            _ -> node
                                                   (n+1)
                                                   st
                                                   ch'
                                                   [drive' (n+2) ch' (Just st) ((ch', n+1) : ancestors)]
                               )
                               unfolded
            in node n st' parent children

--            let l' = drive' (n+1) l st (ancestor : ancestors)
--            in trace (show l') $ case l' of
--                 Leaf Nothing ->
--                   Leaf Nothing
--                 Leaf st@(Just st') ->
--                   let r' = drive' (n+1) r st (ancestor : ancestors)
--                   in node n st' parent [r']
--                 Node _ st x ch ->
----                   let r' = drive' (n+1) (Conj x r) (Just st) ancestors
----                   in
--                   node (n+1) st (applySubst (Conj x r) st) ch
