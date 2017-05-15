module Driver where
import MuKanren hiding (mplus)
import Control.Monad
import Debug.Trace
import Data.List (find, delete)
import Data.Maybe (isJust)
import Data.Foldable (foldrM)



-- data Tree subst ast = Leaf (Maybe subst) | Node Int subst ast [Tree subst ast] | Up Int subst ast | G Int subst ESubst ast [Tree subst ast]

type GenRef = Int

data Tree' subst ast = Success (subst,Int)
                     | Fail
                     | Step Int subst ast (Tree' subst ast)
                     | Or Int subst ast (Tree' subst ast) (Tree' subst ast)
                     | Up Int subst ast
                     | Gen Int subst ast ESubst GenRef (Tree' subst ast)

data Ctx = EmptyCtx | ConjCtx AST Ctx

--node = Node

--instance (Show subst, Show ast) => Show (Tree subst ast) where
--  show t = show' t 0
--   where
--    nspaces n = [' ' | _ <- [1..n]]
--    show' (Leaf s) n = nspaces n ++ "L " ++ show s ++ "\n"
--    show' (Node d s a ns) n = nspaces n ++ "N " ++ show d ++ {- " " ++ show s ++ -} " " ++ show a ++ "\n" ++
--                              concatMap (\x -> show' x (n + 1)) ns
--    show' (Up d s a) n = nspaces n ++ "U " ++ show d ++ " " ++ show s ++ " " ++ " " ++ show a ++  "\n"
--    show' (G d s es a ch) n = nspaces n ++ "G " ++ show d ++ " " ++ show s ++ " " ++ show es ++ " " ++ show a ++ "\n" ++ concatMap (\x -> show' x (n+1)) ch

instance (Show subst, Show ast) => Show (Tree' subst ast) where
  show t = show' t 0 where
    nspaces n = [' ' | _ <- [1..n]]
    show' Fail n' = nspaces n' ++ "F\n"
    show' (Success (s,c)) n' = nspaces n' ++ "S " ++ show s ++ "\n"
    show' (Step n s a ch) n' = nspaces n' ++ "T " ++ show n ++ " " ++ show s ++ " " ++ show a ++ "\n" ++ show' ch (n+1)
    show' (Or n s a l r) n' = nspaces n' ++ "O " ++ show n ++ " " ++ show s ++ " " ++ show a ++ "\n" ++ show' l (n+1) ++ "\n" ++ show' r (n+1)
    show' (Up n s a) n' = nspaces n' ++ "U " ++ show n ++ " " ++ show s ++ " " ++ show a ++ "\n"
    show' (Gen n s a es gr ch) n' = nspaces n' ++ "G " ++ show gr ++ " " ++ show a ++ " " ++ show es ++ "\n" ++ show' ch (n+1)

applySubst (Conj l r)    s        = Conj (applySubst l s) (applySubst r s)
applySubst (Disj l r)    s        = Disj (applySubst l s) (applySubst r s)
applySubst (Fresh f)     (s,c)    = applySubst (f $ var c) (s,c+1)
applySubst (Fun n a)     s        = Fun n $ applySubst a s
applySubst (Zzz a)       s        = Zzz $ applySubst a s
applySubst (Call a args) s'@(s,c) = Call (applySubst a s') (map (\x -> walk' x s) args)
applySubst (Uni l r)     (s,c)    = Uni (walk' l s) (walk' r s)
applySubst g@(GV _ _ _)   _       = g

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

embed l r =
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

      dive l r renaming =
        case (l,r) of
          (_, Uni _ _) -> Nothing
          (l, Zzz r) -> embed' l r renaming
          (l, Conj r r') -> embed' l r renaming `mplus` embed' l r' renaming
          _ -> Nothing
  in
    isJust $ embed' l r []

unfold _ Nothing = [(Nothing, Nothing)]

unfold x st@(Just st'@(s,c)) =
--  if c >= 6 then [(Nothing, Nothing)] else
  case x of
    GV _ _ _ -> [(Nothing, st)]
    Uni  l r -> [(Nothing, unify l r s >>= \s -> Just (s,c))]
    Disj (GV _ _ _) r -> [(Just r, st)]
    Disj l (GV _ _ _) -> [(Just l, st)]
    Disj l r -> unfold l st ++ unfold r st
    Fresh f  -> [(Just $ f (var c), Just (s,c+1))]
    Zzz a    -> [(Just a,st)]
    Fun _ a  -> [(Just a,st)]
    Call (Fun _ a) arg -> [(Just a,st)]
    Conj (GV _ _ _) r -> [(Just r, st)]
    Conj l (GV _ _ _) -> [(Just l, st)]
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

generalize :: AST -> AST -> Int -> Int -> (AST, ESubst, ESubst, Int)
generalize smaller bigger n up =
  let generalizeT :: Term -> Term -> ESubst -> ESubst -> Int -> (Term, ESubst, ESubst, Int)
      generalizeT l r s1 s2 n =
        case (l,r) of
          (Pair l r, Pair l' r') ->
            let (l'', s1' , s2' , n' ) = generalizeT l l' s1  s2  n
                (r'', s1'', s2'', n'') = generalizeT r r' s1' s2' n'
            in (Pair l'' r'', s1'', s2'', n'')
          (Var v, Var u) | v == u -> (Var v, s1, s2, n)  -- TODO
          (Var _, Var _) ->
            (Var n, (n, Right r) : s1, (n, Right l) : s2, n+1)
          (Var _, Pair _ _) ->
            (Var n, (n, Right r) : s1, (n, Right l) : s2, n+1)
          (Atom _, Atom _) ->
            (l, s1, s2, n)
          (Nil, Nil) ->
            (l, s1, s2, n)
          _ -> error $ "Failed to generalize the following terms:\n" ++ show l ++ "\n" ++ show r ++ "\nThis is impossible due to embedding defenition"

      generalize' :: AST -> AST -> ESubst -> ESubst -> Int -> Int -> (AST, ESubst, ESubst, Int)
      generalize' smaller bigger s1 s2 n up =
--        trace ("Attempt to generalize.\nsmaller: " ++ show smaller ++ "\nbigger: " ++ show bigger) $
        case (smaller, bigger) of
          (Disj l r, Disj l' r') ->
            let (l'', s1', s2', n') = generalize' l l' s1 s2 n up
                (r'', s1'', s2'', n'') = generalize' r r' s1' s2' n' up
            in (Disj l'' r'', s1'', s2'', n'')
          (Conj l r, Conj l' r') ->
            let (l'', s1', s2', n') = generalize' l l' s1 s2 n up
                (r'', s1'', s2'', n'') = generalize' r r' s1' s2' n' up
            in (Conj l'' r'', s1'', s2'', n'')
          (Zzz s, Zzz b) ->
            let (g, s1', s2', n') = generalize' s b s1 s2 n up
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
              Just (x,t) -> (GV x s1 up, s1, s2, n)
              Nothing -> let nv = n+1
                         in
--                            trace ("smaller: " ++ show s ++ "\nbigger: " ++ show b) $
                            (GV nv ((nv, Left b) : s1) up, (nv, Left s) : s1, (n, Left s) : s2, nv)
  in generalize' smaller bigger [] [] n up

flatten t EmptyCtx = t
flatten t (ConjCtx t' ctx) = flatten (Conj t t') ctx

unify' l r st@(s,c) =
  unify l r s >>= \s -> Just (s,c)

drive ast =
  drive' 0 ast EmptyCtx emptyState []
  where
    drive' _ (Uni l r) EmptyCtx st _ =
      case unify' l r st of
        Just st' -> Success st'
        Nothing -> Fail

    drive' n t@(Uni l r) c@(ConjCtx a ctx) st ancs =
      case unify' l r st of
        Just st'@(s',c') ->
          let anc = flatten t c
          in Step n s' anc (drive' (n+1) a ctx st' ((anc,n):ancs))
        Nothing -> Fail

    drive' n t@(Zzz a) ctx st@(s,c) ancs =
      let anc = flatten t ctx
      in Step n s anc (drive' (n+1) a ctx st ((anc,n):ancs))

    drive' n t@(Disj l r) ctx st@(s,c) ancs =
      let anc = flatten t ctx
      in Or n s anc (drive' (n+1) l ctx st ((anc,n):ancs)) (drive' (n+1) r ctx st ((anc,n):ancs))

    drive' n t@(Conj l r) ctx st@(s,c) ancs =
      let anc = flatten t ctx
      in Step n s anc (drive' (n+1) l (ConjCtx r ctx) st (ancs))

    drive' n t@(Fresh f) ctx st@(s,c) ancs =
      let anc = flatten t ctx
      in Step n s anc (drive' (n+1) (f $ var c) ctx (s,c+1) (ancs))

    drive' _ (Fun _ _) _ _ _ = error "unapplied function"

    drive' n t@(Call (Fun _ a'''') args) ctx st@(s,c) ancs =
--      if n >= 20 then Fail else
      let anc = flatten t ctx
          ch =
--               trace ("ancestors " ++ show ancs ) $
--               trace ("\nancestor: " ++ show anc ++ "\nterm: " ++ show t) $
               case find (\(a,n') -> renaming a anc) ancs of
                 Just (a,n') -> Up n' s anc
                 Nothing ->
                   case find (\(a,n') ->
                               let isC = isCoupling a anc
                                   emb = embed a anc
                               in
--                                 trace ("checking " ++ show a ++ " AND " ++ show anc ++
--                                         ":\nisCoupling: " ++ show isC ++ "\nisEmbedding: " ++ show emb ++ "\n") $
                                 isC && emb) ancs of
                     Just (a,n') ->
                       let (g, s1, s2, c') = generalize a anc c n'
                       in drive' (n+1) g EmptyCtx st ((anc,n):ancs)
--                       in case ctx of
--                            EmptyCtx -> Up n' s anc
--                            ConjCtx x ctx ->
--                              Gen (n+1) s g s1 n' (drive' (n+2) x ctx st ((anc,n):ancs)) -- TODO check ancestors
                     Nothing -> drive' (n+1) a'''' ctx st ((anc,n):ancs)
      in Step n s anc ch

    drive' n t@(GV v es r) EmptyCtx st@(s,c) ancs =
      Up r s t

    drive' n t@(GV v es r) c@(ConjCtx a ctx) st@(s,_) ancs =
     let anc = flatten t c
     in Gen n s anc es r (drive' (n+1) a ctx st ((anc,n):ancs))

--drive ast =
--  let tree = drive' 0 ast (Just emptyState) [] in tree
--  where
--    drive' _ _ Nothing _ = Leaf Nothing
--
--    drive' n x st@(Just st'@(s,c)) ancestors =
--      let parent = applySubst x st'
--          ancestor = (parent, n)
--      in
--        case x of
--          Uni  l r ->
--            Leaf $ unify l r s >>= \s -> Just (s,c)
--          Disj l r ->
--            let l' = drive' (n+1) l st (ancestor : ancestors)
--                r' = drive' (n+1) r st (ancestor : ancestors)
--            in node n st' parent [l', r']
--          Fresh f ->
--            let a = drive' (n+1) (f $ var c) (Just (s,c+1)) ancestors
--            in node n st' parent [a]
--          Zzz a ->
--            drive' n a st ancestors
--          Fun _ a ->
--            let a'= drive' (n+1) a st (ancestor : ancestors)
--            in node n st' parent [a']
--          Call (Fun name a) arg ->
----            trace ("Calling function " ++ name ++ " with body " ++ show a ++ " and arguments " ++ show arg ) $
--            let a' = drive' (n+1) a st (ancestor : ancestors)
--                child =
----                        trace ("NODES " ++ show ancestors) $
--                        case a' of
--                          Node _ s'@(s,c) ast _ ->
----                            trace ("!!!!" ++ show ast) $
--                            case find (\(x,n) -> renaming ast x) ancestors of -- let e = embed ast x in trace (show e) e) ancestors of
--                              Just (_,y) -> Up y s' ast -- []
--                              Nothing ->
--                                case find (\(x,n) -> isCoupling x ast && embed x ast) ancestors of
--                                  Just (x',y) ->
--                                    let (x'', s1, _, varcount) = generalize x' ast c
--                                    in G y s' s1 x'' [drive' (n+1) x'' (Just (s,varcount)) (ancestor:ancestors)]
--                                  _ -> a'
--                          _ -> a'
--            in node n st' parent [child]
--          Call _ _ ->
--            Leaf Nothing
--          Conj (Uni l l') (Uni r r') ->
--            let st'' = unify l l' s >>= \s ->
--                       unify r r' s >>= \s ->
--                       Just (s,c)
--            in node n st' parent [Leaf st'']
--          Conj (Uni l l') r ->
--            let r' = drive' (n+1) r (unify l l' s >>= \s -> Just (s,c)) (ancestor : ancestors)
--            in node n st' parent [r']
--          Conj l (Uni r r') ->
--            let l' = drive' (n+1) l (unify r r' s >>= \s -> Just (s,c)) (ancestor : ancestors)
--            in node n st' parent [l']
--          Conj (Disj l l') r ->
--            let ch = drive' (n+1) (Disj (Conj l r) (Conj l' r)) st (ancestor : ancestors)
--            in node n st' parent [ch]
--          Conj l (Disj r r') ->
--            let ch = drive' (n+1) (Disj (Conj l r) (Conj l r')) st (ancestor : ancestors)
--            in node n st' parent [ch]
--          Conj l r  -> -- TODO debug this branch
--            if n >= 20 then Leaf Nothing else
--            let unfolded = unfold x st
--                children = map (\y ->
--                                 case y of
--                                   (Nothing, Nothing) -> Leaf Nothing
--                                   (Nothing, st) -> Leaf st
--                                   (Just ch, Just st) ->
--                                     let ch' = applySubst ch st
--                                     in
----                                       trace ("Ancestors " ++ show ancestors) $
--                                       case find (\(x,n) -> renaming ch' x) ancestors of -- let e = embed ast x in trace (show e) e) ancestors of
--                                        Just (_,y) -> Up y st ch' -- []
--                                        Nothing ->
--                                          case find (\(x,n') -> isCoupling x ch' && embed x ch') ancestors of
--                                            Just (x',y) ->
--                                              let (x'', s1, _, varcount) = generalize x' ch' c
--                                              in --trace ("generalization " ++ show varcount) $
--                                                 G y st s1 x'' [drive' (n+1) x'' (Just (s,varcount)) (ancestor:ancestors)]
--                                            _ -> node
--                                                   (n+1)
--                                                   st
--                                                   ch'
--                                                   [drive' (n+2) ch' (Just st) ((ch', n+1) : ancestors)]
--                               )
--                               unfolded
--            in node n st' parent children
--
--residualise t =
--  case t of
--    Leaf Nothing -> Nothing
--    Leaf (Just (subst,c)) -> Just $ conj (map (\(v, u) -> Uni (Var v) u) subst)
--    _ -> error "todo"

























