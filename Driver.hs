module Driver where
import MuKanren
import Debug.Trace

data Tree subst ast = Leaf (Maybe subst) | Node Int subst ast [Tree subst ast]

instance (Show subst, Show ast) => Show (Tree subst ast) where 
  show t = show' t 0 
   where 
    nspaces n = [' ' | _ <- [1..n]]
    show' (Leaf s) n = nspaces n ++ "L " ++ (show s) ++ "\n"
    show' (Node d s a ns) n = nspaces n ++ "N " ++ show d ++ " " ++ show s ++ " " ++ show a ++ "\n" ++ (concat $ map (\x -> show' x (n + 1)) ns)

apply_subst (Conj l r)    s        = Conj (apply_subst l s) (apply_subst r s)
apply_subst (Disj l r)    s        = Disj (apply_subst l s) (apply_subst r s)
apply_subst (Fresh f)     (s,c)    = apply_subst (f $ var c) (s,c+1)
apply_subst (Fun n a)     s        = Fun n $ apply_subst a s
apply_subst (Zzz a)       s        = Zzz $ apply_subst a s
apply_subst (Call a args) s'@(s,c) = Call (apply_subst a s') (map (\x -> walk' x s) args)
apply_subst (Uni l r)     (s,c)    = Uni (walk' l s) (walk' r s)

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
          (arg', s'') = foldl (\(xs, s) x -> let (x', s') = rename' x s in (x' : xs, s')) ([], s') args
      in (Call a' $ reverse arg', s'')
    rename_ast (Uni l r ) s = 
      let (l', s')  = rename' l s 
          (r', s'') = rename' r s'
      in (Uni l' r', s'')

    rename' (Var v) s@(m, c) = 
      case lookup v m of 
        Nothing -> let v' = Var c in (v', ((v, v') : m, c+1))
        Just v' -> (v', s)
    rename' (Pair l r) s = 
      let (l', s')  = rename' l s
          (r', s'') = rename' r s'
      in (Pair l' r', s'')
    rename' x s = (x, s)

eq l r =
  eq' l' r' 
  where 
    l' = rename l
    r' = rename r
    eq' (Conj l l') (Conj r r') = eq' l r && eq' l' r'
    eq' (Disj l l') (Disj r r') = eq' l r && eq' l' r'
    eq' (Uni  l l') (Uni  r r') = l == r && l' == r'
    -- The following one should not be ever used, TODO consider deleting
    -- eq' (Fun ln l)  (Fun rn r)  = ln == rn && eq' l r
    eq' (Zzz l)     (Zzz r)     = eq' l r
    eq' (Call (Fun l _) ls) (Call (Fun r _) rs) = l == r && foldl (\acc (l, r) -> acc && l == r) True (zip ls rs)
    eq' _ _ = False 

-- vars can only be the same
embed_t (Var l) (Var r) = l == r

-- coupling rules
embed_t (Nil)       (Nil)       = True
embed_t (Pair l l') (Pair r r') = embed_t l r && embed_t r r'
embed_t (Atom l)    (Atom r)    = l == r

-- diving rules
embed_t _ (Var _)    = False
embed_t _ (Atom _)   = False
embed_t _  Nil       = False 
embed_t x (Pair l r) = embed_t x l || embed_t x r

-- coupling rules 
embed (Uni l l') (Uni r r') = embed_t l r && embed_t l' r'
embed (Conj l l') (Conj r r') = embed l r && embed l' r'
embed (Disj l l') (Disj r r') = embed l r && embed l' r'
embed (Call (Fun nl al) als) (Call (Fun nr ar) ars) = nl == nr && and (zipWith embed_t als ars)
embed (Zzz l) (Zzz r) = embed l r

-- diving rules 
embed _ (Uni _ _) = False
embed l (Zzz r) = embed l r
embed l (Conj r r') = embed l r || embed l r'
embed l (Disj r r') = embed l r || embed l r'

drive ast = 
  let (_, tree) = drive' 0 ast (Just empty_state) in tree
  where 
    drive' n _ Nothing = (n, Leaf Nothing)

    drive' n x st@(Just st'@(s,c)) = 
      case x of 
        Uni  l r -> 
          (n, Leaf $ unify l r s >>= \s -> Just (s,c))
        Disj l r -> 
          let (n' , l') = drive' (n +1) l st
              (n'', r') = drive' (n'+1) r st
          in (n'', Node n st' (apply_subst x st') [l', r'])
        Fresh f ->
          let (n', a) = drive' (n+1) (f $ var c) (Just (s,c+1))
          in (n', Node n st' (apply_subst x st') [a])
        Zzz a -> 
          drive' n a st
        Fun _ a -> 
          let (n', a') = drive' (n+1) a st 
          in (n', Node n st' (apply_subst x st') [a'])
        Call (Fun _ a) arg | c <= 10 ->
          let (n', a') = drive' (n+1) a st
          in (n', Node n st' (apply_subst x st') [a'])
        Call _ _ -> 
          (n, Leaf Nothing)
        Conj (Uni l l') (Uni r r') -> 
          let st'' = unify l l' s >>= \s -> 
                     unify r r' s >>= \s -> 
                     Just (s,c)
          in (n, Node n st' (apply_subst x st') [Leaf st''])
        Conj (Uni l l') r -> 
          let (n', r') = drive' (n+1) r (unify l l' s >>= \s -> Just (s,c))
          in (n', Node n st' (apply_subst x st') [r'])
        Conj l (Uni r r') ->
          let (n', l') = drive' (n+1) l (unify r r' s >>= \s -> Just (s,c))
          in (n', Node n st' (apply_subst x st') [l'])
        Conj l r -> 
          let (n', l') = drive' (n+1) l st
          in case l' of 
               Leaf Nothing -> 
                 (n, Leaf Nothing)
               Leaf st@(Just st') ->
                 let (n'', r') = drive' (n'+1) r st
                 in (n'', Node n st' (apply_subst x st') [r'])
               Node _ st x ch -> 
                 (n', Node (n'+1) st (apply_subst (Conj x r) st) ch)
