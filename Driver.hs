module Driver where
import MuKanren
import Debug.Trace
import Data.List (find)

data Edge a = Up Int | Dn a

data Tree subst ast = Leaf (Maybe subst) | Node Int subst ast [Edge (Tree subst ast)]

node = Node 

instance (Show subst, Show ast) => Show (Tree subst ast) where 
  show t = show' t 0 
   where 
    nspaces n = [' ' | _ <- [1..n]]
    show' (Leaf s) n = nspaces n ++ "L " ++ (show s) ++ "\n"
    show' (Node d s a ns) n = nspaces n ++ "N " ++ show d ++ " " ++ show s ++ " " ++ show a ++ "\n" ++ 
                             (concat $ map (\x -> case x of Dn x -> show' x (n + 1)
                                                            Up d -> "Up " ++ show d) 
                                           ns)

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
embed_t (Pair l l') (Pair r r') = embed_t l r && embed_t l' r'
embed_t (Atom l)    (Atom r)    = l == r

-- diving rules
embed_t _ (Var _)    = False
embed_t _ (Atom _)   = False
embed_t _  Nil       = False 
embed_t x (Pair l r) = embed_t x l || embed_t x r

embed l r = 
  let l' = rename l
      r' = rename r
  in 
    trace ("EMBED l' = " ++ show l' ++ " AND r' = " ++ show r') $
    embed' l' r' where
      -- coupling rules 
      embed' (Uni  l l') (Uni  r r') = embed_t l r && embed_t l' r'
      embed' (Conj l l') (Conj r r') = embed' l r && embed' l' r'
      embed' (Disj l l') (Disj r r') = embed' l r && embed' l' r'
      embed' (Call (Fun nl al) als) (Call (Fun nr ar) ars) = nl == nr && and (zipWith embed_t als ars) 
      embed' (Zzz l) (Zzz r) = embed' l r

      -- diving rules 
      embed' _ (Uni _ _) = False
      embed' l (Zzz r) = embed' l r
      embed' l (Conj r r') = embed' l r || embed' l r'
      embed' l (Disj r r') = embed' l r || embed' l r'

      embed' _ _ = trace "fell through " $  False

drive ast = 
  let tree = drive' 0 ast (Just empty_state) [] in tree
  where 
    drive' _ _ Nothing _ = Leaf Nothing

    drive' n x st@(Just st'@(s,c)) ancestors = 
      let parent = apply_subst x st'
          ancestor = (parent, n)
      in
        case x of 
          Uni  l r -> Leaf $ unify l r s >>= \s -> Just (s,c)
          Disj l r -> 
            let l' = drive' (n+1) l st (ancestor : ancestors)
                r' = drive' (n+1) r st (ancestor : ancestors)
            in node n st' parent [Dn l', Dn r']
          Fresh f ->
            let a = drive' (n+1) (f $ var c) (Just (s,c+1)) (ancestor : ancestors)
            in node n st' parent [Dn a]
          Zzz a -> 
            drive' n a st ancestors
          Fun _ a -> 
            let a'= drive' (n+1) a st (ancestor : ancestors)
            in node n st' parent [Dn a']
          Call (Fun _ a) arg ->
            let a' = drive' (n+1) a st (ancestor : ancestors)
                child = 
                        trace ("NODES " ++ show ancestors) $
                        case a' of 
                          Node _ _ ast _ -> 
                            case find (\(x,n)  -> {- trace ("embed? " ++ show ast ++ " AND " ++ show x) $ -} let e = embed ast x in trace (show e) $ e) ancestors of 
                              Just (_,y) -> Up y
                              Nothing -> Dn a'
                          _ -> Dn a'
            in node n st' parent [child]
          Call _ _ -> 
            Leaf Nothing
          Conj (Uni l l') (Uni r r') -> 
            let st'' = unify l l' s >>= \s -> 
                       unify r r' s >>= \s -> 
                       Just (s,c)
            in node n st' parent [Dn $ Leaf st'']
          Conj (Uni l l') r -> 
            let r' = drive' (n+1) r (unify l l' s >>= \s -> Just (s,c)) (ancestor : ancestors)
            in node n st' parent [Dn r']
          Conj l (Uni r r') ->
            let l' = drive' (n+1) l (unify r r' s >>= \s -> Just (s,c)) (ancestor : ancestors)
            in node n st' parent [Dn l']
          Conj l r -> -- TODO debug this branch
            let l' = drive' (n+1) l st (ancestor : ancestors)
            in case l' of 
                 Leaf Nothing -> 
                   Leaf Nothing
                 Leaf st@(Just st') ->
                   let r' = drive' (n+1) r st (ancestor : ancestors)
                   in node n st' parent [Dn r']
                 Node _ st x ch -> 
                   node (n+1) st (apply_subst (Conj x r) st) ch
                   
                   
                   
                   
                   
                   
                   
                   
