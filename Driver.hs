module Driver where
import MuKanren
import Debug.Trace

data Tree subst ast = Leaf (Maybe subst)   | Node subst ast [Tree subst ast]

instance (Show subst, Show ast) => Show (Tree subst ast) where 
  show t = show' t 0 
   where 
    nspaces n = [' ' | _ <- [1..n]]
    show' (Leaf s) n = nspaces n ++ "L " ++ (show s) ++ "\n"
    show' (Node s a ns) n = nspaces n ++ "N " ++ show s ++ " " ++ show a ++ "\n" ++ (concat $ map (\x -> show' x (n + 1)) ns)

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
          a' = a -- (a', s')    = rename_ast a s -- this loops. Doesn't seem necessary for now anyways
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
    eq' (Fun ln l)  (Fun rn r)  = ln == rn && eq' l r
    eq' (Zzz l)     (Zzz r)     = eq' l r
    eq' (Call (Fun l _) ls) (Call (Fun r _) rs) = l == r && foldl (\acc (l, r) -> acc && l == r) True (zip ls rs)
    eq' _ _ = False 

drive _ Nothing = -- trace "nothing" $
                  Leaf Nothing

drive x@(Uni t t')   (Just (s,c))    = 
--trace "uni " $
--trace (show s) $
  Leaf $ (unify t t' s >>= \s -> Just (s,c))

drive x@(Disj g g')  st@(Just st')   = 
--trace "disj " $
--trace (show st') $
  Node st' (apply_subst x st') [drive g st, drive g' st]


drive x@(Fun _ a)    st@(Just st')   =
--trace "fun" $
--trace (show st') $
  Node st' (apply_subst x st') [drive a st]

drive x@(Fresh f)    (Just st@(s,c)) = 
--trace "fresh" $
--trace (show s) $ 
  Node st (apply_subst x st) [drive (f $ var c) (Just (s, c+1))]

drive x@(Zzz a)      st@(Just st')   = Node st' x [drive a st]

-- TODO Meaningfull guard should be here (check for embedding or smth like that)
drive x@(Call f arg) st@(Just st'@(s,c)) | c <= 10   = 
--trace "call" $
--trace (show s) $ 
  Node st' (apply_subst x st') [drive f st]

drive x@(Call f arg) st@(Just st')   = 
--trace "call nothing" $
  Leaf Nothing

drive x@(Conj (Uni l l') (Uni r r')) (Just st@(s,c)) = 
--trace "conj uni uni" $
--trace (show x) $
--trace (show s) $
  Node st (apply_subst x st) [Leaf st'] 
  where 
    st' = unify l l' s >>= \s -> 
            --trace (show s) $ 
            unify r r' s >>= \s -> Just (s,c)

drive x@(Conj (Uni l l') r) (Just st@(s,c)) = 
--trace "conj uni _" $
--trace (show x) $
--trace (show s) $
  let t = apply_subst x st in trace ("\n" ++ show (rename t) ++ "\n") $ 

    Node st (apply_subst x st) [drive r (unify l l' s >>= \s -> Just (s, c))]

drive x@(Conj l (Uni r r')) (Just st@(s,c)) = 
--trace "conj _ uni" $
--trace (show x) $
--trace (show s) $
  Node st (apply_subst x st) [drive l (unify r r' s >>= \s -> Just (s, c))]

drive x@(Conj l r) st@(Just st'@(s,c)) = 
  Node st' (apply_subst x st') $ [
    case drive l st of 
      Leaf Nothing -> Leaf Nothing
      Leaf st@(Just st') -> Node st' (apply_subst x st') [drive r st]
      Node st x ch -> Node st (apply_subst (Conj x r) st) ch
  ]
