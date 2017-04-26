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


apply_subst (Conj l r)    s        = Conj (apply_subst l s) $ apply_subst r s
apply_subst (Disj l r)    s        = Disj (apply_subst l s) $ apply_subst r s
apply_subst (Fresh f)     (s,c)    = apply_subst (f $ var c) (s,c+1)
apply_subst (Fun n a)     s        = Fun n $ apply_subst a s
apply_subst (Zzz a)       s        = Zzz $ apply_subst a s
apply_subst (Call a args) s'@(s,c) = Call (apply_subst a s') $ map (\x -> walk' x s) args
apply_subst (Uni l r)     (s,c)    = Uni (walk' l s) $ walk' r s

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
