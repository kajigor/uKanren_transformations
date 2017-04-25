module Driver where
import MuKanren
import Debug.Trace

data Tree subst ast = Leaf (Maybe subst)   | Node subst ast [Tree subst ast] deriving Show

drive _ Nothing = trace "nothing" $ Leaf Nothing

drive x@(Uni t t')   (Just (s,c))    = 
  trace "uni " $
  trace (show s) $
  Leaf $ (unify t t' s >>= \s -> Just (s,c))

drive x@(Disj g g')  st@(Just st')   = 
  trace "disj " $
  trace (show st') $
  Node st' x [drive g st, drive g' st]


drive x@(Fun _ a)    st@(Just st')   =
  trace "fun" $
  trace (show st') $
  Node st' x [drive a st]

drive x@(Fresh f)    (Just st@(s,c)) = 
  trace "fresh" $
  trace (show s) $ 
  Node st x [drive (f $ var c) (Just (s, c+1))]

drive x@(Zzz a)      st@(Just st')   = Node st' x [drive a st]

-- TODO Meaningfull guard should be here (check for embedding or smth like that)
drive x@(Call f arg) st@(Just st'@(s,c)) | c <= 20   = 
  trace "call" $
  trace (show s) $ 
  Node st' x [drive f st]

drive x@(Call f arg) st@(Just st')   = 
  trace "call nothing" $
  Leaf Nothing

drive x@(Conj (Uni l l') (Uni r r')) (Just st@(s,c)) = 
  trace "conj uni uni" $
  trace (show x) $
  trace (show s) $
  Node st x [Leaf st'] 
  where 
    st' = unify l l' s >>= \s -> trace (show s) $ unify r r' s >>= \s -> Just (s,c)

drive x@(Conj (Uni l l') r) (Just st@(s,c)) = 
  trace "conj uni _" $
  trace (show x) $
  trace (show s) $
  Node st x [drive r (unify l l' s >>= \s -> Just (s, c))]

drive x@(Conj l (Uni r r')) (Just st@(s,c)) = 
  trace "conj _ uni" $
  trace (show x) $
  trace (show s) $
  Node st x [drive l (unify r r' s >>= \s -> Just (s, c))]

drive x@(Conj l r) st@(Just st'@(s,c)) = 
-- Кажется, я запуталась с тем, на каком уровне создаются узлы. 
-- Нужно ли сохранять текущий терм, а его анфолдинг делать в детей или сразу анфолдить?
  case drive l st of 
    Leaf Nothing -> Leaf Nothing
    Leaf st@(Just st') -> Node st' x [drive r st]
    Node st x ch -> Node st (Conj x r) ch

