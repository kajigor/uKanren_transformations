module FirstOrder.Eval where

import Syntax (Term (..), G (..), S, unsafeDisj, unsafeConj)
import Subst (Subst (..))
import Eval (unify)

type Thunk a = G a

data Description = Descr String

data Stream a = Empty
              | Cons a (Stream a)
              | Bind (Stream a) (G a)
              | Mplus (Stream a) (Stream a)
              | Pause a (G a)

isMature :: Stream a -> Bool
isMature Empty = True
isMature (Cons _ _) = True
isMature _ = False

mature :: Stream Subst -> Stream Subst
mature s | isMature s = s
mature s = mature (step s)

step :: Stream Subst -> Stream Subst
step (Mplus s1 s2) =
  let s1' = if isMature s1 then s1 else step s1 in
  case s1' of
    Empty -> s2
    Cons h t ->
      Cons h (Mplus s2 t)
    _ -> Mplus s2 s1'
step (Bind s g) =
  let s' = if isMature s then s else step s in
  case s' of
    Empty -> Empty
    Cons h t ->
      step (Mplus (Pause h g) (Bind t g))
    _ -> Bind s' g
step (Pause st g) =
  start st g
step s = s

start st (Disjunction g1 g2 xs) =
  step (Mplus (Pause st g1) (Pause st (unsafeDisj $ g2 : xs)))
start st (Conjunction g1 g2 xs) =
  step (Bind (Pause st g1) (unsafeConj $ g2 : xs))
start st (Invoke thunk _) =
  Pause st thunk
start st (t1 :=: t2) =
  case unify (Just st) t1 t2 of
    Nothing -> Empty
    Just s -> Cons s Empty


