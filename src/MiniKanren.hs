{-
   Deeply embedded minikanren
-}

module MiniKanren where
import Control.Monad (foldM)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data
import DataShow
import State
import Stream

walk :: State -> Term -> Term
walk state (Free i) =
  case lookup i (getSubst state) of
    Nothing -> Free i
    Just t -> walk state t
walk state (Var v) = error $ "syntactic variable " ++ v ++ " in walk\nstate: " ++ show state
walk _ x = x

unify :: State -> Term -> Term -> Maybe State
unify state u' v' =
  let u = subst state u'
      v = subst state v'

      unify' (Free u) (Free v) | u == v = Just state
      unify' u@(Free _) _ = Just (extSubst state u v)
      unify' _ v@(Free _) = Just (extSubst state v u)
      unify' (Ctor c1 ts1) (Ctor c2 ts2) | c1 == c2 && length ts1 == length ts2 =
        foldM (\s (u,v) -> unify s u v) state (zip ts1 ts2)
      unify' (Var _) _ = error "syntactic variable in unification"
      unify' _ (Var _) = error "syntactic variable in unification"
      unify' _ _ = Nothing
  in -- first, eliminate syntactic variables by substitution,
     -- then walk terms
     unify' (walk state u) (walk state v)

maybeToStream Nothing  = Empty
maybeToStream (Just a) = Mature a Empty

eval :: (String -> Def) -> State -> Goal -> Stream State
eval env state (Unify t1 t2) =
  maybeToStream $ unify state t1 t2
eval env state (Disj g1 g2) = eval env state g1 `mplus` eval env state g2
eval env state (Conj g1 g2) = eval env state g1 `bind` (\s -> eval env s (substG state g2))
eval env state (Fresh s g)  = eval env (newVar state s) g
eval env state (Zzz g) = Immature (eval env state g)
eval env state (Invoke f actualArgs) =
  eval env state' body
  where
    Def _ formalArgs body = env f
    state' = foldl bindVar state $ zip formalArgs (map (subst state) actualArgs)

env :: Spec -> String -> Def
env spec name =
  fromMaybe
    (error $ "No definition with name " ++ name ++ " in specification!")
    (find (\ (Def n _ _) -> n == name) (defs spec))

reify :: Term -> Stream State -> [Term]
reify var =
  map' (reifyState var)
  where
    map' _ Empty = []
    map' f (Mature h t) = f h : map' f t
    map' _ (Immature _) = error "Trying to reify immature stream"

    reifyState var state =
      let var' = walk' state var in
      walk' (reify' emptyState var') var'
      where
        walk' state var =
          case walk'' state var of
            Ctor name args -> Ctor name (map (walk' state) args)
            x -> x

        walk'' :: State -> Term -> Term
        walk'' state (Free i) =
          case lookup i (getSubst state) of
            Nothing -> Free i
            Just i' -> walk'' state i'
        walk'' _ u = u

        reify' state var =
          case walk'' state var of
            Free i -> extS state i
            Ctor name args -> foldl reify' state args
            Var n -> state
          where
            extS state i =
              State { getSubst = (i, reifyName $ index state) : getSubst state
                    , getState = getState state
                    , index = index state + 1
                    , vars = vars state
                    }
        reifyName i = Var ("_." ++ show i)

var = Var
(===) = Unify
(|||) = Disj
(&&&) = Conj
callFresh = Fresh
zzz = Zzz
nil = Ctor "Nil" []
cons h t = Ctor "Cons" [h,t]

seq2 f [x]    = x
seq2 f (x:xs) = x `f` seq2 f xs

conj = seq2 (&&&)
disj = seq2 (|||)

conde ds = disj (map conj ds)

fresh xs g = foldr Fresh g xs
