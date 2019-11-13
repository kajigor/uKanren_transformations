{-# LANGUAGE TupleSections #-}

module Eval where

import Control.Monad
import Data.List
import Syntax
import Stream
import Debug.Trace
import Text.Printf

-- States
type Iota  = ([X], X -> Ts)
type Sigma = [(S, Ts)]
type Delta = [S]
type P     = Name -> Def
type Gamma = (P, Iota, Delta)

unifyG :: (S -> Ts -> Maybe Sigma -> Maybe Sigma)
          -> Maybe Sigma -> Ts -> Ts -> Maybe Sigma
unifyG _ Nothing _ _ = Nothing
unifyG f st@(Just subst) u v =
  -- trace (printf "Unifying\n%s\nwith\n%s\nin\n%s" (show u) (show v) (show st)) $
  unify' (walk u subst) (walk v subst)  where
    unify' (V u') (V v') | u' == v' = Just subst
    unify' (V u') t = f u' t $ Just $ (u', v) : subst
    unify' t (V v') = f v' t $ Just $ (v', u) : subst
    unify' (C a as) (C b bs) | a == b && length as == length bs =
      foldl (\ st' (u', v') -> unifyG f st' u' v') st $ zip as bs
    unify' _ _ = Nothing
    walk x@(V v') s =
      case lookup v' s of
        Nothing -> x
        Just t  -> walk t s
    walk u' _ = u'

-- Unification
unify :: Maybe Sigma -> Ts -> Ts -> Maybe Sigma
unify = unifyG occursCheck where
  occursCheck u' t s = if elem u' $ fv t then Nothing else s

unifyNoOccursCheck :: Maybe Sigma -> Ts -> Ts -> Maybe Sigma
unifyNoOccursCheck = unifyG (\_ _ -> id)

---- Interpreting syntactic variables
infix 9 <@>
(<@>) :: Iota -> Tx -> Ts
i <@> (V x) = app i x
i <@> (C c ts) = C c $ map (i<@>) ts

showInt :: Iota -> String
showInt (dom, f) = intercalate ", " $ map (\ x -> printf "%s -> %s" x (show $ f x)) dom

---- Extending variable interpretation
extend :: Iota -> X -> Ts -> Iota
extend (xs, i) x ts = (if x `elem` xs then xs else x : xs , \y -> if x == y then ts else i y)

emptyIota :: Iota
emptyIota = ([], error . printf "Empty interpretation on %s" . show)

app :: Iota -> X -> Ts
app (_, i) = i

---- Applying substitution
substitute :: Sigma -> Ts -> Ts
substitute s t@(V x) =
  case lookup x s of
    Just tx | tx /= t -> substitute s tx
    _ -> t
substitute s (C m ts) = C m $ map (substitute s) ts

substituteGoal :: Sigma -> G S -> G S
substituteGoal s (Invoke name as) = Invoke name (map (substitute s) as)
substituteGoal _ g = error $ printf "We have only planned to substitute into calls, and you are trying to substitute into:\n%s" (show g)

substituteConjs :: Sigma -> [G S] -> [G S]
substituteConjs s = map $ substituteGoal s


---- Composing substitutions
o :: Sigma -> Sigma -> Sigma
o sigma theta =
  case map fst sigma `intersect` map fst theta of
    [] -> map (\ (s, ts) -> (s, substitute sigma ts)) theta ++ sigma
    _  -> error "Non-disjoint domains in substitution composition"

dotSigma :: Sigma -> String
dotSigma s = printf " [ %s ] " (intercalate ", " (map (\(x,y) -> printf "%s &rarr; %s" (dot $ V x) (dot y)) s))

showSigma :: Sigma -> String
showSigma s = printf " [ %s ] " (intercalate ", " (map (\(x,y) -> printf "%s &rarr; %s" (show $ V x) (show y)) s))

showSigma' :: Sigma -> String
showSigma' s = printf " [ %s ] " (intercalate ", " (map (\(x,y) -> printf "%s -> %s" (show $ V x) (show y)) s))

-- Pre-evaluation
preEval' :: Gamma -> G X -> (G S, Gamma, [S])
preEval' = preEval []
 where
  preEval vars g@(_, i, _) (t1 :=: t2)    = (i <@> t1 :=: i <@> t2, g, vars)
  preEval vars g           (g1 :/\: g2)   = let (g1', g' , vars' ) = preEval vars  g  g1 in
                                            let (g2', g'', vars'') = preEval vars' g' g2 in
                                            (g1' :/\: g2', g'', vars'')
  preEval vars g           (g1 :\/: g2)   = let (g1', g' , vars')  = preEval vars  g  g1 in
                                            let (g2', g'', vars'') = preEval vars' g' g2 in
                                            (g1' :\/: g2', g'', vars'')
  preEval vars   (p, i, y : d') (Fresh x g') =
    preEval (y : vars) (p, extend i x (V y), d') g'
  preEval vars g@(_, i, _) (Invoke f fs)  = (Invoke f (map (i <@>) fs), g, vars)
  preEval vars e           (Let    def' g) = let (g', e', vars') = preEval vars e g in
                                             (Let def' g', e', vars')

postEval' :: [X] -> G X -> G X
postEval' as goal =
  let freshs = fvg goal \\ as in
  foldr Fresh (postEval (freshs ++ as) goal) freshs
  where
    postEval vars (Let (f, args, b) g) =
      Let (f, args, let freshs = (fvg b \\ args) \\ vars
                    in  foldr Fresh (postEval (vars ++ args ++ freshs) b) freshs) $ postEval vars g
    postEval vars (g1 :/\: g2) = postEval vars g1 :/\: postEval vars g2
    postEval vars (g1 :\/: g2) = postEval vars g1 :\/: postEval vars g2
    postEval _ g = g


-- Evaluation relation
eval :: Gamma -> Sigma -> G S -> Stream (Sigma, Delta)
eval     (_, _, d) s (t1 :=:  t2)  = fmap (,d) (maybeToStream $ unify (Just s) t1 t2)
eval env           s (g1 :\/: g2)  = eval env s g1 `mplus` eval env s g2
eval env@(p, i, _) s (g1 :/\: g2)  = eval env s g1 >>= (\ (s', d') -> eval (p, i, d') s' g2)
eval     (p, i, d) s (Invoke f as) =
  let (_, fs, g) = p f in
  let i'         = foldl (\ i'' (f', a) -> extend i'' f' a) i $ zip fs as in
  let (g', env', _) = preEval' (p, i', d) g in
  eval env' s g'
eval env s (Let def' g) = eval (update env def') s g
eval _ _ _ = error "Impossible case in eval"

env0 :: Gamma
env0 = (\ i -> error $ printf "Empty environment on %s" (show i), emptyIota, [0 ..])

update :: Gamma -> Def -> Gamma
update (p, i, d) def'@(name, _, _) = (\ name' -> if name == name' then def' else p name', i, d)

updateDefsInGamma :: Gamma -> [Def] -> Gamma
updateDefsInGamma = foldl update

s0 :: Sigma
s0 = []

run :: G X -> Stream Sigma
run goal =
  let (goal', env', _) = preEval' env0 goal in
  fmap fst $ eval env' s0 goal'
