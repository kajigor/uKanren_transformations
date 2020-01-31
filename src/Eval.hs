{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

module Eval where

import           Control.Monad
import           Data.List
import           Stream
import           Syntax
import           Text.Printf

-- States
type Iota  = ([X], X -> Ts)
type Sigma = [(S, Ts)]
type Delta = [S]
type P     = Name -> Def
type Gamma = (P, Iota, Delta)

unifyG :: (S -> Ts -> Sigma -> Bool)
          -> Maybe Sigma -> Ts -> Ts -> Maybe Sigma
unifyG _ Nothing _ _ = Nothing
unifyG f st@(Just subst) u v =
  unify' (walk u subst) (walk v subst)  where
    unify' (V u') (V v') | u' == v' = Just subst
    unify' (V u') (V v') = Just $ (min u' v', V $ max u' v') : subst
    unify' (V u') t = 
      if f u' t subst 
      then Nothing 
      else return $ (u', v) : subst 
    unify' t (V v') = 
      if f v' t subst 
      then Nothing 
      else return $ (v', u) : subst  
    unify' (C a as) (C b bs) | a == b && length as == length bs =
      foldl (\ st' (u', v') -> unifyG f st' u' v') st $ zip as bs
    unify' _ _ = Nothing

walk :: Ts -> Sigma -> Ts
walk x@(V v') s =
  case lookup v' s of
    Nothing -> x
    Just t  -> walk t s
walk u' _ = u'

-- Unification
unify :: Maybe Sigma -> Ts -> Ts -> Maybe Sigma
unify =
    unifyG occursCheck
  where
    occursCheck :: S -> Ts -> Sigma -> Bool
    occursCheck u' t s = 
      let t' = walk t s in
      case t' of
        V v' | v' == u' -> True 
        V _ -> False 
        C _ as -> any (\x -> occursCheck u' x s) as

    -- occursCheck u' t s = if elem u' $ fv t then Nothing else s

unifySubsts :: Sigma -> Sigma -> Maybe Sigma
unifySubsts one two =
    -- trace ("one: " ++ show one ++ "\ntwo: " ++ show two) $
    let maximumVar = max (findUpper one) (findUpper two) in
    let one' = manifactureTerm maximumVar one in
    let two' = manifactureTerm maximumVar two in
    unify (Just s0) one' two'
  where
    findUpper []  = 0
    findUpper lst = maximum $ map fst lst
    supplement upper lst = lst --  [(x, y) | x <- [0..upper], let y = maybe (V x) id (lookup x lst)]
    manifactureTerm upper subst = C "ManifacturedTerm" $ map snd $ supplement upper subst

unifyNoOccursCheck :: Maybe Sigma -> Ts -> Ts -> Maybe Sigma
unifyNoOccursCheck = unifyG (\_ _ -> const True)

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

-- Applying substitution
class Subst a where
  substitute :: Sigma -> a -> a

instance Subst (Term S) where
  substitute s t@(V x) =
    case lookup x s of
      Just tx | tx /= t -> substitute s tx
      _                 -> t
  substitute s (C m ts) = C m $ map (substitute s) ts

instance Subst (G S) where
  substitute s (Invoke name as) = Invoke name (map (substitute s) as)
  substitute _ g = error $ printf "We have only planned to substitute into calls, and you are trying to substitute into:\n%s" (show g)

instance Subst [G S] where
  substitute = map . substitute

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
preEval :: Gamma -> G X -> (G S, Gamma, [S])
preEval = go []
 where
  go vars g@(_, i, _) (t1 :=: t2)  = (i <@> t1 :=: i <@> t2, g, vars)
  go vars g           (g1 :/\: g2) = let (g1', g' , vars' ) = go vars  g  g1 in
                                     let (g2', g'', vars'') = go vars' g' g2 in
                                     (g1' :/\: g2', g'', vars'')
  go vars g           (g1 :\/: g2) = let (g1', g' , vars')  = go vars  g  g1 in
                                     let (g2', g'', vars'') = go vars' g' g2 in
                                     (g1' :\/: g2', g'', vars'')
  go vars   (p, i, y : d') (Fresh x g') =
    go (y : vars) (p, extend i x (V y), d') g'
  go vars g@(_, i, _) (Invoke f fs)  = (Invoke f (map (i <@>) fs), g, vars)
  go vars e           (Let    def g) = let (g', e', vars') = go vars e g in
                                       (Let def g', e', vars')

postEval :: [X] -> G X -> G X
postEval as goal =
  let freshs = fvg goal \\ as in
  foldr Fresh (go (freshs ++ as) goal) freshs
  where
    go vars (Let (Def f args b) g) =
      let freshs = (fvg b \\ args) \\ vars in
      let b' = foldr Fresh (go (vars ++ args ++ freshs) b) freshs in
      Let (Def f args b') $ go vars g
    go vars (g1 :/\: g2) = go vars g1 :/\: go vars g2
    go vars (g1 :\/: g2) = go vars g1 :\/: go vars g2
    go _ g = g

topLevel :: Program -> Stream (Sigma, Delta)
topLevel (Program defs goal) =
  let gamma = foldl update env0 defs in
  let (goal', gamma', _) = preEval gamma goal in
  eval gamma' s0 goal'

-- Evaluation relation
eval :: Gamma -> Sigma -> G S -> Stream (Sigma, Delta)
eval     (_, _, d) s (t1 :=:  t2)  = fmap (,d) (maybeToStream $ unify (Just s) t1 t2)
eval env           s (g1 :\/: g2)  = eval env s g1 `mplus` eval env s g2
eval env@(p, i, _) s (g1 :/\: g2)  = eval env s g1 >>= (\ (s', d') -> eval (p, i, d') s' g2)
eval     (p, i, d) s (Invoke f as) =
  let (Def _ fs g) = p f in
  let i'         = foldl (\ i'' (f', a) -> extend i'' f' a) i $ zip fs as in
  let (g', env', _) = preEval (p, i', d) g in
  eval env' s g'
eval env s (Let def g) = eval (update env def) s g
eval _ _ _ = error "Impossible case in eval"

env0 :: Gamma
env0 = (\ i -> error $ printf "Empty environment on %s" (show i), emptyIota, [0 ..])

update :: Gamma -> Def -> Gamma
update (p, i, d) def@(Def name _ _) = (\ name' -> if name == name' then def else p name', i, d)

updateDefsInGamma :: Gamma -> [Def] -> Gamma
updateDefsInGamma = foldl update

s0 :: Sigma
s0 = []

run :: G X -> Stream Sigma
run goal =
  let (goal', env', _) = preEval env0 goal in
  fmap fst $ eval env' s0 goal'
