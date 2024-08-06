{-# LANGUAGE TupleSections #-}

module PD.Residualization where

import PD.PartialDeduction
import qualified CPD.Residualization as CpdR
import Control.Applicative ((<|>))
import Data.List (find, nub)
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import Def
import Descend
import Embed (isVariant)
import qualified Eval as E
import Program
import qualified Residualization as Res
import qualified Subst
import Syntax
import Text.Printf (printf)
import Util.Miscellaneous (fst3)

residualize :: (PDTree, G S, [S]) -> Program G X
residualize (Fail, goal, names) = Program [] (generateGoal goal names)
residualize (tree, goal, names) =
  let (defs, newGoal) = generateDefs tree in
  Program defs newGoal

generateDefs :: PDTree -> ([Def G X], G X)
generateDefs tree =
  let toplevel = fromJust $ nodeContent tree in
  let leaves = collectLeaves tree in
  let gens = collectGens tree in
  let distinct = nub $ map snd (leaves ++ gens) in
  let simplified = restrictSubsts $ simplify $ renameAmbigousVars tree in
  -- let nodes = (toplevel, simplified) : map (\(_, x) -> findNode x tree) gens ++ map (`findNode` tree) distinct in
  let nodes = (toplevel, simplified) : map (`findNode` tree) distinct in
  let definitions = foldl (\defs gs -> fst3 (CpdR.renameGoals gs defs)) [] $ map fst nodes in
  let defWithTree = zip (reverse definitions) (map snd nodes) in
  let invocations = map (generateInvocation definitions) leaves in
  let defs = map (generateDef definitions invocations) defWithTree in
  let (_, newGoal) = generateInvocation definitions (toplevel, toplevel) in
  (defs, Res.vident <$> newGoal)

generateInvocation :: CpdR.Definitions -> ([G S], [G S]) -> ([G S], G S)
generateInvocation defs (gs, v) =
    let Just (goal, n, as) = find ((v ==) . fst3) defs in
    let name = n in
    let args = generateArgs as in
    let res = call name args in
    (gs, call name args)
  where
    generateArgs xs =
      case CpdR.unifyInvocationLists v gs (Just Subst.empty) of
        Just subst ->
          map (\a -> fromMaybe (V a) (Subst.lookup a subst)) xs
        Nothing -> error (printf "Failed to generate invocation for %s" (show v))
    getArgs (Invoke _ args) = args

generateInvocation' defs gs v =
  snd $ generateInvocation defs (gs, v)

findNode :: [G S] -> PDTree -> ([G S], PDTree)
findNode v tree =
  let nodes = go tree in
  case find nontrivial nodes of
    Just n -> (v, restrictSubsts $ simplify $ renameAmbigousVars n)
    Nothing -> error $ printf "Residualization error: no node for\n%s" (show v)
  where
    go node@(Or _ (Descend goal _) _) | [goal] == v {-|| [goal] `isVariant` v -}= return node
    go node@(Conj _ goal _) | goal == v {-|| goal `isVariant` v -}= return node
    go node@(Gen ch goal _) | [goal] == v {-|| [goal] `isVariant` v-} = [node, ch]
    go node@(Leaf goal _ _) | [goal] == v {-|| [goal] `isVariant` v-} = return node
    go (Or ch _ _) = concatMap go ch
    go (Conj ch _ _) = concatMap go ch
    go (Gen ch _ _) = go ch
    go _ = []

nontrivial :: PDTree -> Bool
-- nontrivial (Leaf _ _ _) = False
nontrivial _ = True

nodeContent (Or _ (Descend goal _) _) = Just [goal]
nodeContent (Conj _ goal _) = Just goal
nodeContent x = Nothing -- error "Failed to get node content: unsupported node type"

generateDef :: CpdR.Definitions -> [([G S], G S)] -> (([G S], Name, [S]), PDTree) -> Def G X
generateDef defs invocations ((gs, n, args), tree) =
  let body = generateGoalFromTree defs invocations tree args in
  let argsX = map Res.vident args in
  Def n argsX (E.postEval argsX body)

generateGoalFromTree :: CpdR.Definitions -> [([G S], G S)] -> PDTree -> [S] -> G X
generateGoalFromTree definitions invocations tree args =
  case go True tree of
    Just goal -> Res.vident <$> goal
    Nothing -> error $ printf "Failed to generate relation body for %s" (show $ nodeContent tree)
  where
    residualizeEnv :: Subst.Subst S -> Maybe (G S)
    residualizeEnv xs =
      conj (map (\(s, ts) -> V s === ts) $ reverse (Subst.toList xs)) <|> return success

    go :: Bool -> PDTree -> Maybe (G S)
    go r Fail = Just failure
    go r (Success s) = residualizeEnv s
    go r (Or ch (Descend gs _) s) =
      let unifs = residualizeEnv s in
      let rest = getInvocation r [gs] <|> disj (mapMaybe (go False) ch) in
      mkGoal unifs rest
    go r (Leaf gs s vs) =
      let unifs = residualizeEnv s in
      let rest = getInvocation' [gs] [vs] in
      mkGoal unifs rest
    go r (Conj ch gs s) =
      let unifs = residualizeEnv s in
      let rest = getInvocation r gs <|> conj (mapMaybe (go False) ch) in
      mkGoal unifs rest
    go r (Gen ch gs gs') =
      getInvocation' [gs] [gs'] <|> conj (catMaybes [go False ch]) 

    mkGoal (Just u) (Just r) =
      Just (f u r)
      where
        f (Conjunction x x' xs) (Conjunction y y' ys) = Conjunction x x' (xs ++ y : y' : ys)
        f g (Conjunction y y' ys) = Conjunction g y (y' : ys)
        f (Conjunction x x' xs) g = Conjunction x x' (xs ++ [g])
        f g h = Conjunction g h []
    mkGoal (Just u) Nothing = Just u
    mkGoal Nothing (Just r) = Just r
    mkGoal _ _ = Nothing


    getInvocation True gs =
      Nothing
    getInvocation _ gs =
      snd <$> find ((gs ==) . fst) invocations

    getInvocation' gs v = return $ generateInvocation' definitions gs v


renameAmbigousVars :: PDTree -> PDTree
renameAmbigousVars tree = tree

generateGoal :: G S -> [S] -> G X
generateGoal g ns = Res.vident <$> g

collectLeaves :: PDTree -> [([G S], [G S])]
collectLeaves (Leaf gs _ v) = [([gs], [v])]
collectLeaves (Or ch _ _) = concatMap collectLeaves ch
collectLeaves (Conj ch _ _) = concatMap collectLeaves ch
collectLeaves (Gen ch gs gs') = collectLeaves ch
collectLeaves g = []

collectGens :: PDTree -> [([G S], [G S])]
collectGens (Leaf gs _ v) = []
collectGens (Or ch _ _) = concatMap collectGens ch
collectGens (Conj ch _ _) = concatMap collectGens ch
collectGens (Gen ch gs gs') = ([gs], [gs']) : collectGens ch
collectGens g = []
