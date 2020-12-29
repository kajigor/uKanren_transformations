{-# LANGUAGE TupleSections #-}

module ConsPD.Residualization where

import           ConsPD.Unfold
import           Control.Applicative ((<|>))
import qualified CPD.LocalControl    as LC
import qualified CPD.Residualization as CpdR
import           Data.List ( find, intercalate, nub )
import           Data.Maybe          (catMaybes, fromJust, fromMaybe)
import qualified Eval                as E
import qualified Residualization     as Res
import qualified Subst
import           Syntax
import           Text.Printf         (printf)
import           Util.Miscellaneous  (fst3)

topLevel :: Program -> Program
topLevel input =
  residualize $ ConsPD.Unfold.topLevel (-1) input


residualize :: (ConsPDTree, G S, [S]) -> Program
residualize (Fail, goal, names) = Program [] (generateGoal goal names)
residualize (tree, goal, names) =
  -- let restricted = restrictSubsts tree in
  let restricted = tree in
  let (defs, newGoal) = generateDefs restricted in
  Program defs newGoal

generateDefs :: ConsPDTree -> ([Def], G X)
generateDefs tree =
  let toplevel = fromJust $ nodeContent tree in
  let leaves = collectLeaves tree in
  let gens = collectGens tree in
  let distinct = nub $ map snd leaves in
  let simplified = restrictSubsts $ simplify $ renameAmbigousVars $ tree in
  let nodes = (toplevel, simplified) : map (\(_,x) -> findNode x tree) gens ++ map (`findNode` tree) distinct in
  let definitions = foldl (\defs gs -> fst3 (CpdR.renameGoals gs defs) ) [] $ map fst nodes in
  let defWithTree = zip (reverse definitions) (map snd nodes) in
  let invocations = map (generateInvocation definitions) leaves in
  let defs = map (generateDef definitions invocations) defWithTree in
  -- let defs = map (generateDef invocations) defWithTree in
  let (_, newGoal) = generateInvocation definitions (toplevel, toplevel) in
  (defs, Res.vident <$> newGoal)

showDefinitions :: CpdR.Definitions -> String
showDefinitions = intercalate "\n\n" . map go
  where
    go (gs, n, args) = printf "%s %s: %s" n (show args) (show gs)

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

generateInvocation' :: CpdR.Definitions -> [G S] -> [G S] -> G S
generateInvocation' defs gs v =
    let Just (goal, n, as) = find ((v ==) . (fst3)) defs in
    let name = n in
    let args = generateArgs as in
    let res = call name args in
    (call name args)
  where
    generateArgs xs =
      case CpdR.unifyInvocationLists v gs (Just Subst.empty) of
        Just subst ->
          map (\a -> fromMaybe (V a) (Subst.lookup a subst)) xs
        Nothing -> error (printf "Failed to generate invocation for %s" (show v))
    getArgs (Invoke _ args) = args



findNode :: [G S] -> ConsPDTree -> ([G S], ConsPDTree)
findNode v tree =
    let nodes = go tree in
    case find nontrivial nodes of
      Just n -> (v, restrictSubsts $ simplify $ renameAmbigousVars n)
      Nothing -> error $ printf "Residualization error: no node for\n%s" (show v)
  where
    go node@(Or _ (LC.Descend goal _) _) | goal == v = return node
    go node@(Conj _ goal _)              | goal == v = return node
    go node@(Split _ goal _)             | goal == v = return node
    go node@(Gen ch _ goal _ _)          | goal == v = [node, ch]
    go (Or ch _ _)                       = concatMap go ch
    go (Conj ch _ _)                     = concatMap go ch
    go (Split ch _ _)                    = concatMap go ch
    go (Gen ch _ _ _ _)                  = go ch
    go _                                 = []

nontrivial :: ConsPDTree -> Bool
nontrivial (Leaf _ _ _ _) = False
nontrivial _              = True

nodeContent (Or _ (LC.Descend goal _) _) = Just goal
nodeContent (Conj _ goal _)              = Just goal
nodeContent (Split _ goal _)             = Just goal
nodeContent x                            = Nothing -- error "Failed to get node content: unsupported node type"

generateDef :: CpdR.Definitions -> [([G S], G S)] -> (([G S], Name, [S]), ConsPDTree) -> Def
generateDef defs invocations ((gs, n, args), tree) =
  let body = generateGoalFromTree defs invocations tree args in
  let argsX = map Res.vident args in
  Def n argsX (E.postEval argsX body)

-- generateGoalFromTree :: [([G S], G S)] -> ConsPDTree -> FN.FreshNames -> G X
generateGoalFromTree :: CpdR.Definitions -> [([G S], G S)] -> ConsPDTree -> [S] -> G X
generateGoalFromTree definitions invocations tree args =
    case go args True tree of
      Just goal ->
        let normalized = goal in --  NonConjunctive.Unfold.disj $ map NonConjunctive.Unfold.conj $ normalize goal in
        Res.vident <$> normalized
      Nothing -> error $ printf "Failed to generate relation body for %s" (show $ nodeContent tree)
    -- Res.vident <$> (disj (map conj $ filter (not . null) $ go tree))
  where
    residualizeEnv :: Subst.Subst -> Maybe (G S)
    residualizeEnv xs =
      (conj $ map (\(s, ts) -> (V s) === ts) $ reverse (Subst.toList xs)) <|> return success

    go :: [S] -> Bool -> ConsPDTree -> Maybe (G S)
    go seen r Fail           = Just failure
    go seen r (Success ss _) = residualizeEnv ss
    go seen r (Or ch (LC.Descend gs _) s) = do
      -- let vs = getNewVars seen gs s
      let unifs = residualizeEnv s
      let rest = getInvocation r gs <|> (disj $ catMaybes $ map (go seen False) ch)
      mkGoal unifs rest (:/\:)
    go seen r (Split ch gs s)  = do
      -- let vs = getNewVars seen gs s
      let unifs = residualizeEnv s
      let rest = getInvocation r gs <|> (conj $ catMaybes $ map (go seen False) ch)
      mkGoal unifs rest (:/\:)
    go seen r (Leaf gs s _ vs) = do
      let unifs = residualizeEnv s
      let rest = getInvocation' gs vs -- snd (fromJust $ find ((gs ==) . fst) invocations)
      mkGoal unifs rest (:/\:)
    go seen r (Conj ch gs s)   = do
      let unifs = residualizeEnv s
      let rest = getInvocation r gs <|> (conj $ catMaybes $ map (go seen False) ch)
      mkGoal unifs rest (:/\:)
    go seen r (Gen ch gs gs' gen s) = do
      let unifs = residualizeEnv s
      let rest = getInvocation r gs
      mkGoal unifs rest (:/\:)
    go seen r (Prune _ _)     = error "Failed to residualize: Prune node in tree"

    mkGoal (Just u) (Just r) f = Just (f u r)
    mkGoal (Just u) Nothing _  = Just u
    mkGoal Nothing (Just r) _  = Just r
    mkGoal _ _ _               = Nothing

    -- getNewVars seen goal subst =
    --   let vg = concatMap fvgs goal in
    --   let vs = map fst subst ++ concatMap (fv . snd) subst in
    --   (nub $ union vg vs) \\ seen

    getInvocation True _ = Nothing
    getInvocation _   gs =
      snd <$> (find ((gs ==) . fst) invocations)

    getInvocation' gs v = return $ generateInvocation' definitions gs v


  -- go :: ConsPDTree -> [[G S]]
    -- go Fail            = [[fail]]
    -- go (Success ss _) | null ss = [[success]]
    -- go (Success ss _)  = [residualizeEnv ss]
    -- go (Or ch _ s)     = let unifs = residualizeEnv s in concatMap (map (unifs ++) . go) ch
    -- go (Conj ch _ s)   = let unifs = residualizeEnv s in map (unifs ++) $ concat $ productList $ map go ch
    -- go (Split ch _ s)  = let unifs = residualizeEnv s in map (unifs ++) $ concat $ productList $ map go ch
    -- go (Leaf gs s _ _) = [residualizeEnv s ++ [snd (fromJust $ find ((gs ==) . fst) invocations)]]
    -- go (Gen _ _ _)     = error "Failed to residualize: Gen node in tree"
    -- go (Prune _ _)     = error "Failed to residualize: Prune node in tree"


renameAmbigousVars :: ConsPDTree -> ConsPDTree
renameAmbigousVars tree = tree
  --   go (getVars (fromJust $ nodeContent tree) []) tree
  -- where
  --   go seen (Or ch d@(LC.Descend gs _) s) =
  --     let vs = getNewVars seen gs s in
  --     (Or (map (go (seen ++ vs)) ch) d s)
  --   go seen t@(Split ch gs s) =
  --     let vs = getNewVars seen gs s in
  --     let m = maxVar t + 1 in
  --     let renamed = map (\(n, t) -> renameTree (n*m) seen t) (zip [1..] ch) in
  --     (Split (map (go (seen ++ vs)) renamed) gs s)
  --   go seen (Conj _ _ _) = error "Failed to rename: Conj node in tree "
  --   go seen (Gen _ _ _) = error "Failed to rename: Gen node in tree"
  --   go seen (Prune _ _) = error "Failed to rename: Prune node in tree"
  --   go _ g = g

  --   getNewVars seen goal subst =
  --     let vars = getVars goal subst in
  --     -- trace (printf "\n\nNewVars\nGoal\n%s\nVars\n%s\n" (show goal) (show $  vars \\ seen)) $
  --     (vars \\ seen)

  --   getVars goal subst =
  --     let vg = concatMap fvgs goal in
  --     let vs = map fst subst ++ concatMap (fv . snd) subst in
  --     nub $ union vg vs

  --   maxVar :: ConsPDTree -> Int
  --   maxVar = maximum . getVarsTree

  --   getVarsTree (Success s _) = getVars [] s
  --   getVarsTree (Or ch (LC.Descend gs y) s) = nub $ (concatMap getVarsTree ch) ++ getVars gs s
  --   getVarsTree (Split ch gs s) = nub $ (concatMap getVarsTree ch) ++ getVars gs s
  --   getVarsTree (Leaf gs s _ _) = getVars gs s
  --   getVarsTree _ = []

  --   renameTree :: Int -> FN.FreshNames -> ConsPDTree -> ConsPDTree
  --   renameTree n seen =
  --       go
  --     where
  --       f = renameVars n seen

  --       go Fail = Fail
  --       go (Success s g) = Success (renameSubst s) g
  --       go (Or ch (LC.Descend x y) s) = Or (map go ch) (LC.Descend (renameGoals x) y) (renameSubst s)
  --       go (Split ch gs s) = Split (map go ch) (renameGoals gs) (renameSubst s)
  --       go (Leaf gs s g v) = Leaf (renameGoals gs) (renameSubst s) g v
  --       go g = g

  --       renameGoals = ((f <$>) <$>)

  --       renameSubst = map (\(v,t) -> (f v, f <$> t))

  --       renameVars :: Int -> FN.FreshNames -> (S -> S)
  --       renameVars n seen =
  --           rename
  --         where
  --           rename v | v `elem` seen = v
  --           rename v = v + n

  --   disj :: [G S] -> (G S)
  --   disj xs = foldl1 (:\/:) xs

  --   conj :: [G S] -> (G S)
  --   conj xs = foldl1 (:/\:) xs


generateGoal :: G S -> [S] -> G X
generateGoal g ns = (Res.vident <$> g)

collectLeaves :: ConsPDTree -> [([G S], [G S])]
collectLeaves (Leaf  gs _ _ v) = [(gs, v)]
collectLeaves (Or    ch _ _)   = concatMap collectLeaves ch
collectLeaves (Conj  ch _ _)   = concatMap collectLeaves ch
collectLeaves (Gen ch gs gs' _ _) = collectLeaves ch
collectLeaves (Split [x] _ _)  = collectLeaves x
collectLeaves (Split ch _ _)   =
  let children =
        catMaybes $ map (\x -> do y <- nodeContent x
                                  return (y,y))
                        ch
  in
  let deeperLeaves = concatMap collectLeaves ch in
  children ++ deeperLeaves
collectLeaves (Prune _ _)      = error "Cannot residualize a tree with Prune nodes"
collectLeaves g = []

collectGens :: ConsPDTree -> [([G S], [G S])]
collectGens (Leaf  gs _ _ v) = []
collectGens (Or    ch _ _)   = concatMap collectGens ch
collectGens (Conj  ch _ _)   = concatMap collectGens ch
collectGens (Split ch _ _)  = concatMap collectGens ch
collectGens (Gen ch gs gs' _ _) = (gs, gs') : collectGens ch
collectGens (Prune _ _)      = error "Cannot residualize a tree with Prune nodes"
collectGens g = []
