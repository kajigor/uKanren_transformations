{-# LANGUAGE TupleSections #-}

module NonConjunctive.Residualization where

import           Control.Applicative   ((<|>))
import qualified CPD.LocalControl      as LC
import qualified CPD.Residualization   as CpdR
import           Data.List             (find, intercalate, nub, sort, union,
                                        (\\))
import           Data.Maybe            (catMaybes, fromJust, fromMaybe)
import           Debug.Trace           (trace, traceM)
import           Embed                 (isInst)
import qualified Eval                  as E
import           NonConjunctive.Unfold
import qualified Residualize           as Res
import           Syntax
import           Text.Printf           (printf)
import           Unfold                (normalize)
import           Util.Miscellaneous    (fst3, show', snd3, trd3)

topLevel :: Program -> Program
topLevel input =
  residualize $ nonConjunctive (-1) input


residualize :: (NCTree, G S, [S]) -> Program
residualize (Fail, goal, names) = Program [] (generateGoal goal names)
residualize (tree, goal, names) =
  let restricted = restrictSubsts tree in
  let (defs, newGoal) = generateDefs restricted in
  Program defs newGoal

generateDefs :: NCTree -> ([Def], G X)
generateDefs tree =
  let toplevel = fromJust $ nodeContent tree in
  let leaves = collectLeaves tree in
  let distinct = nub $ map snd leaves in
  let simplified = simplify $ renameAmbigousVars $ tree in
  let nodes = (toplevel, simplified) : map (flip findNode tree) distinct in
  let definitions = foldl (\defs gs -> fst3 (CpdR.renameGoals gs defs) ) [] $ map fst nodes in
  trace (printf "\nDefs:\n%s\n" (showDefinitions definitions)) $
  let defWithTree = zip (reverse definitions) (map snd nodes) in
  let invocations = map (generateInvocation definitions) leaves in
  let defs = map (generateDef definitions invocations) defWithTree in
  -- let defs = map (generateDef invocations) defWithTree in
  let (_, newGoal) = generateInvocation definitions (toplevel, toplevel) in
  (defs, Res.vident <$> newGoal)

showDefinitions = intercalate "\n\n" . map go
  where
    go (gs, n, args) = printf "%s %s: %s" n (show args) (show gs)

generateInvocation :: CpdR.Definitions -> ([G S], [G S]) -> ([G S], G S)
generateInvocation defs (gs, v) =
    let Just (goal, n, as) = find ((v ==) . (fst3)) defs in
    let name = n in
    let args = generateArgs as in
    let res = call name args in
    trace (printf "\nGenerating Invocations\nGs:\n%s\nV:\n%s\nRes:\n%s\n" (show gs) (show v) (show res)) $
    (gs, call name args)
  where
    generateArgs xs =
      case CpdR.unifyInvocationLists v gs (Just E.s0) of
        Just subst ->
          map (\a -> fromMaybe (V a) (lookup a subst)) xs
        Nothing -> error (printf "Failed to generate invocation for %s" (show v))
    getArgs (Invoke _ args) = args

generateInvocation' :: CpdR.Definitions -> [G S] -> [G S] -> G S
generateInvocation' defs gs v =
    let Just (goal, n, as) = find ((v ==) . (fst3)) defs in
    let name = n in
    let args = generateArgs as in
    let res = call name args in
    trace (printf "\nGenerating Invocations\nGs:\n%s\nV:\n%s\nRes:\n%s\n" (show gs) (show v) (show res)) $
    (call name args)
  where
    generateArgs xs =
      case CpdR.unifyInvocationLists v gs (Just E.s0) of
        Just subst ->
          map (\a -> fromMaybe (V a) (lookup a subst)) xs
        Nothing -> error (printf "Failed to generate invocation for %s" (show v))
    getArgs (Invoke _ args) = args



findNode :: [G S] -> NCTree -> ([G S], NCTree)
findNode v tree =
    let nodes = go tree in
    case find nontrivial nodes of
      Just n -> (v, simplify $ renameAmbigousVars n)
      Nothing -> error $ printf "Residualization error: no node for\n%s" (show v)
  where
    go node@(Or _ (LC.Descend goal _) _) | goal == v = return node
    go node@(Conj _ goal _)              | goal == v = return node
    go node@(Split _ goal _)             | goal == v = return node
    go (Or ch _ _)                       = concatMap go ch
    go (Conj ch _ _)                     = concatMap go ch
    go (Split ch _ _)                    = concatMap go ch
    go _                                 = []

nontrivial :: NCTree -> Bool
nontrivial (Leaf _ _ _ _) = False
nontrivial _              = True

nodeContent (Or _ (LC.Descend goal _) _) = Just goal
nodeContent (Conj _ goal _)              = Just goal
nodeContent (Split _ goal _)             = Just goal
nodeContent x                            = Nothing -- error "Failed to get node content: unsupported node type"

generateDef :: CpdR.Definitions -> [([G S], G S)] -> (([G S], Name, [S]), NCTree) -> Def
generateDef defs invocations ((gs, n, args), tree) =
  let body = generateGoalFromTree defs invocations tree args in
  let argsX = map Res.vident args in
  Def n argsX (E.postEval argsX body)

-- generateGoalFromTree :: [([G S], G S)] -> NCTree -> [S] -> G X
generateGoalFromTree :: CpdR.Definitions -> [([G S], G S)] -> NCTree -> [S] -> G X
generateGoalFromTree definitions invocations tree args =
    case go args True tree of
      Just goal ->
        let normalized = goal in --  NonConjunctive.Unfold.disj $ map NonConjunctive.Unfold.conj $ normalize goal in
        Res.vident <$> normalized
      Nothing -> error $ printf "Failed to generate relation body for %s" (show $ nodeContent tree)
    -- Res.vident <$> (disj (map conj $ filter (not . null) $ go tree))
  where
    disj :: [G S] -> Maybe (G S)
    disj [] = Nothing
    disj xs = Just $ foldl1 (:\/:) xs

    conj :: [G S] -> Maybe (G S)
    conj [] = Nothing
    conj xs = Just $ foldl1 (:/\:) xs

    fail = call "fail" []
    success = call "success" []

    residualizeState :: E.Sigma -> Maybe (G S)
    residualizeState xs = (conj $ map (\(s, ts) -> (V s) === ts) $ reverse xs) <|> return success

    go :: [S] -> Bool -> NCTree -> Maybe (G S)
    go seen r Fail           = Just fail
    go seen r (Success ss _) = residualizeState ss
    go seen r (Or ch (LC.Descend gs _) s) = do
      -- let vs = getNewVars seen gs s
      let unifs = residualizeState s
      let rest = getInvocation r gs <|> (disj $ catMaybes $ map (go seen False) ch)
      mkGoal unifs rest (:/\:)
    go seen r (Split ch gs s)  = do
      -- let vs = getNewVars seen gs s
      let unifs = residualizeState s
      let rest = getInvocation r gs <|> (conj $ catMaybes $ map (go seen False) ch)
      mkGoal unifs rest (:/\:)
    go seen r (Leaf gs s _ vs) = do
      let unifs = residualizeState s
      let rest = getInvocation' gs vs -- snd (fromJust $ find ((gs ==) . fst) invocations)
      mkGoal unifs rest (:/\:)
    go seen r (Conj ch gs s)   = do
      let unifs = residualizeState s
      let rest = getInvocation r gs <|> (conj $ catMaybes $ map (go seen False) ch)
      mkGoal unifs rest (:/\:)
    go seen r (Gen _ _ _)     = error "Failed to residualize: Gen node in tree"
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


  -- go :: NCTree -> [[G S]]
    -- go Fail            = [[fail]]
    -- go (Success ss _) | null ss = [[success]]
    -- go (Success ss _)  = [residualizeState ss]
    -- go (Or ch _ s)     = let unifs = residualizeState s in concatMap (map (unifs ++) . go) ch
    -- go (Conj ch _ s)   = let unifs = residualizeState s in map (unifs ++) $ concat $ productList $ map go ch
    -- go (Split ch _ s)  = let unifs = residualizeState s in map (unifs ++) $ concat $ productList $ map go ch
    -- go (Leaf gs s _ _) = [residualizeState s ++ [snd (fromJust $ find ((gs ==) . fst) invocations)]]
    -- go (Gen _ _ _)     = error "Failed to residualize: Gen node in tree"
    -- go (Prune _ _)     = error "Failed to residualize: Prune node in tree"


renameAmbigousVars :: NCTree -> NCTree
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

  --   maxVar :: NCTree -> Int
  --   maxVar = maximum . getVarsTree

  --   getVarsTree (Success s _) = getVars [] s
  --   getVarsTree (Or ch (LC.Descend gs y) s) = nub $ (concatMap getVarsTree ch) ++ getVars gs s
  --   getVarsTree (Split ch gs s) = nub $ (concatMap getVarsTree ch) ++ getVars gs s
  --   getVarsTree (Leaf gs s _ _) = getVars gs s
  --   getVarsTree _ = []

  --   renameTree :: Int -> [S] -> NCTree -> NCTree
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

  --       renameVars :: Int -> [S] -> (S -> S)
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

collectLeaves :: NCTree -> [([G S], [G S])]
collectLeaves (Leaf  gs _ _ v) = [(gs, v)]
collectLeaves (Or    ch _ _)   = concatMap collectLeaves ch
collectLeaves (Conj  ch _ _)   = concatMap collectLeaves ch
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
collectLeaves (Gen _ _ _)      = error "This method was not supposed to create Gen nodes"
collectLeaves g = []
