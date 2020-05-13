{-# LANGUAGE TupleSections #-}

module NonConjunctive.Residualization where

import qualified CPD.LocalControl      as LC
import qualified CPD.Residualization   as CpdR
import           Data.List             (sort, (\\), nub, find, intercalate)
import           Data.Maybe            (catMaybes, fromJust, fromMaybe)
import           Debug.Trace           (trace)
import           Embed                 (isInst)
import qualified Eval                  as E
import           NonConjunctive.Unfold
import qualified Residualize           as Res
import           Syntax
import           Text.Printf           (printf)
import           Util.Miscellaneous    (fst3, snd3, trd3, show')
import Control.Applicative ((<|>))

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
  let simplified = simplify tree in
  let nodes = (toplevel, simplified) : map (flip findNode tree) distinct in
  let definitions = foldl (\defs gs -> fst3 (CpdR.renameGoals gs defs) ) [] $ map fst nodes in
  trace (printf "\nDefs:\n%s\n" (showDefinitions definitions)) $
  let defWithTree = zip (reverse definitions) (map snd nodes) in
  let invocations = map (generateInvocation definitions) leaves in
  let defs = map (generateDef invocations) defWithTree in
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
    (gs, call name args)
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
      Just n -> trace (printf "\nNontrivialTreeFor\nGs:\n%s\n\nTree:\n%s\n" (show v) (show n)) $  (v, simplify n)
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
nontrivial _ = True

nodeContent (Or _ (LC.Descend goal _) _) = Just goal
nodeContent (Conj _ goal _)              = Just goal
nodeContent (Split _ goal _)             = Just goal
nodeContent x                            = Nothing -- error "Failed to get node content: unsupported node type"

generateDef :: [([G S], G S)] -> (([G S], Name, [S]), NCTree) -> Def
generateDef invocations ((gs, n, args), tree) =
  let body = generateGoalFromTree invocations tree in
  let argsX = map Res.vident args in
  Def n argsX (E.postEval argsX body)

generateGoalFromTree :: [([G S], G S)] -> NCTree -> G X
generateGoalFromTree invocations tree =
    case go True tree of
      Just goal -> Res.vident <$> goal
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

    go :: Bool -> NCTree -> Maybe (G S)
    go r Fail            = Just fail
    go r (Success ss _)  = residualizeState ss
    go r (Or ch (LC.Descend gs _) s)     = do
      unifs <- residualizeState s
      rest <- getInvocation r gs <|> (disj $ catMaybes $ map (go False) ch)
      return $ unifs :/\: rest
    go r (Conj ch gs s)   = do
      unifs <- residualizeState s
      rest <- getInvocation r gs <|> (conj $ catMaybes $ map (go False) ch)
      return $ unifs :/\: rest
    go r (Split ch gs s)  = do
      unifs <- residualizeState s
      rest <- getInvocation r gs <|> (conj $ catMaybes $ map (go False) ch)
      return $ unifs :/\: rest
    go r (Leaf gs s _ _) = do
      unifs <- residualizeState s
      rest <- getInvocation r gs -- snd (fromJust $ find ((gs ==) . fst) invocations)
      return $ unifs :/\: rest
    go r (Gen _ _ _)     = error "Failed to residualize: Gen node in tree"
    go r (Prune _ _)     = error "Failed to residualize: Prune node in tree"

    getInvocation True _ = Nothing
    getInvocation _   gs = snd <$> (find ((gs ==) . fst) invocations)

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

-- data NCTree = Fail
--             | Success E.Sigma E.Gamma
--             | Or [NCTree] (LC.Descend [G S]) E.Sigma
--             | Conj [NCTree] [G S] E.Sigma
--             | Gen NCTree [G S] Generalizer
--             | Leaf [G S] E.Sigma E.Gamma [G S] -- last argument is a goal renaming of which current node is
--             | Split [NCTree] [G S] E.Sigma
--             | Prune [G S] E.Sigma
--             -- deriving (Show, Eq)

generateGoal :: G S -> [S] -> G X
generateGoal g ns = (Res.vident <$> g)

collectLeaves :: NCTree -> [([G S], [G S])]
collectLeaves (Leaf  gs _ _ v) = [(gs, v)]
collectLeaves (Or    ch _ _)   = concatMap collectLeaves ch
collectLeaves (Conj  ch _ _)   = concatMap collectLeaves ch
collectLeaves (Split [x] _ _)  = collectLeaves x
collectLeaves (Split ch _ _)   =
  let children = []
        -- catMaybes $ map (\x -> do y <- nodeContent x
        --                           return (y,y))
        --                 ch
  in
  let deeperLeaves = concatMap collectLeaves ch in
  children ++ deeperLeaves
collectLeaves (Prune _ _)      = error "Cannot residualize a tree with Prune nodes"
collectLeaves (Gen _ _ _)      = error "This method was not supposed to create Gen nodes"
collectLeaves g = []
