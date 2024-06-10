{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module Hypergraph.Hypergraph where

import Prelude hiding (tail, head)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (guard)
import Data.List (subsequences, intercalate)
import Data.Maybe (catMaybes, isJust)
import Syntax ( Term(..) )
import Subst (Subst, empty)
import Eval (unify)
import Debug.Trace

type Var = Integer
type Node n = n
data Hyperarc n l = Arc { tail :: Set.Set n, head :: n, arcLabel :: l } deriving (Eq, Show)
data Hypergraph n al = Hypergraph { nodes :: [Node n], groundNodes :: Set.Set n, arcs :: [Hyperarc n al] } deriving (Eq, Show)

data Mode = Ground | NonGround deriving (Eq, Show, Ord)
data DetLabel = Bot | Det | Nondet deriving (Eq, Show, Ord, Bounded)

type DepRep n = Hypergraph n DetLabel
type DepArc n = Hyperarc n DetLabel
type DepNode n = Node n
type AbstractSubst v = DepRep v

prettyDet Bot = "⊥"
prettyDet Det = "d"
prettyDet Nondet = "?"


varnames = ["x", "y", "z", "w"]

prettyArc :: DepArc String -> String
prettyArc (Arc tail head label) = concat ["{", intercalate ", " $ Set.toList tail, "}", "-", prettyDet label, "->", head]

prettyVars :: Hypergraph Integer l -> Hypergraph String l
prettyVars d@(Hypergraph { nodes = n}) | length n <= length varnames = hypergraphmap (\i -> varnames !! fromInteger i) d
                                       | otherwise = hypergraphmap show d
prettyGraph' :: DepRep String -> String
prettyGraph' (Hypergraph { groundNodes = g, arcs = a }) = concat ["<", intercalate ", " $ Set.toList g, ">[", intercalate ", " $ map prettyArc a, "]"]

prettyGraph :: (Show n) => DepRep n -> String
prettyGraph = prettyGraph' . hypergraphmap show

prettyVarGraph :: DepRep Integer -> String
prettyVarGraph = prettyGraph' . prettyVars

isSatisfied :: (Ord n) => Set.Set n -> Hyperarc n l -> Bool
isSatisfied n e = tail e `Set.isSubsetOf` n

type Connection n l = [Hyperarc n l]

loops :: (Ord n) => Hypergraph n l -> n -> [Hyperarc n l]
loops g v = filter (\e -> v `Set.member` tail e && head e == v) (arcs g)

findConnections :: (Ord n) => Hypergraph n DetLabel -> Set.Set n -> n -> [Connection n DetLabel]
findConnections g@(Hypergraph nodes _ arcs) s n
    | n `Set.member` s = [] : map return (loops g n)
    | otherwise = concatMap (\a -> map (a:) $ findConnections g (Set.insert (head a) s) n) availableArcs
    where availableArcs = filter (\a -> head a `Set.notMember` s && isSatisfied s a) arcs

connectionLabel :: (Ord al, Bounded al) => Connection n al -> al
connectionLabel c = lub $ map arcLabel c

lub :: (Ord al, Bounded al) => [al] -> al
lub [] = minBound
lub c = maximum c

glb :: (Ord al, Bounded al) => [al] -> al
glb [] = maxBound
glb c = minimum c

relevantDependence :: (Ord n) => DepRep n -> Set.Set n -> n -> Maybe DetLabel
relevantDependence g s n = case findConnections g s n of
    [] -> Nothing
    l -> Just $ glb $ filter (/= Bot) $ map connectionLabel l

isRedundant :: (Ord n) => DepRep n -> DepArc n -> Bool
isRedundant _ (Arc tail head Nondet) | head `Set.member` tail = True
isRedundant g e@(Arc tail head l) = case relevantDependence (g {arcs = List.delete e (arcs g)}) tail head of
    Nothing -> False
    Just l' -> l' <= l


clearRedundant :: (Ord n) => DepRep n -> DepRep n
clearRedundant g = foldl (\g' e -> if isRedundant g' e then g' {arcs = List.delete e (arcs g')} else g') g (arcs g)

clearUnimportant :: (Ord n) => DepRep n -> DepRep n
clearUnimportant g = g { arcs = filter (\e -> arcLabel e < Nondet || head e `Set.notMember` groundNodes g) (arcs g) }

isRedundantTail :: (Ord n) => DepRep n -> Set.Set n -> n -> DetLabel -> Bool
isRedundantTail g t i l = case relevantDependence g (Set.delete i t) i of
    Nothing -> False
    Just l' -> l' <= l

simplifyArc :: (Ord n) => DepRep n -> DepArc n -> DepArc n
simplifyArc g (Arc t h l) = Arc (foldl (\t' i -> if isRedundantTail g t' i l then Set.delete i t' else t') t t) h l

simplifyArcs :: (Ord n) => DepRep n -> DepRep n
simplifyArcs g = g { arcs = map (simplifyArc g) (arcs g) }

simplify :: (Ord n) => DepRep n -> DepRep n
simplify = clearRedundant . simplifyArcs

arcmap :: (Ord b) => (a -> b) -> Hyperarc a l -> Hyperarc b l
arcmap f a = a { tail = Set.mapMonotonic f (tail a), head = f (head a) }

hypergraphmap :: (Ord b) => (a -> b) -> Hypergraph a l -> Hypergraph b l
hypergraphmap f (Hypergraph nodes ground arcs) = Hypergraph (f <$> nodes) (Set.mapMonotonic f ground) (arcmap f <$> arcs)

type Predicate = String
type Constructor = String

data Atom v = Atom Predicate [Term v] deriving (Eq, Show, Functor)
data AbstractAtom = AAtom Predicate (DepRep Integer) deriving (Eq, Show)

type Rule v = (Atom v, [Atom v])
type Program v = [Rule v]

vars :: (Eq v) => Term v -> [v]
vars (V v) = [v]
vars (C _ ts) = List.nub $ ts >>= vars

groundIdx :: (Eq v) => Atom v -> [Integer]
groundIdx (Atom _ ts) = groundIdx' ts
    where
        groundIdx' [] = []
        groundIdx' (t:ts) | null (vars t) = 0 : rest
                          | otherwise = rest
                          where rest = map (+1) (groundIdx' ts)

data MguNodes v = PosNode Integer Integer | VarNode v | GroundNode Integer Integer deriving (Eq, Show, Ord)

imap :: (Integer -> t -> a) -> [t] -> [a]
imap = imap' 0
    where
        imap' _ _ [] = []
        imap' n f (x:xs) = f n x : imap' (n+1) f xs

concatImap :: (Integer -> t -> [a]) -> [t] -> [a]
concatImap f = concatMap (uncurry f) . imap (,)

iconcat :: [[v]] -> [(Integer, v)]
iconcat = concatImap (\i x -> (i,) <$> x)

mguDep :: (Ord v) => [(Atom v, AbstractAtom)] -> Maybe (DepRep (MguNodes v))
mguDep atoms = do
    guard (all (\(Atom p ts, AAtom p' ds) -> p == p' && length ts == length (nodes ds)) atoms)
    let gid = iconcat $ groundIdx . fst <$> atoms
    let n = concat [
            concatImap (\i (_, AAtom _ ds) -> PosNode i <$> nodes ds) atoms,
            map VarNode (List.nub $ concatMap (\(Atom _ ts, _) -> ts >>= vars) atoms),
            map (uncurry GroundNode) gid
            ]
    let v = Set.unions (imap (\i (_, AAtom _ ds) -> Set.mapMonotonic (PosNode i) $ groundNodes ds) atoms) `Set.union` Set.fromList (map (uncurry GroundNode) gid)
    let e = concat [
                concatImap (\i (_, AAtom _ ds) -> map (\a -> a { tail = Set.mapMonotonic (PosNode i) (tail a), head = PosNode i (head a) }) (arcs ds)) atoms,
                [Arc {tail = Set.singleton (GroundNode i j), head = PosNode i j, arcLabel = Bot} | (i, j) <- gid],
                [Arc {tail = Set.singleton (PosNode i j), head = GroundNode i j, arcLabel = Bot} | (i, j) <- gid],
                concatImap (\i (Atom _ ts, _) -> imap (\j t -> Arc {tail = Set.fromList (map VarNode $ vars t), head = PosNode i j, arcLabel = Bot }) ts) atoms,
                concatImap (\i (Atom _ ts, _) -> concatImap (\j t -> map (\v -> Arc {tail = Set.singleton (PosNode i j), head = VarNode v, arcLabel = Bot }) (vars t)) ts) atoms
            ]
    return $ Hypergraph n v e


propagateGroundEdge :: (Ord v) => DepRep v -> DepArc v -> DepRep v
propagateGroundEdge g@(Hypergraph { groundNodes = v }) (Arc { tail = t, head = h }) | t `Set.isSubsetOf` v = g { groundNodes = Set.insert h v }
                                                                                    | otherwise = g

simpleFixpoint :: (Eq x) => (x -> x) -> x -> x
simpleFixpoint f x = let x' = f x in if x' == x then x' else simpleFixpoint f x'

propagateGround :: (Ord v) => DepRep v -> DepRep v
propagateGround = simpleFixpoint (\g -> foldl propagateGroundEdge g (arcs g))

isVarNode (VarNode _) = True
isVarNode _ = False

extractVarsList :: [MguNodes v] -> [v]
extractVarsList = map (\(VarNode v) -> v) . filter isVarNode

extractVarsSet :: Set.Set (MguNodes v) -> Set.Set v
extractVarsSet = Set.mapMonotonic (\(VarNode v) -> v) . Set.filter isVarNode

mguSubst :: (Ord v) => DepRep (MguNodes v) -> AbstractSubst v
mguSubst d = let vs = extractVarsList (nodes d) in Hypergraph {
    nodes = vs,
    groundNodes = extractVarsSet $ groundNodes d,
    arcs = catMaybes [Arc t x <$> relevantDependence d (Set.mapMonotonic VarNode t) (VarNode x) | t <- Set.fromList <$> filter (/= []) (subsequences vs), x <- vs]
}

alphaMgu :: (Ord v) => [(Atom v, AbstractAtom)] -> Maybe (AbstractSubst v)
alphaMgu atoms = mguSubst . propagateGround <$> mguDep atoms


data ApplyNodes v = SubstNode v | TermNode Integer v deriving (Eq, Show, Ord, Functor)

termNodes :: (Eq v) => Atom v -> [ApplyNodes v]
termNodes (Atom p ts) = concatImap (\j t -> TermNode j <$> vars t) ts

isTermNode :: ApplyNodes v -> Bool
isTermNode (TermNode _ _) = True
isTermNode _ = False

applyDep :: (Ord v) => (Atom v, AbstractSubst v) -> DepRep (ApplyNodes v)
applyDep (a, s) = let
    tn = termNodes a
    n = (SubstNode <$> nodes s) ++ tn
    v = Set.mapMonotonic SubstNode $ groundNodes s
    e = (arcmap SubstNode <$> arcs s) ++ concat [let t' = SubstNode x in [Arc (Set.singleton t) t' Bot, Arc (Set.singleton t') t Bot] | t@(TermNode _ x) <- tn, x `elem` nodes s]
    in Hypergraph n v e

termApplyNodes :: (Ord v) => DepRep (ApplyNodes v) -> Integer -> [ApplyNodes v]
termApplyNodes d j = [t | t@(TermNode j' v) <- nodes d, j == j']

freeApplyVars :: (Ord v) => DepRep (ApplyNodes v) -> Integer -> Set.Set v
freeApplyVars d j = Set.fromList [v | t@(TermNode j' v) <- nodes d, j == j', not $ any (\e -> head e == t) (arcs d)]

linkedApplyNodes :: (Ord v) => DepRep (ApplyNodes v) -> Integer -> [ApplyNodes v]
linkedApplyNodes d j = [t | t@(TermNode j' v) <- nodes d, j == j', any (\e -> head e == t) (arcs d)]

isGroundedTermVar :: (Ord v) => Integer -> (Atom v, DepRep (ApplyNodes v)) -> Bool
isGroundedTermVar j (atom, d) = Set.fromList (filter (\(TermNode j' _) -> j == j') $ termNodes atom) `Set.isSubsetOf` groundNodes d


mgu :: (Ord v) => Term v -> Term v -> Maybe (Subst (Bool, v))
mgu a b = unify (Just Subst.empty) ((False,) <$> a) ((True,) <$> b)


pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = ((x,) <$> xs) ++ pairs xs


takeSequence :: [Integer] -> [a] -> [a]
takeSequence [] _ = []
takeSequence _ [] = []
takeSequence (i:is) (x:xs) | i == 0 = x : rest is
                           | otherwise = rest (i:is)
                           where rest ii = takeSequence (map (subtract 1) ii) xs

collectPaths [] = Det
collectPaths xs = lub xs

applyConnections :: (Show v, Ord v) => [(Atom v, AbstractSubst v)] -> [DepRep (ApplyNodes v)] -> [Integer] -> [Hyperarc Integer DetLabel]
applyConnections atoms d n = do
    t <- filter (/= []) (subsequences n)
    h <- n

    guard $ all (\g -> freeApplyVars g h `Set.isSubsetOf` Set.unions (freeApplyVars g <$> t)) d

    let paths = sequence [relevantDependence g (Set.unions (Set.fromList . termApplyNodes g <$> t)) h' | g <- d, h' <- linkedApplyNodes g h]
    guard $ isJust paths
    let (Just l') = collectPaths <$> paths
    let l = if any (\(Atom p ts, Atom p' ts') -> isJust (mgu (C p (takeSequence t ts)) (C p' (takeSequence t ts')))) (pairs (map fst atoms))
            then Nondet else l'
    return $ Arc (Set.fromList t) h l

applySinglePred :: (Show v, Ord v) => [(Atom v, AbstractSubst v)] -> AbstractAtom
applySinglePred [] = error "Empty alpha-apply"
applySinglePred atoms@((Atom p ts,_):_) =
    let d = propagateGround . applyDep <$> atoms in
    let n = imap const ts in
    let v = Set.fromList (filter (\j -> all (isGroundedTermVar j) $ zip (map fst atoms) d) n) in
    let e = applyConnections atoms d n in
    AAtom p $ simplify $ Hypergraph n v e

groupByPred :: [(Atom v, AbstractSubst v)] -> Map.Map Predicate [(Atom v, AbstractSubst v)]
groupByPred atoms = Map.fromList $ (\v@((Atom p _, _):_) -> (p,v)) <$> List.groupBy (\(Atom p _, _) (Atom p' _, _) -> p == p') atoms

alphaApply :: (Show v, Ord v) => [(Atom v, AbstractSubst v)] -> Map.Map Predicate AbstractAtom
alphaApply atoms = let s = groupByPred atoms in Map.map applySinglePred s

alphaConsequence :: (Show v, Ord v) => Program v -> Map.Map Predicate AbstractAtom -> Map.Map Predicate AbstractAtom
alphaConsequence prog interp = trace "iter" $ alphaApply $ catMaybes $ do
    (h, bs) <- prog
    return $ do
        bs' <- mapM (\(Atom p _) -> Map.lookup p interp) bs
        s <- alphaMgu (zip bs bs')
        return (h, s)

alphaInterp' :: (Show v, Ord v) => Program v -> Map.Map Predicate AbstractAtom
alphaInterp' prog = simpleFixpoint (alphaConsequence prog) Map.empty

alphaInterp :: (Show v, Ord v) => Program v -> Map.Map Predicate AbstractAtom
alphaInterp prog = Map.map (\(AAtom p d) -> AAtom p (clearUnimportant d)) $ alphaInterp' prog

showDependence :: AbstractAtom -> String
showDependence (AAtom p d) = let d' = prettyVars d in concat [p, "(", intercalate "," (nodes d'), "): ", prettyGraph' d']

showDependences :: Map.Map Predicate AbstractAtom -> String
showDependences = unlines . map showDependence . Map.elems

sumlistExample = [
    (Atom "plus" [V "X", C "0" [], V "X"], []),
    (Atom "plus" [V "X", C "S" [V "Y"], C "S" [V "Z"]], [Atom "plus" [V "X", V "Y", V "Z"]]),
    (Atom "sumlist" [C "[]" [], C "0" []], []),
    (Atom "sumlist" [C ":" [V "X", V "Xs"], V "S"], [Atom "sumlist" [V "Xs", V "Ss"], Atom "plus" [V "X", V "Ss", V "S"]]),
    (Atom "select" [V "X", C ":" [V "X", V "Xs"], V "Xs"], []),
    (Atom "select" [V "X", C ":" [V "Y", V "Ys"], C ":" [V "Y", V "Zs"]], [Atom "select" [V "X", V "Ys", V "Zs"]]),
    (Atom "check" [V "X", V "Y", V "R"], [Atom "select" [V "Y", V "X", V "R"], Atom "sumlist" [V "X", V "Y"]])
    ]

appendExample = [
    (Atom "append" [C "[]" [], V "X", V "X"], []),
    (Atom "append" [C ":" [V "X", V "Xs"], V "Y", C ":" [V "X", V "Ys"]], [Atom "append" [V "Xs", V "Y", V "Ys"]])
    ]

appendApplyTest :: [(Atom String, AbstractSubst String)]
appendApplyTest = [
    (Atom "append" [C "[]" [], V "X", V "X"], Hypergraph [] Set.empty []),
    (Atom "append" [C ":" [V "X", V "Y"], V "Z", C ":" [V "X", V "U"]],
     Hypergraph ["Y", "Z", "U"] (Set.singleton "Y") [Arc (Set.fromList ["Y"]) "Y" Det, Arc (Set.fromList ["Z"]) "U" Det, Arc (Set.fromList ["U"]) "Z" Det])]

notExample = [
    (Atom "not" [C "true" [], C "false" []], []),
    (Atom "not" [C "false" [], C "true" []], [])
    ]

pureNot = [
    (Atom "not" [C "true" [], C "false" []], Hypergraph [] Set.empty []),
    (Atom "not" [C "false" [], C "true" []], Hypergraph [] Set.empty [])
    ]

test = let (Just x) = Map.lookup "check" (alphaInterp sumlistExample) in x

-- a1 = Map.fromList [("check",AAtom "check" (Hypergraph {nodes = [0,1,2], groundNodes = Set.fromList [], arcs = [Arc {tail = Set.fromList [0], head = 1, arcLabel = Det},Arc {tail = Set.fromList [1], head = 2, arcLabel = Nondet},Arc {tail = Set.fromList [2], head = 0, arcLabel = Det}]})),("plus",AAtom "plus" (Hypergraph {nodes = [0,1,2], groundNodes = Set.fromList [1], arcs = [Arc {tail = Set.fromList [0], head = 2, arcLabel = Nondet},Arc {tail = Set.fromList [0,1], head = 2, arcLabel = Det},Arc {tail = Set.fromList [2], head = 1, arcLabel = Nondet},Arc {tail = Set.fromList [1,2], head = 0, arcLabel = Nondet}]})),("select",AAtom "select" (Hypergraph {nodes = [0,1,2], groundNodes = Set.fromList [], arcs = [Arc {tail = Set.fromList [1], head = 2, arcLabel = Nondet},Arc {tail = Set.fromList [0,2], head = 1, arcLabel = Nondet},Arc {tail = Set.fromList [1,2], head = 0, arcLabel = Nondet}]})),("sumlist",AAtom "sumlist" (Hypergraph {nodes = [0,1], groundNodes = Set.fromList [], arcs = [Arc {tail = Set.fromList [0], head = 1, arcLabel = Det},Arc {tail = Set.fromList [1], head = 0, arcLabel = Nondet}]}))]
-- a2 = alphaConsequence [(Atom "check" [V "X", V "Y", V "R"], [Atom "select" [V "Y", V "X", V "R"], Atom "sumlist" [V "X", V "Y"]])] a1
-- test = let (Just (AAtom "check" g)) = Map.lookup "check" a2 in length (arcs g)

unifyAtom = (Atom "==" [V "X", V "X"], [])
