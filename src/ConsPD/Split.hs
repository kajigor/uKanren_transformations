{-# LANGUAGE TupleSections #-}
module ConsPD.Split where

import Syntax
import Generalization
import Data.Maybe (isNothing, isJust, fromJust, listToMaybe)
import qualified Embed 
import Data.Function (on)
import Data.List (sortBy, intercalate, intersect, nub)
import Debug.Trace (trace, traceShow)
import qualified FreshNames as FN
import Control.Monad.State (State, get, runState)
import qualified Subst
import Text.Printf (printf)
import qualified Environment as Env

-- I don't see why we should check for strict embedding. 
embedding :: Embed.Embed a b => b -> b -> Bool
embedding bigger smaller = Embed.embed smaller bigger -- && not (Embed.isVariant smaller bigger) 

whistle :: [[G S]] -> [G S] -> Maybe [G S]
whistle seen bigger =
  let candidates = filter (embedding bigger) seen in 
  let sorted = sortBy (flip compare `on` length) candidates in -- longer conjunctions come first
  listToMaybe sorted

runSplit seen goal = runState (split seen goal) FN.defaultNames

abstractChild :: [[G S]] -> (Subst.Subst S, [G S], Env.Env) -> State FN.FreshNames [(Subst.Subst S, [G S], Generalizer, Env.Env)]
abstractChild seen (subst, goal, env) = do 
  res <- split seen goal
  fn <- get 
  let env' = Env.updateNames env fn 
  return $ map (\(goal, gen) -> (subst, goal, gen, env')) res 

mcs :: (Eq a, Show a) => [G a] -> [[G a]]
mcs []     = []
mcs [g]    = [[g]]
mcs (g:gs) =
  let (con, non, _) =
        foldl (\(con, non, vs) x -> if null (vs `intersect` vars x)
                                    then (con, x : non, vs)
                                    else (x : con, non, nub $ vars x ++ vs))
              ([g], [], vars g)
              gs
  in  reverse con : mcs (reverse non)

vars :: (Eq a, Show a) => G a -> [Term a]
vars (Invoke _ args) =
  nub $ concatMap getVars args where
    getVars (V v)    = [V v]
    getVars (C _ ts) = concatMap getVars ts
vars _ = []

-- splits a goal and generalizes the results
split :: [[G S]] -> [G S] -> State FN.FreshNames [([G S], Generalizer)]
split seen goal = 
    case go seen goal [] of 
      Just ([], outs) -> return $ map (, Subst.empty) $ mcs goal -- split by common vars if there is no embedding; 
      Just (ins, outs) -> transform ins outs 
      Nothing -> error $ printf "Splitting failed miserably\nGoal:\n%s\n\nSeen:\n%s\n" (show goal) (intercalate "\n" $ map show seen)
  where 
    transform ins outs = do 
      generalized <- map (\(g, _, gen) -> (g, gen)) <$> mapM (uncurry generalizeGoalsState) ins 
      return (generalized ++ map (, Subst.empty) outs)

nil = C "nil" [] 
cons h t = C "cons" [h, t]

z = C "z" []
s x = C "s" [x] 

a = Invoke "a" []

f x = Invoke "f" [x]
g x y = Invoke "g" [x, y]

h x y z = Invoke "h" [x, y, z]


seen = [[g z (s z)], [f z, f (s z), h z z z]]

goal = [h z z z, g (s z) (s z), f z, g (s z) z, g z (s z), f (s (s z)), f z, h (s z) z (s z), g z z]

go :: [[G S]] -> [G S] -> [([G S], [G S])] -> Maybe ([([G S], [G S])], [[G S]])
go seen goal inss =   
  case whistle seen goal of 
    Just emb -> do 
      (ins, outs) <- siftElements (flip embedding) emb goal
      let outs' = concat outs 
      if goal == outs' 
      then return (reverse (ins ++ inss), outs)
      else go seen (concat outs) (ins ++ inss) 
    Nothing -> return (reverse inss, map (:[]) goal)

-- here Nothing serves as a boundary between consequitive lists of elements 
-- [Nothing, Nothing, Just 1, Just 2, Nothing, Just 3, Nothing, Just 4] -> [[1,2],[3],[4]]
listOfMaybesToLists :: [Maybe a] -> [[a]]
listOfMaybesToLists xs = 
    go (dropNothing xs) 
  where 
    dropNothing = dropWhile isNothing
    go [] = [] 
    go xs = 
      let (justs, rest) = span isJust xs in 
      map fromJust justs : go (dropNothing rest)

-- Here the goal is to find all instances of the first list within the last one
-- Gaps are allowed, but they must result in splitting of the list accordingly
-- ghci> siftElements (==) [1,2] [1,2]
-- Just ([[1, 2]], [])
-- ghci> siftElements (==) [1,2] [2,1]
-- Just ([], [[2, 1]])
-- ghci> siftElements (==) [1,2] [3,1,4,2,5]
-- Just ([[1, 2]], [[3], [4], [5]])
-- siftElements (==) [1,2] [0,2, 1, 0,2, 1, 2, 1, 0, 1, 2, 0]
-- Just ([[1, 2], [1, 2], [1, 2]], [[0, 2], [0], [0, 1], [0]])
siftElements :: (a -> b -> Bool) -> [a] -> [b] -> Maybe ([([a], [b])], [[b]])
siftElements p xs ys = do 
    go p xs (map Just ys) [] 
  where 
    go p xs ys inss = do 
      case siftElementsOnce p xs ys ([], []) of 
        Just (ins, outs) -> go p xs (reverse outs) ((xs, reverse ins) : inss)
        Nothing -> return (reverse inss, listOfMaybesToLists ys)

    siftElementsOnce :: (a -> b -> Bool) -> [a] -> [Maybe b] -> ([b], [Maybe b]) -> Maybe ([b], [Maybe b])
    siftElementsOnce p [] ys (ins, outs) = Just (ins, reverse ys ++ outs)
    siftElementsOnce _ _ [] _ = Nothing 
    siftElementsOnce p (x:xs) (y:ys) (ins, outs) = 
      case y of 
        Just y' | p x y' -> siftElementsOnce p xs ys (y':ins, Nothing:outs)
        _ -> siftElementsOnce p (x:xs) ys (ins, y:outs)


-- f :: Eq a => (a -> a -> Bool) -> [a] -> [[a]] -> Maybe [[a]]
-- f p xs ys =
--   case splitIntoGroups p xs (concat ys) of
--     Just (ins, out) -> (ins :) <$> f p xs out
--     _ -> return ys

-- splitIntoGroups :: (a -> a -> Bool) -> [a] -> [a] -> Maybe ([a], [[a]])
-- splitIntoGroups p xs ys = do
--   (ins, out) <- go p xs ys ([], [])
--   return (reverse ins, reverse out)

-- -- go p x y acc: x <= y
-- go :: (a -> a -> Bool) -> [a] -> [a] -> ([a], [[a]]) -> Maybe ([a], [[a]])
-- go _ _ [] _ = Nothing
-- go p [] ys (ins, out) = Just (ins, ys : out)
-- go p (x:xs) (y:ys) (ins, out)
--   | p x y = go p xs ys (x : ins, out)
--   | otherwise =
--       let (newOut, ys') = break (p x) ys in
--       go p (x : xs) ys' (ins, (y : newOut) : out)

-- -- go p x y acc: x <= y
-- go :: (a -> a -> Bool) -> [a] -> [Maybe a] -> ([a], [Maybe a]) -> Maybe ([a], [[a]])
-- go p [] ys (ins, out) = Just (ins, ys : out)
-- go p (x : xs) (y : ys) (ins, out)
--   | p x y = go p xs ys (x : ins, out)
--   | otherwise =
--       let (newOut, ys') = break (p x) ys
--        in go p (x : xs) ys' (ins, (y : newOut) : out)
