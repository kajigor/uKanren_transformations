{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module ConsPD.GlobalControl where

import           Control.Monad.State
import qualified ConsPD.LocalControl as LC
import ConsPD.State

import           Data.List           (intercalate, sortBy)
import qualified Data.Set            as Set

import           Descend
import           Embed
import qualified Environment         as Env
import qualified Eval                as E
import qualified FreshNames          as FN
import           Generalization      (Generalizer)
import           Prelude             hiding (sequence)
import           Program
import qualified Subst
import           Syntax
-- import qualified Data.IntMap as Set
import Debug.Trace
import Util.Miscellaneous (snd4)
import Data.Function (on)
import Data.Maybe (listToMaybe)
import qualified ConsPD.Split as Split
import Environment (Env(getFreshNames))

data GlobalTree = Fail 
                | Success (Subst.Subst S)
                | Leaf  (Descend [G S]) Generalizer (Subst.Subst S)
                | Node  (Descend [G S]) Generalizer (Subst.Subst S) LC.SldTree [GlobalTree]
                | Split [G S] [GlobalTree] (Subst.Subst S)
                | Transient (Descend [G S]) GlobalTree (Subst.Subst S) (Maybe LC.SldTree) 
                | Prune (Descend [G S]) (Subst.Subst S)
                deriving (Show)

restrictSubsts :: GlobalTree -> GlobalTree
restrictSubsts =
    go Subst.empty
  where
    go _ Fail = Fail 
    go subst (Success s) = Success (Subst.difference s subst)
    go subst (Split gs ch s) = Split gs (map (go s) ch) (Subst.difference s subst)
    go subst (Node gs gen s sldTree ch) = Node gs gen (Subst.difference s subst) sldTree (map (go s) ch)
    go subst (Leaf gs gen s) = Leaf gs gen (Subst.difference s subst)
    go subst (Prune gs s) = Prune gs (Subst.difference s subst)
    go subst (Transient gs ch s sld) = Transient gs (go s ch) (Subst.difference s subst) sld 

sequence :: Descend a -> [a]
sequence = getAncs

collectNodes :: GlobalTree -> [([G S], GlobalTree)]
collectNodes node@(Node _ _ _ _ []) = []
collectNodes node@(Node d _ _ _ ch) = (getCurr d, node) : concatMap collectNodes ch
collectNodes node@(Transient d ch _ _) = (getCurr d, node) : collectNodes ch
collectNodes (Split _ ch _) = concatMap collectNodes ch
collectNodes _ = []

getNodes :: GlobalTree -> [([G S], LC.SldTree)]
getNodes (Node _ _ _ (LC.Leaf _ _ _) _) = [] -- This happens because of the instance check
getNodes (Node d _ _ sld ch) = (getCurr d, sld) : concatMap getNodes ch
getNodes (Split _ ch _) = concatMap getNodes ch
getNodes (Transient d ch _ (Just sld)) = (getCurr d, sld) : getNodes ch
getNodes (Transient d ch _ _) = getNodes ch
getNodes _ = []

-- initial splitting into maximally connected suconjunctions, may be something else
part :: [G S] -> [[G S]]
part = LC.mcs

abstract :: [[G S]] -> [G S] -> State FN.FreshNames [([G S], Generalizer)]
abstract seen goals =
    let qCurly = part goals in
    -- trace "Abstract; qCurly" $ traceShow qCurly $ 
    go (map (, Subst.empty) qCurly)
  where
    go [] = return []
    go inp@((m, gen):gs) = -- | trace "Go (m, gen):gs" $ traceShow inp $ True =
      case whistle seen m of
        Nothing -> do
          goals <- go gs
          return ((m, gen) : goals)
        Just b -> do
          (goals, m2) <- generalize' m b
          blah <- go ((if null m2 then ((m2, gen) :) else id) gs)
          return (goals ++ blah)

whistle :: [[G S]] -> [G S] -> Maybe [G S]
whistle seen m =
--   find (\b -> Embed.embed b m && not (Embed.isVariant b m)) (sequence descend)
  let candidates = filter (\b -> Embed.embed b m && not (Embed.isVariant b m)) seen in
  let sorted = sortBy (flip compare `on` length) candidates in -- longer conjunctions come first
  listToMaybe sorted

generalize' :: [G S] -> [G S] -> State FN.FreshNames ([([G S], Generalizer)], [G S])
generalize' m b = do
  ((m1, m2), genOrig) <- LC.split b m
  return $ (map (,genOrig) (LC.mcs m1), m2)

generalize :: [G S] -> [G S] -> State FN.FreshNames [([G S], Generalizer)]
generalize m b = do
    ((m1, m2), genOrig) <- LC.split b m
    return $ map (, genOrig) (LC.mcs m1) ++ map (,Subst.empty) (LC.mcs m2)

abstractChild :: [[G S]] -> (Subst.Subst S, [G S], Env.Env) -> State FN.FreshNames [(Subst.Subst S, [G S], Generalizer, Env.Env)]
-- abstractChild ancs (subst, goals, env) | length goals > 2 = do
--     concat <$> mapM (\g -> abstractChild ancs (subst, [g], env)) goals 
abstractChild seen (subst, g, env) = do
    put (Env.getFreshNames env)
    abstracted <- abstract seen g
    d' <- get
    res <- mapM (\(g, gen) -> go (subst, g, gen, Env.updateNames env d')) abstracted
    return $ -- trace "=================================\nAbstractChild\ngoal" $ traceShow g $ trace "Seen" $ trace (intercalate "\n" $ map show seen) $ trace "Res" $ trace (intercalate "\n" $ map (show . snd4) $ concat res) $ trace "=================================\n"
             concat res 
  where 
    go :: (Subst.Subst S, [G S], Generalizer, Env.Env) -> State FN.FreshNames [(Subst.Subst S, [G S], Generalizer, Env.Env)]
    go (subst, g, gen, env) | Subst.null gen = do
      put (Env.getFreshNames env)
      abstracted <- abstract seen g 
      d' <- get 
      return $ map (\(g, gen) -> (subst, g, gen, Env.updateNames env d')) abstracted 
    go x = return [x]



topLevel :: Program G X -> LC.Heuristic -> (GlobalTree, G S, [S])
topLevel (Program defs goal) heuristic =
    let env = Env.fromDefs defs in
    let ((logicGoal, names), env') = runState (E.preEval goal) env in
    let tree = evalState (go (Descend.init [logicGoal]) Subst.empty Subst.empty) (ConsPD.State.init env') in
    (tree, logicGoal, names)
  where
    go :: Descend.Descend [G S] -> Subst.Subst S -> Generalizer -> State ConsPDState GlobalTree
    go d@(Descend.Descend goal ancs) subst generalizer = do
        env <- gets getEnv
        -- if fst (FN.getFreshName (Env.getFreshNames env)) > 1000
        -- then return $ Prune d subst
        -- else
        do
          seen <- gets getSeen
          sldTree <- LC.sldResolution goal subst heuristic
          modifySeen (const $ Set.insert goal seen)
          let d' = Descend.add goal d
          let resultants = LC.resultants sldTree
          if null resultants 
          then 
            return Fail 
          else do 
            children <- mapM (split d') (LC.resultants sldTree)
            case -- trace "\n=======================\nResultants: " $ trace (intercalate "\n" $ map show resultants) $ 
                -- trace "children: " $ trace (intercalate "\n" $ map show children) $ 
                -- trace "\n=======================\n" $
                children of
              [x] -> return $ Transient d x subst (Just sldTree)
              _ -> return $ Node d generalizer subst sldTree children
      where
        split :: Descend [G S] -> (Subst.Subst S, [G S], Maybe Env.Env) -> State ConsPDState GlobalTree
        split _ (subst', [], _) =
          return $ Success subst' -- (Subst.difference subst' subst)
        split descend (subst, gs, Just env) = do
            seen <- gets getSeen
            let ancs = getAncs descend
            -- let (abstracted, freshNames) = runState (Split.abstractChild (Set.toList seen) (subst, gs, env)) (Env.getFreshNames env)
            let (abstracted, freshNames) = runState (Split.abstractChild ancs (subst, gs, env)) (Env.getFreshNames env)
            modifyEnv (`Env.updateNames` freshNames)
            children <- mapM processChild abstracted
            case children of
              [x] -> return $ Transient descend x subst Nothing 
              _ -> return $ Split gs children subst
          where
            processChild abstracted@(subst', goals, generalizer, env) = do
              seen <- gets getSeen
              -- modifyEnv (const env) -- make sure environment is updated at every abstractChild call
              if null goals || any (`Embed.isInst` goals) seen
              then return $ Leaf (Descend.add goals descend) generalizer subst'
              else do
                tree <- do go (Descend.add goals descend) subst' generalizer -- Check if descend is correct
                modifySeen (Set.insert goals)
                return tree


    forgetEnv = map (\(x, y, _) -> (x, y, Subst.empty))
    forgetStuff = map (\(x, y, gen, _) -> (x, y, gen))
