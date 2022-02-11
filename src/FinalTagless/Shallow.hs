{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
module FinalTagless.Shallow where

import FinalTagless.GoalSyntax
import qualified Syntax

-- import qualified Subst
-- import qualified Stream
-- import qualified FreshNames as FN
-- import Control.Monad.State
-- import Stream
-- import Eval (unify)
-- import FinalTagless.View
-- import qualified Data.Map as Map
-- import qualified VarInterpretation as VI

-- data Def goalRepr = Goal Syntax.X goalRepr =>
--                     Def Syntax.Name [Syntax.Name] (goalRepr Syntax.X)

-- data Program goalRepr = Goal Syntax.X goalRepr =>
--                         Program [Def goalRepr] (goalRepr Syntax.X)

-- newtype Definitions goalRepr = Definitions { getDefinitions :: Goal Syntax.X goalRepr => Map.Map Syntax.Name (Def goalRepr) }

-- data Env goalRepr = Env { getDefs :: Definitions goalRepr
--                         , getInterp :: VI.Interpretation
--                         , getFreshNames :: FN.FreshNames
--                         }

-- def1 :: Goal Syntax.X goalRepr => Def goalRepr
-- def1 = Def "appendo" ["x", "y", "z"] g1

-- program1 :: Goal Syntax.X goalRepr => Program goalRepr
-- program1 = Program [def1]
--                    (manyFresh ["x", "y"] $ call "appendo" [var "x", var "y", var "y"])

-- newtype Shallow v = Shallow {
--   unShallow :: State (Subst.Subst, Env) (Stream.Stream Subst.Subst) }

-- instance Goal (Syntax.S) Shallow where
--   unif t1 t2 = Shallow $ do
--     (s, _) <- get
--     return $ maybeToStream $ unify (Just s) t1 t2
--   call name args = Shallow $ do
--     (s, env) <- get
--     let defs = getDefinitions (getDefs env)
--     let Def _ fs body = defs Map.! name
--     undefined


-- shallowEval = unShallow


-- g4S :: (Goal Syntax.S goalRepr) => goalRepr Syntax.S
-- g4S = (unif (con "nil" []) (con "cons" [var 1, con "nil" []]))


-- showStuff = unView g4S

-- -- class Goal v goalRepr where
-- --   unif :: Syntax.Term v -> Syntax.Term v -> goalRepr v
-- --   conj :: goalRepr v -> goalRepr v -> [goalRepr v] -> goalRepr v
-- --   disj :: goalRepr v -> goalRepr v -> [goalRepr v] -> goalRepr v
-- --   fresh :: String -> goalRepr v -> goalRepr v
-- --   call :: String -> [Syntax.Term v] -> goalRepr v

-- --   safeConj :: goalRepr v -> [goalRepr v] -> goalRepr v
-- --   safeConj x [] = x
-- --   safeConj x (y:xs) = conj x y xs

-- --   safeDisj :: goalRepr v -> [goalRepr v] -> goalRepr v
-- --   safeDisj x [] = x
-- --   safeDisj x (y:xs) = disj x y xs

-- --   manyFresh :: [String] -> goalRepr v -> goalRepr v
-- --   manyFresh names goal = foldr fresh goal names
