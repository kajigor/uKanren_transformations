{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FinalTagless.Deep where

import FinalTagless.GoalSyntax
import qualified Syntax

newtype Deep v = Deep { unDeep :: Syntax.G v }

instance Goal v Deep where
  unif x y = Deep $ (Syntax.:=:) x y
  conj x y xs = Deep $ Syntax.Conjunction (unDeep x) (unDeep y) (map unDeep xs)
  disj x y xs = Deep $ Syntax.Disjunction (unDeep x) (unDeep y) (map unDeep xs)
  fresh x g = Deep $ Syntax.Fresh x (unDeep g)
  call n args = Deep $ Syntax.Invoke n args



program3 :: Syntax.Program
program3 = Syntax.Program [] (unDeep g3)

program1 :: Syntax.Program
program1 = Syntax.Program [Syntax.Def "appendo" ["x", "y", "z"] (unDeep g1)] (unDeep g2)
