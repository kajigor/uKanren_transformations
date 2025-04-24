{-# LANGUAGE TypeFamilies #-}
module Test.Shallow.Types.Tree(Tree(..), leaf, node) where

import Shallow.Core.Internal.Logic
import Shallow.Utils.Type
import Control.Applicative (liftA3)

data Tree elem = Leaf | Node (Tree elem) elem (Tree elem)

leaf :: Logic (Tree elem) var
leaf = Ground Leaf'

node :: Logic (Tree elem) var -> Logic elem var -> Logic (Tree elem) var -> Logic (Tree elem) var
node l x r = Ground $ Node' l x r

leaf' :: Reified (Tree elem) var
leaf' = Leaf'

node' :: Reified (Tree elem) var -> Reified elem var -> Reified (Tree elem) var -> Reified (Tree elem) var
node' l x r = Node' (Ground l) (Ground x) (Ground r)

instance (LogicType a) => LogicType (Tree a) where

    data Reified (Tree a) var = Leaf' | Node' (Logic (Tree a) var) (Logic a var) (Logic (Tree a) var)

    quote Leaf' = quote0 "Leaf" Leaf'
    quote (Node' l x r) = quote3 "Node" Node' l x r

    unifyVal _ Leaf' Leaf' = pure ()
    unifyVal unif (Node' l x r) (Node' l' x' r') = unif l l' *> unif x x' *> unif r r'
    unifyVal _ _ _ = empty

    derefVal _ Leaf' = pure Leaf
    derefVal deref (Node' l x r) = liftA3 Node (deref l) (deref x) (deref r)

    project Leaf = leaf'
    project (Node l x r) = node' (project l) (project x) (project r)

    reify Leaf' = return Leaf
    reify (Node' (Ground l) (Ground x) (Ground r)) = liftA3 Node (reify l) (reify x) (reify r)
    reify (Node' _ _ _) = Nothing