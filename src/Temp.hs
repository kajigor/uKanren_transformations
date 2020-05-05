{-# LANGUAGE ScopedTypeVariables #-}

module Temp where

class Generator a where
  gen :: [a]


instance (Generator a) => Generator [a] where
  gen = [] : (map (flip (:)) gen <*> gen)


instance Generator Word where
  gen = [0..2]