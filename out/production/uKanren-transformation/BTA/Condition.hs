{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BTA.Condition where
import Data.Maybe (mapMaybe)

data Condition a 
    = Lt a a 
    | Eq a a
    deriving (Eq, Functor, Show)
