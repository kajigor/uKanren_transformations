module Descend where

data Descend a = Descend { getCurr :: a, getAncs :: [a] } deriving (Eq)

instance (Show a) => Show (Descend a) where
  show (Descend curr ancs) =
    show curr