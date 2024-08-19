module Descend where

data Descend a = Descend { getCurr :: a, getAncs :: [a] } deriving (Eq)

init :: a -> Descend a 
init curr = Descend { getCurr = curr, getAncs = [] }

add :: a -> Descend a -> Descend a 
add curr (Descend x xs) = Descend curr (x : xs)

modifyCurr :: (a -> a) -> Descend a -> Descend a 
modifyCurr f (Descend x xs) = Descend (f x) xs 

instance (Show a) => Show (Descend a) where
  show (Descend curr ancs) =
    show curr
