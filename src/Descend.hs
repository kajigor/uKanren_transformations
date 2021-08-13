module Descend where

import Text.Printf ( printf )
import qualified Util.Miscellaneous as Utils

data Descend a = Descend { getCurr :: a, getAncs :: [a] } deriving (Eq)

instance (Show a) => Show (Descend a) where
  show (Descend curr ancs) = 
    show curr
    -- printf "%s <-\n%s" (show curr) (Utils.showList "  " ancs)