module Mode.Inst where

import Text.Printf

data Inst = Ground
          | Free
          deriving (Eq, Ord)

instance Show Inst where
  show Ground = "g"
  show Free = "f"

data Mode = Mode { before :: Inst, after :: Maybe Inst }
          deriving (Eq, Ord)

instance Show Mode where
  show mode =
      printf "%s -> %s" (show $ before mode) (showAfter $ after mode)
    where
      showAfter Nothing = "_"
      showAfter (Just inst) = show inst