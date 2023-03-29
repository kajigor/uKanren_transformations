module Mode.Inst where

import           Text.Printf

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

-- Disregards the after mode
identicalBeforeMode :: Mode -> Mode -> Bool
identicalBeforeMode x y =
  before x == before y

-- Before modes should be compatible:
-- a ground var may be put in place of a free var, but not vice versa.
compatibleBeforeMode :: Mode -> Mode -> Bool
compatibleBeforeMode x y =
    compare (before x) (before y)
  where
    compare Free Ground = False
    compare _ _ = True

-- Tests that modes are exactly equal
identicalModes :: [(a, Mode)] -> [(a, Mode)] -> Bool
identicalModes xs ys = map snd xs == map snd ys

-- Disregards the after mode
identicalBeforeModes :: [(a, Mode)] -> [(a, Mode)] -> Bool
identicalBeforeModes xs ys =
  length xs == length ys &&
  all (uncurry identicalBeforeMode) (zipWith (\x y -> (snd x, snd y)) xs ys)

-- Before modes should be compatible:
-- a ground var may be put in place of a free var, but not vice versa.
compatibleBeforeModes :: [(a, Mode)] -> [(a, Mode)] -> Bool
compatibleBeforeModes callArgs moddedDef =
  length callArgs == length moddedDef &&
  all (uncurry compatibleBeforeMode) (zipWith (\x y -> (snd x, snd y)) callArgs moddedDef)

isBeforeGround :: (a, Mode) -> Bool
isBeforeGround (_, mode) = before mode == Ground
