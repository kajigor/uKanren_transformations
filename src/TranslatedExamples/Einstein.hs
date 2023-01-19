{-# LANGUAGE TupleSections #-}

module TranslatedExamples.Einstein where

import Control.Monad
import Stream

yes :: Stream ()
yes = return ()

no :: Stream ()
no = empty

(|||) :: Stream () -> Stream () -> Stream ()
(|||) = mplus

(&&&) :: Stream () -> Stream () -> Stream ()
(&&&) = (>>)

listToStream :: [a] -> Stream a
listToStream = foldr (mplus . return) empty

data HouseColor = Yellow | Blue | Red | Ivory | Green deriving (Show, Eq)

data Nationality = Norwegean | Ukranian | Englishman | Spaniard | Japanese deriving (Show, Eq)

data Drink = Water | Tea | Milk | Juice | Coffee deriving (Show, Eq)

data Smoke = Kools | Chesterfield | OldGold | LuckyStrike | Parliament deriving (Show, Eq)

data Pet = Fox | Horse | Snails | Dog | Zebra deriving (Show, Eq)

data Occupant = Occupant {house :: HouseColor, nationality :: Nationality, drinks :: Drink, smokes :: Smoke, pet :: Pet} deriving (Show, Eq)

newtype State = State (Occupant, Occupant, Occupant, Occupant, Occupant) deriving (Show, Eq)

allHouses :: [HouseColor]
allHouses = [Yellow, Blue, Red, Ivory, Green]

allNationailities :: [Nationality]
allNationailities = [Norwegean, Ukranian, Englishman, Spaniard, Japanese]

allDrinks :: [Drink]
allDrinks = [Water, Tea, Milk, Juice, Coffee]

allSmokes :: [Smoke]
allSmokes = [Kools, Chesterfield, OldGold, LuckyStrike, Parliament]

allPets :: [Pet]
allPets = [Fox, Horse, Snails, Dog, Zebra]

validState :: Stream State
validState = do
  (o1, o2) <- diffOutOut
  o3 <- diffInOut o1
  o4 <- diffInOut o1
  o5 <- diffInOut o1
  _ <- diffInIn o2 o3
  _ <- diffInIn o2 o4
  _ <- diffInIn o2 o5
  _ <- diffInIn o3 o4
  _ <- diffInIn o3 o5
  _ <- diffInIn o4 o5
  return (State (o1, o2, o3, o4, o5))
  where
    diffOutOut :: Stream (Occupant, Occupant)
    diffOutOut = do
      (h, h') <- unqOutOut allHouses
      (n, n') <- unqOutOut allNationailities
      (d, d') <- unqOutOut allDrinks
      (s, s') <- unqOutOut allSmokes
      (p, p') <- unqOutOut allPets
      return (Occupant h n d s p, Occupant h' n' d' s' p')

    diffInOut :: Occupant -> Stream Occupant
    diffInOut p1 = do
      h <- unqInOut (house p1) allHouses
      n <- unqInOut (nationality p1) allNationailities
      d <- unqInOut (drinks p1) allDrinks
      s <- unqInOut (smokes p1) allSmokes
      p <- unqInOut (pet p1) allPets
      return (Occupant h n d s p)

    diffInIn :: Occupant -> Occupant -> Stream ()
    diffInIn p1 p2 = do
      _ <- unqInIn (house p1) (house p2)
      _ <- unqInIn (nationality p1) (nationality p2)
      _ <- unqInIn (drinks p1) (drinks p2)
      _ <- unqInIn (smokes p1) (smokes p2)
      _ <- unqInIn (pet p1) (pet p2)
      yes

    unqOutOut :: (Eq a) => [a] -> Stream (a, a)
    unqOutOut v = msum $ map (\target -> (target,) <$> listToStream (filter (/= target) v)) v

    unqInOut :: (Eq a) => a -> [a] -> Stream a
    unqInOut x v = listToStream (filter (/= x) v)

    unqInIn :: (Eq a) => a -> a -> Stream ()
    unqInIn x y = guard (x /= y)

suchThat :: (Occupant -> Stream ()) -> State -> Stream ()
suchThat rel (State (o1, o2, o3, o4, o5)) =
  msum
    [ rel o1,
      rel o2,
      rel o3,
      rel o4,
      rel o5
    ]

neighboursSuchThat :: (Occupant -> Occupant -> Stream ()) -> State -> Stream ()
neighboursSuchThat rel (State (o1, o2, o3, o4, o5)) =
  msum
    [ rel o1 o2,
      rel o2 o3,
      rel o3 o4,
      rel o4 o5
    ]

neighboursSuchThatBi rel = neighboursSuchThat (\l r -> rel l r ||| rel r l)

clue02 :: State -> Stream ()
clue02 = suchThat $ \x -> guard (nationality x == Englishman) &&& guard (house x == Red)

clue03 :: State -> Stream ()
clue03 = suchThat $ \x -> guard (nationality x == Spaniard) &&& guard (pet x == Dog)

clue04 :: State -> Stream ()
clue04 = suchThat $ \x -> guard (house x == Green) &&& guard (drinks x == Coffee)

clue05 :: State -> Stream ()
clue05 = suchThat $ \x -> guard (nationality x == Ukranian) &&& guard (drinks x == Tea)

clue06 :: State -> Stream ()
clue06 = neighboursSuchThat $ \l r -> guard (house l == Ivory) &&& guard (house r == Green)

clue07 :: State -> Stream ()
clue07 = suchThat $ \x -> guard (smokes x == OldGold) &&& guard (pet x == Snails)

clue08 :: State -> Stream ()
clue08 = suchThat $ \x -> guard (house x == Yellow) &&& guard (smokes x == Kools)

clue09 :: State -> Stream ()
clue09 (State (_, _, o3, _, _))  = guard (drinks o3 == Milk)

clue10 :: State -> Stream ()
clue10 (State (o1, _, _, _, _)) = guard (nationality o1 == Norwegean)

clue11 :: State -> Stream ()
clue11 = neighboursSuchThatBi $ \x y -> guard (smokes x == Chesterfield) &&& guard (pet y == Fox)

clue12 :: State -> Stream ()
clue12 = neighboursSuchThatBi $ \x y -> guard (pet x == Horse) &&& guard (smokes y == Kools)

clue13 :: State -> Stream ()
clue13 = suchThat $ \x -> guard (smokes x == LuckyStrike) &&& guard (drinks x == Juice)

clue14 :: State -> Stream ()
clue14 = suchThat $ \x -> guard (nationality x == Japanese) &&& guard (smokes x == Parliament)

clue15 :: State -> Stream ()
clue15 = neighboursSuchThatBi $ \x y -> guard (nationality x == Norwegean) &&& guard (house y == Blue)

einstein :: Stream State
einstein = do
    s <- validState
    _ <- clue02 s
    _ <- clue03 s
    _ <- clue04 s
    _ <- clue05 s
    _ <- clue06 s
    _ <- clue07 s
    _ <- clue08 s
    _ <- clue09 s
    _ <- clue10 s
    _ <- clue11 s
    _ <- clue12 s
    _ <- clue13 s
    _ <- clue14 s
    _ <- clue15 s
    return s

mainEinstein :: IO ()
mainEinstein = mapM_ print (takeS 10 einstein)