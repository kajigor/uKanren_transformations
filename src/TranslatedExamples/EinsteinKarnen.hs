module TranslatedExamples.EinsteinKarnen where

import Eval (run)
import Stream (takeS)
import Subst (showSubst')
import Syntax

data Nop = Nop

instance Show Nop where
  show x = ""

fresh1' :: (Show x) => x -> (Tx -> G a) -> G a
fresh1' i f = fresh ["a" ++ show i] (f (V $ "a" ++ show i))

fresh2' :: (Show x) => x -> (Tx -> Tx -> G a) -> G a
fresh2' i f = fresh ["a" ++ show i, "b" ++ show i] (f (V $ "a" ++ show i) (V $ "b" ++ show i))

fresh3' :: (Show x) => x -> (Tx -> Tx -> Tx -> G a) -> G a
fresh3' i f = fresh ["a" ++ show i, "b" ++ show i, "c" ++ show i] (f (V $ "a" ++ show i) (V $ "b" ++ show i) (V $ "c" ++ show i))

fresh4' :: (Show x) => x -> (Tx -> Tx -> Tx -> Tx -> G a) -> G a
fresh4' i f = fresh ["a" ++ show i, "b" ++ show i, "c" ++ show i, "d" ++ show i] (f (V $ "a" ++ show i) (V $ "b" ++ show i) (V $ "c" ++ show i) (V $ "d" ++ show i))

fresh5' :: (Show x) => x -> (Tx -> Tx -> Tx -> Tx -> Tx -> G a) -> G a
fresh5' i f = fresh ["a" ++ show i, "b" ++ show i, "c" ++ show i, "d" ++ show i, "e" ++ show i] (f (V $ "a" ++ show i) (V $ "b" ++ show i) (V $ "c" ++ show i) (V $ "d" ++ show i) (V $ "e" ++ show i))

fresh1 :: (Tx -> G a) -> G a
fresh1 = fresh1' Nop

fresh2 :: (Tx -> Tx -> G a) -> G a
fresh2 = fresh2' Nop

fresh3 :: (Tx -> Tx -> Tx -> G a) -> G a
fresh3 = fresh3' Nop

fresh4 :: (Tx -> Tx -> Tx -> Tx -> G a) -> G a
fresh4 = fresh4' Nop

fresh5 :: (Tx -> Tx -> Tx -> Tx -> Tx -> G a) -> G a
fresh5 = fresh5' Nop

inj :: (Show a) => a -> Tx
inj x = C (show x) []

data HouseColor = Yellow | Blue | Red | Ivory | Green deriving (Show, Eq)

data Nationality = Norwegean | Ukranian | Englishman | Spaniard | Japanese deriving (Show, Eq)

data Drink = Water | Tea | Milk | Juice | Coffee deriving (Show, Eq)

data Smoke = Kools | Chesterfield | OldGold | LuckyStrike | Parliament deriving (Show, Eq)

data Pet = Fox | Horse | Snails | Dog | Zebra deriving (Show, Eq)

data Occupant = Occupant {house :: Tx, nationality :: Tx, drinks :: Tx, smokes :: Tx, pet :: Tx}

newtype State = State (Occupant, Occupant, Occupant, Occupant, Occupant)

freshOccupant :: Show x => x -> (Occupant -> G X) -> G X
freshOccupant s f = fresh5' s $ \h n d s p -> f (Occupant h n d s p)

freshState :: (State -> G X) -> G X
freshState f =
  freshOccupant "o1" $ \o1 ->
    freshOccupant "o2" $ \o2 ->
      freshOccupant "o3" $ \o3 ->
        freshOccupant "o4" $ \o4 ->
          freshOccupant "o5" $ \o5 -> f (State (o1, o2, o3, o4, o5))

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

validState :: State -> G X
validState (State (o1, o2, o3, o4, o5)) =
  unsafeConj
    [ diff o1 o2,
      diff o1 o3,
      diff o1 o4,
      diff o1 o5,
      diff o2 o3,
      diff o2 o4,
      diff o2 o5,
      diff o3 o4,
      diff o3 o5,
      diff o4 o5
    ]
  where
    diff :: Occupant -> Occupant -> G X
    diff p1 p2 =
      unsafeConj
        [ unq (house p1) (house p2) (map inj allHouses),
          unq (nationality p1) (nationality p2) (map inj allNationailities),
          unq (drinks p1) (drinks p2) (map inj allDrinks),
          unq (smokes p1) (smokes p2) (map inj allSmokes),
          unq (pet p1) (pet p2) (map inj allPets)
        ]

    unq :: Tx -> Tx -> [Tx] -> G X
    unq x y v = unsafeDisj $ map (\target -> (x === target) &&& unsafeDisj (map (y ===) (filter (/= target) v))) v


suchThat :: (Occupant -> G X) -> State -> G X
suchThat rel (State (o1, o2, o3, o4, o5)) = unsafeDisj
    [ rel o1,
      rel o2,
      rel o3,
      rel o4,
      rel o5
    ]

neighboursSuchThat :: (Occupant -> Occupant -> G X) -> State -> G X
neighboursSuchThat rel (State (o1, o2, o3, o4, o5)) = unsafeDisj
    [ rel o1 o2,
      rel o2 o3,
      rel o3 o4,
      rel o4 o5
    ]

neighboursSuchThatBi rel = neighboursSuchThat (\l r -> rel l r ||| rel r l)

clue02 :: State -> G X
clue02 = suchThat $ \x -> (nationality x === inj Englishman) &&& (house x === inj Red)

clue03 :: State -> G X
clue03 = suchThat $ \x -> (nationality x === inj Spaniard) &&& (pet x === inj Dog)

clue04 :: State -> G X
clue04 = suchThat $ \x -> (house x === inj Green) &&& (drinks x === inj Coffee)

clue05 :: State -> G X
clue05 = suchThat $ \x -> (nationality x === inj Ukranian) &&& (drinks x === inj Tea)

clue06 :: State -> G X
clue06 = neighboursSuchThat $ \l r -> (house l === inj Ivory) &&& (house r === inj Green)

clue07 :: State -> G X
clue07 = suchThat $ \x -> (smokes x === inj OldGold) &&& (pet x === inj Snails)

clue08 :: State -> G X
clue08 = suchThat $ \x -> (house x === inj Yellow) &&& (smokes x === inj Kools)

clue09 :: State -> G X
clue09 (State (_, _, o3, _, _))  = drinks o3 === inj Milk

clue10 :: State -> G X
clue10 (State (o1, _, _, _, _)) = nationality o1 === inj Norwegean

clue11 :: State -> G X
clue11 = neighboursSuchThatBi $ \x y -> (smokes x === inj Chesterfield) &&& (pet y === inj Fox)

clue12 :: State -> G X
clue12 = neighboursSuchThatBi $ \x y -> (pet x === inj Horse) &&& (smokes y === inj Kools)

clue13 :: State -> G X
clue13 = suchThat $ \x -> (smokes x === inj LuckyStrike) &&& (drinks x === inj Juice)

clue14 :: State -> G X
clue14 = suchThat $ \x -> (nationality x === inj Japanese) &&& (smokes x === inj Parliament)

clue15 :: State -> G X
clue15 = neighboursSuchThatBi $ \x y -> (nationality x === inj Norwegean) &&& (house y === inj Blue)

einstein :: State -> G X
einstein s =
  unsafeConj
    [ validState s,
      clue02 s,
      clue03 s,
      clue04 s,
      clue05 s,
      clue06 s,
      clue07 s,
      clue08 s,
      clue09 s,
      clue10 s,
      clue11 s,
      clue12 s,
      clue13 s,
      clue14 s,
      clue15 s
    ]

mainEinstein = mapM_ (putStrLn . showSubst') (takeS 1 $ run $ Program [] (freshState $ \s -> einstein s))