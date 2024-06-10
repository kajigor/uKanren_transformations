module TranslatedExamples.Bridge where

import Control.Monad
import Data.Functor (($>))
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

data Person = PA | PB | PC | PD deriving (Show, Eq)

data Qua = Qua Bool Bool Bool Bool Bool deriving (Show, Eq)

data State = State Qua Qua deriving (Show, Eq)

data Move = Single Person | Double Person Person deriving (Show, Eq)

data Nat = Zro | Suc Nat deriving (Show, Eq)

addNat :: Nat -> Nat -> Stream Nat
addNat Zro b = return b
addNat (Suc a) b = do
  out' <- addNat a b
  return (Suc out')

maxNat :: Nat -> Nat -> Stream Nat
maxNat Zro b = return b
maxNat a Zro = return a
maxNat (Suc a) (Suc b) = do
  out' <- maxNat a b
  return (Suc out')

lte :: Nat -> Nat -> Stream ()
lte Zro _ = yes
lte (Suc a) (Suc b) = lte a b
lte _ _ = no

fromInt :: Int -> Nat
fromInt 0 = Zro
fromInt n = Suc (fromInt (n - 1))

toInt :: Nat -> Int
toInt Zro = 0
toInt (Suc n) = 1 + toInt n

crossingTime :: Person -> Stream Nat
crossingTime PA = return (fromInt 1)
crossingTime PB = return (fromInt 2)
crossingTime PC = return (fromInt 5)
crossingTime PD = return (fromInt 8)

moveTime :: Move -> Stream Nat
moveTime (Double a b) = do
  ta <- crossingTime a
  tb <- crossingTime b
  maxNat ta tb
moveTime (Single a) = crossingTime a

totalTime :: [Move] -> Stream Nat
totalTime [] = return Zro
totalTime (h : t) = do
  time <- moveTime h
  out' <- totalTime t
  addNat time out'

isTorch :: Qua -> Stream ()
isTorch (Qua True _ _ _ _) = yes
isTorch _ = no

noTorch :: Qua -> Stream ()
noTorch (Qua False _ _ _ _) = yes
noTorch _ = no

swap :: State -> Stream State
swap (State l r) = return $ State r l

moveTorchForward :: Qua -> Stream Qua
moveTorchForward (Qua False a b c d) = return $ Qua True a b c d
moveTorchForward _ = empty

moveTorchBackward :: Qua -> Stream Qua
moveTorchBackward (Qua True a b c d) = return $ Qua False a b c d
moveTorchBackward _ = empty

movePersonForwardOut :: Qua -> Stream (Qua, Person)
movePersonForwardOut (Qua t a b c d) =
  msum
    [ guard (not a) $> (Qua t True b c d, PA),
      guard (not b) $> (Qua t a True c d, PB),
      guard (not c) $> (Qua t a b True d, PC),
      guard (not d) $> (Qua t a b c True, PD)
    ]

movePersonBackwardIn :: Qua -> Person -> Stream Qua
movePersonBackwardIn (Qua t True b c d) PA = return $ Qua t False b c d
movePersonBackwardIn (Qua t a True c d) PB = return $ Qua t a False c d
movePersonBackwardIn (Qua t a b True d) PC = return $ Qua t a b False d
movePersonBackwardIn (Qua t a b c True) PD = return $ Qua t a b c False
movePersonBackwardIn _ _ = empty

chainSingleMoveForwardOut :: Qua -> Stream (Qua, Person)
chainSingleMoveForwardOut oldQua = do
  q <- moveTorchForward oldQua
  movePersonForwardOut q

chainSingleMoveBackwardIn :: Qua -> Person -> Stream Qua
chainSingleMoveBackwardIn oldQua a = do
  q <- moveTorchBackward oldQua
  movePersonBackwardIn q a

chainDoubleMoveForwardOut :: Qua -> Stream (Qua, Person, Person)
chainDoubleMoveForwardOut oldQua = do
  q <- moveTorchForward oldQua
  (q', a) <- movePersonForwardOut q
  (q'', b) <- movePersonForwardOut q'
  return (q'', a, b)

chainDoubleMoveBackwardIn :: Qua -> Person -> Person -> Stream Qua
chainDoubleMoveBackwardIn oldQua a b = do
  q <- moveTorchBackward oldQua
  q' <- movePersonBackwardIn q a
  movePersonBackwardIn q' b

applyMove :: Qua -> Qua -> Stream (Move, State)
applyMove l r =
  msum
    [ do
        (r', a) <- chainSingleMoveForwardOut r
        l' <- chainSingleMoveBackwardIn l a
        return (Single a, State l' r'),
      do
        (r', a, b) <- chainDoubleMoveForwardOut r
        l' <- chainDoubleMoveBackwardIn l a b
        return (Double a b, State l' r')
    ]

step :: State -> Stream (Move, State)
step (State l r) =
  msum
    [ do
        _ <- isTorch l
        _ <- noTorch r
        applyMove l r,
      do
        _ <- noTorch l
        _ <- isTorch r
        (m, s'') <- applyMove r l
        s' <- swap s''
        return (m, s')
    ]

evalBridges :: State -> State -> Stream [Move]
evalBridges s s' =
  msum
    [ guard (s == s') $> [],
      do
        (m, s'') <- step s
        m' <- Immature $ evalBridges s'' s'
        return (m : m')
    ]

boundedEvalBridges :: State -> State -> Stream [Move]
boundedEvalBridges s s' = do
  ms <- evalBridges s s'
  t <- totalTime ms
  _ <- lte t (fromInt 15)
  return ms

startState :: State
startState = State (Qua True True True True True) (Qua False False False False False)

endState :: State
endState = State (Qua False False False False False) (Qua True True True True True)

mainBridges :: IO ()
mainBridges = mapM_ print (takeS 1 (boundedEvalBridges startState endState))