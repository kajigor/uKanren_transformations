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

crossingTime :: Person -> Stream Int
crossingTime PA = return 1
crossingTime PB = return 2
crossingTime PC = return 5
crossingTime PD = return 8

data Qua = Qua Bool Bool Bool Bool Bool deriving (Show, Eq)

data State = State Qua Qua deriving (Show, Eq)

data Move = Single Person | Double Person Person deriving (Show, Eq)

moveTime :: Move -> Stream Int
moveTime (Double a b) = do
  ta <- crossingTime a
  tb <- crossingTime b
  return (max ta tb)
moveTime (Single a) = crossingTime a

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
evalBridges s s' = (guard (s == s') $> []) `mplus`
      do
        (m, s'') <- step s
        m' <- Immature $ evalBridges s'' s'
        return (m : m')

boundedEvalBridges :: State -> State -> Stream [Move]
boundedEvalBridges s s' = do
    ms <- evalBridges s s'
    t <- traverse moveTime ms
    guard (sum t <= 15)
    return ms

startState :: State
startState = State (Qua True True True True True) (Qua False False False False False)

endState :: State
endState = State (Qua False False False False False) (Qua True True True True True)

preL :: Qua
preL = Qua True True True True False

preR :: Qua
preR = Qua False False False False True

postL :: Qua
postL = Qua False False True True False

postR :: Qua
postR = Qua True True False False True

mainBridges :: IO ()
mainBridges = mapM_ print (takeS 1 (boundedEvalBridges startState endState))