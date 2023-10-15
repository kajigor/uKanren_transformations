module TranslatedExamples.WGC where

import Stream hiding (Empty)
import Control.Monad (guard)

data Move = Empty | Goat | Wolf | Cabbage deriving (Show, Eq)

yes :: Stream ()
yes = return ()

no :: Stream ()
no = empty

(|||) :: Stream () -> Stream () -> Stream ()
(|||) = mplus

(&&&) :: Stream () -> Stream () -> Stream ()
a &&& b = do
  x <- a
  y <- b
  yes

pair :: a -> a -> (a, a)
pair a b = (a, b)

type State = ((Bool, Bool), (Bool, Bool))

type FullState = (State, State)

qua :: Bool -> Bool -> Bool -> Bool -> State
qua a b c d = pair (pair a b) (pair c d)

isGoat :: State -> Stream ()
isGoat ((True, x), (y, z)) = yes
isGoat _ = no

isWolf :: State -> Stream ()
isWolf ((x, True), (y, z)) = yes
isWolf _ = no

isCabbage :: State -> Stream ()
isCabbage ((x, y), (True, z)) = yes
isCabbage _ = no

isMan :: State -> Stream ()
isMan ((x, y), (z, True)) = yes
isMan _ = no

noGoat :: State -> Stream ()
noGoat ((False, x), (y, z)) = yes
noGoat _ = no

noWolf :: State -> Stream ()
noWolf ((x, False), (y, z)) = yes
noWolf _ = no

noCabbage :: State -> Stream ()
noCabbage ((x, y), (False, z)) = yes
noCabbage _ = no

noMan :: State -> Stream ()
noMan ((x, y), (z, False)) = yes
noMan _ = no

safe :: FullState -> Stream ()
safe (left, right) = safe' left &&& safe' right
  where
    safe' :: State -> Stream ()
    safe' side =
      isMan side
        ||| ( noMan side
                &&& ( noGoat side
                        ||| ( isGoat side
                                &&& ( (noCabbage side &&& noWolf side)
                                        ||| (isCabbage side &&& isWolf side)
                                    )
                            )
                    )
            )

swap :: FullState -> Stream FullState
swap (left, right) = return (right, left)

step :: FullState  -> Stream (Move, FullState)
step (left, right) = do
    _ <- isMan left
    _ <- noMan right
    (m, state') <- step' left right
    _ <- safe state'
    return (m, state')
    `mplus` do
        _ <- noMan left
        _ <- isMan right
        (m, state') <- step' right left
        _ <- safe state'
        state'' <- swap state'
        return (m, state'')
    where
        step' left@((lg, lw), (lc, lm)) ((rg, rw), (rc, rm)) = foldr mplus empty [
                do
                    return (Empty, pair (qua lg lw lc False) (qua rg rw rc True)),
                do
                    _ <- isGoat left
                    return (Goat, pair (qua False lw lc False) (qua True rw rc True)),
                do
                    _ <- isWolf left
                    return (Wolf, pair (qua lg False lc False) (qua rg True rc True)),
                do
                    _ <- isCabbage left
                    return (Cabbage, pair (qua lg lw False False) (qua rg rw True True))
            ]

evalWGC :: FullState -> FullState -> Stream [Move]
evalWGC state state' = do
    guard (state == state')
    return []
    `mplus` do
        (h, state'') <- step state
        t <- Immature $ evalWGC state'' state'
        return (h : t)

startState :: FullState
startState = pair (qua True True True True) (qua False False False False)

endState :: FullState
endState = pair (qua False False False False) (qua True True True True)

mainWGC :: IO ()
mainWGC = mapM_ print (takeS 10 $ evalWGC startState endState)