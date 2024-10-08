module TranslatedExamples.WGCKarnen where

import Eval
import Stream hiding (Empty)
import Subst (showSubst')
import Syntax
import Program
import Def

data Move = Empty | Goat | Wolf | Cabbage deriving (Show, Eq)

fresh1' :: X -> (Tx -> G X) -> G X
fresh1' i f = fresh ["a" ++ i] (f (V $ "a" ++ i))

fresh2' :: X -> (Tx -> Tx -> G X) -> G X
fresh2' i f = fresh ["a" ++ i, "b" ++ i] (f (V $ "a" ++ i) (V $ "b" ++ i))

fresh3' :: X -> (Tx -> Tx -> Tx -> G X) -> G X
fresh3' i f = fresh ["a" ++ i, "b" ++ i, "c" ++ i] (f (V $ "a" ++ i) (V $ "b" ++ i) (V $ "c" ++ i))

fresh4' :: X -> (Tx -> Tx -> Tx -> Tx -> G X) -> G X
fresh4' i f = fresh ["a" ++ i, "b" ++ i, "c" ++ i, "d" ++ i] (f (V $ "a" ++ i) (V $ "b" ++ i) (V $ "c" ++ i) (V $ "d" ++ i))

fresh1 = fresh1' ""

fresh2 = fresh2' ""

fresh3 = fresh3' ""

fresh4 = fresh4' ""

qua :: Tx -> Tx -> Tx -> Tx -> Tx
qua x y z t = C "qua" [x, y, z, t]

qua' :: Bool -> Bool -> Bool -> Bool -> Tx
qua' x y z t = qua (inj x) (inj y) (inj z) (inj t)

pair :: Tx -> Tx -> Tx
pair left right = C "state" [left, right]

inj :: (Show a) => a -> Tx
inj x = C (show x) []

isGoat :: Tx -> G X
isGoat s = fresh3 $ \x y z -> s === qua (inj True) x y z

isWolf :: Tx -> G X
isWolf s = fresh3 $ \x y z -> s === qua x (inj True) y z

isCabbage :: Tx -> G X
isCabbage s = fresh3 $ \x y z -> s === qua x y (inj True) z

isMan :: Tx -> G X
isMan s = fresh3 $ \x y z -> s === qua x y z (inj True)

noGoat :: Tx -> G X
noGoat s = fresh3 $ \x y z -> s === qua (inj False) x y z

noWolf :: Tx -> G X
noWolf s = fresh3 $ \x y z -> s === qua x (inj False) y z

noCabbage :: Tx -> G X
noCabbage s = fresh3 $ \x y z -> s === qua x y (inj False) z

noMan :: Tx -> G X
noMan s = fresh3 $ \x y z -> s === qua x y z (inj False)

safe :: Tx -> G X
safe state = fresh2 $ \left right -> state === pair left right &&& safe' left &&& safe' right
  where
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

swap :: Tx -> Tx -> G X
swap state state' = fresh2 $ \left right -> state === pair left right &&& state' === pair right left

step' :: Tx -> Tx -> Tx -> Tx -> G X
step' move left right state' =
  fresh4' "l" $ \lg lw lc lm ->
    fresh4' "r" $ \rg rw rc rm ->
      left === qua lg lw lc lm &&& right === qua rg rw rc rm
        &&& unsafeDisj
          [ move === inj Empty &&& state' === pair (qua lg lw lc (inj False)) (qua rg rw rc (inj True)) &&& safe state',
            move === inj Goat &&& isGoat left &&& state' === pair (qua (inj False) lw lc (inj False)) (qua (inj True) rw rc (inj True)) &&& safe state',
            move === inj Wolf &&& isWolf left &&& state' === pair (qua lg (inj False) lc (inj False)) (qua rg (inj True) rc (inj True)) &&& safe state',
            move === inj Cabbage &&& isCabbage left &&& state' === pair (qua lg lw (inj False) (inj False)) (qua rg rw (inj True) (inj True)) &&& safe state'
          ]

step :: Tx -> Tx -> Tx -> G X
step state move state' =
  fresh2' "s" $ \left right ->
    state === pair left right
      &&& unsafeDisj
        [ isMan left &&& noMan right &&& step' move left right state',
          isMan right &&& noMan left &&& fresh1' "s''" (\state'' -> step' move right left state'' &&& swap state'' state')
        ]

evalWGC :: Tx -> Tx -> Tx -> G X
evalWGC state moves state' =
  unsafeDisj
    [ moves === C "nil" [] &&& (state === state'),
      fresh3 $ \move moves' state'' -> moves === C "cons" [move, moves] &&& step state move state'' &&& call "evalWGC" [state'', moves', state']
    ]

startState :: Tx
startState = pair (qua' True True True True) (qua' False False False False)

endState :: Tx
endState = pair (qua' False False False False) (qua' True True True True)

wgcProgram :: Program G X
wgcProgram = Program [Def "evalWGC" ["state", "moves", "state'"] (evalWGC (V "state") (V "moves") (V "state'"))] $ fresh1 $ \moves -> call "evalWGC" [startState, moves, endState]

mainWGC :: IO ()
mainWGC =
  mapM_
    (putStrLn . showSubst')
    (takeS 1 $ run $ wgcProgram)