module TranslatedExamples.BridgeKarnen where

import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Def ( Def(Def) )
import Eval (run)
import Program ( Program(Program) )
import Stream (takeS)
import Subst (Subst)
import qualified Subst (lookup)
import Syntax

data Nop = Nop

instance Show Nop where
  show x = ""

fresh1' :: (Show x) => x -> (Tx -> G X) -> G X
fresh1' i f = fresh ["a" ++ show i] (f (V $ "a" ++ show i))

fresh2' :: (Show x) => x -> (Tx -> Tx -> G X) -> G X
fresh2' i f = fresh ["a" ++ show i, "b" ++ show i] (f (V $ "a" ++ show i) (V $ "b" ++ show i))

fresh3' :: (Show x) => x -> (Tx -> Tx -> Tx -> G X) -> G X
fresh3' i f = fresh ["a" ++ show i, "b" ++ show i, "c" ++ show i] (f (V $ "a" ++ show i) (V $ "b" ++ show i) (V $ "c" ++ show i))

fresh4' :: (Show x) => x -> (Tx -> Tx -> Tx -> Tx -> G X) -> G X
fresh4' i f = fresh ["a" ++ show i, "b" ++ show i, "c" ++ show i, "d" ++ show i] (f (V $ "a" ++ show i) (V $ "b" ++ show i) (V $ "c" ++ show i) (V $ "d" ++ show i))

fresh5' :: (Show x) => x -> (Tx -> Tx -> Tx -> Tx -> Tx -> G X) -> G X
fresh5' i f = fresh ["a" ++ show i, "b" ++ show i, "c" ++ show i, "d" ++ show i, "e" ++ show i] (f (V $ "a" ++ show i) (V $ "b" ++ show i) (V $ "c" ++ show i) (V $ "d" ++ show i) (V $ "e" ++ show i))

inj :: (Show a) => a -> Tx
inj x = C (show x) []

data Person = PA | PB | PC | PD deriving (Show, Eq)

zro :: Term a
zro = C "zro" []

suc :: Tx -> Tx
suc n = C "suc" [n]

nil :: Tx
nil = C "nil" []

cons :: Tx -> Tx -> Tx
cons h t = C "cons" [h, t]

fromInt :: Int -> Tx
fromInt 0 = zro
fromInt n = suc (fromInt (n - 1))

toInt :: Ts -> Subst -> Maybe Int
toInt (V n) s = Subst.lookup n s >>= (`toInt` s)
toInt (C "zro" []) s = Just 0
toInt (C "suc" [t]) s = (+ 1) <$> toInt t s
toInt _ _ = Nothing

maxo :: Tx -> Tx -> Tx -> G X
maxo a b out =
  unsafeDisj
    [ (a === zro) &&& (out === b),
      (b === zro) &&& (out === a),
      fresh3' "maxo" $ \a' b' out' -> (a === suc a') &&& (b === suc b') &&& (out === suc out') &&& Delay (call "maxo" [a', b', out'])
    ]

maxoDef :: Def G X
maxoDef = Def "maxo" ["a", "b", "out"] (maxo (V "a") (V "b") (V "out"))

addo :: Tx -> Tx -> Tx -> G X
addo a b out =
  unsafeDisj
    [ (a === zro) &&& (out === b),
      fresh2' "adoo" $ \a' out' -> (a === suc a') &&& (out === suc out') &&& Delay (call "addo" [a', b, out'])
    ]

addoDef :: Def G X
addoDef = Def "addo" ["a", "b", "out"] (addo (V "a") (V "b") (V "out"))

lteo :: Tx -> Tx -> G X
lteo a b =
  unsafeDisj
    [ a === zro,
      fresh2' "lteo" $ \a' b' -> a === suc a' &&& b === suc b' &&& Delay (call "lteo" [a', b'])
    ]

lteoDef :: Def G X
lteoDef = Def "lteo" ["a", "b"] (lteo (V "a") (V "b"))


qua :: Tx -> Tx -> Tx -> Tx -> Tx -> Tx
qua t a b c d = C "qua" [t, a, b, c, d]

qua' :: Bool -> Bool -> Bool -> Bool -> Bool -> Tx
qua' t a b c d = qua (inj t) (inj a) (inj b) (inj c) (inj d)

state :: Tx -> Tx -> Tx
state left right = C "state" [left, right]

singleMove :: Tx -> Tx
singleMove p1 = C "move" [p1]

doubleMove :: Tx -> Tx -> Tx
doubleMove p1 p2 = C "move" [p1, p2]

crossingTime :: Tx -> Tx -> G X
crossingTime p t =
  unsafeDisj
    [ (p === inj PA) &&& t === fromInt 1,
      (p === inj PB) &&& t === fromInt 2,
      (p === inj PC) &&& t === fromInt 5,
      (p === inj PD) &&& t === fromInt 8
    ]

moveTime :: Tx -> Tx -> G X
moveTime m time =
  fresh2' "people" $ \a b -> fresh2' "time" $ \ta tb ->
    unsafeDisj
      [ unsafeConj
          [ m === doubleMove a b,
            crossingTime a ta,
            crossingTime b tb,
            maxo ta tb time
          ],
        unsafeConj
          [ m === singleMove a,
            crossingTime a time
          ]
      ]

totalTime :: Tx -> Tx -> G X
totalTime ms out =
  unsafeDisj
    [ ms === nil &&& out === zro,
      fresh4' "totalTime" $ \h time t out' -> ms === cons h t &&& moveTime h time &&& Delay (call "totalTime" [t, out']) &&& addo time out' out
    ]

totalTimeDef :: Def G X
totalTimeDef = Def "totalTime" ["move", "outTime"] (totalTime (V "move") (V "outTime"))

isTorch :: Tx -> G X
isTorch s = fresh4' "isTorch" $ \a b c d -> s === qua (inj True) a b c d

noTorch :: Tx -> G X
noTorch s = fresh4' "noTorch" $ \a b c d -> s === qua (inj False) a b c d

personToT :: (Person -> G X) -> Tx -> G X
personToT f p =
  unsafeDisj
    [ (p === inj PA) &&& f PA,
      (p === inj PB) &&& f PB,
      (p === inj PC) &&& f PC,
      (p === inj PD) &&& f PD
    ]

moveTorch :: Tx -> Tx -> G X
moveTorch oldQua newQua = fresh4' "moveTorch" $ \a b c d -> oldQua === qua (inj False) a b c d &&& newQua === qua (inj True) a b c d

movePerson :: Tx -> Tx -> Person -> G X
movePerson oldQua newQua PA = fresh4' "movePerson" $ \t b c d -> oldQua === qua t (inj False) b c d &&& newQua === qua t (inj True) b c d
movePerson oldQua newQua PB = fresh4' "movePerson" $ \t a c d -> oldQua === qua t a (inj False) c d &&& newQua === qua t a (inj True) c d
movePerson oldQua newQua PC = fresh4' "movePerson" $ \t a b d -> oldQua === qua t a b (inj False) d &&& newQua === qua t a b (inj True) d
movePerson oldQua newQua PD = fresh4' "movePerson" $ \t a b c -> oldQua === qua t a b c (inj False) &&& newQua === qua t a b c (inj True)

movePersonT :: Tx -> Tx -> Tx -> G X
movePersonT p oldQua newQua = personToT (movePerson oldQua newQua) p

chainSingleMove :: Tx -> Tx -> Tx -> G X
chainSingleMove oldQua newQua a = fresh1' "chainMove" $ \q ->
  unsafeConj
    [ moveTorch oldQua q,
      movePersonT a q newQua
    ]

chainDoubleMove :: Tx -> Tx -> Tx -> Tx -> G X
chainDoubleMove oldQua newQua a b = fresh2' "chainMove" $ \q1 q2 ->
  unsafeConj
    [ moveTorch oldQua q1,
      movePersonT a q1 q2,
      movePersonT b q2 newQua
    ]

applyMove :: Tx -> Tx -> Tx -> Tx -> G X
applyMove m l r newState = fresh2' "apply#quas" $ \l' r' ->
  (newState === state l' r')
    &&& fresh2'
      "apply#people"
      ( \a b ->
          unsafeDisj
            [ unsafeConj
                [ m === doubleMove a b,
                  chainDoubleMove r r' a b,
                  chainDoubleMove l' l a b
                ],
              unsafeConj
                [ m === singleMove a,
                  chainSingleMove r r' a,
                  chainSingleMove l' l a
                ]
            ]
      )

swap :: Tx -> Tx -> G X
swap s s' = fresh2' "swap" $ \l r -> (s === state l r) &&& (s' === state r l)

step :: Tx -> Tx -> Tx -> G X
step s m s' = fresh2' "step#state" $ \l r ->
  (s === state l r)
    &&& unsafeDisj
      [ isTorch l &&& noTorch r &&& applyMove m l r s',
        isTorch r &&& noTorch l &&& fresh1' "step#s''" (\s'' -> applyMove m r l s'' &&& swap s'' s')
      ]

evalBridges :: Tx -> Tx -> Tx -> G X
evalBridges state moves state' =
  unsafeDisj
    [ moves === nil &&& (state === state'),
      fresh3' "eval" $ \move moves' state'' -> moves === cons move moves' &&& step state move state'' &&& Delay (call "evalBridges" [state'', moves', state'])
    ]

boundedEvalBridges :: Tx -> Tx -> Tx -> G X
boundedEvalBridges state moves state' =
  unsafeConj
    [ call "evalBridges" [state, moves, state'],
      fresh1' "boundedEvalBridges" $ \time -> call "totalTime" [moves, time] &&& call "lteo" [time, fromInt 15]
    ]

evalBridgesDef :: Def G X
evalBridgesDef = Def "evalBridges" ["state", "moves", "state'"] (evalBridges (V "state") (V "moves") (V "state'"))

startState :: Tx
startState = state (qua' True True True True True) (qua' False False False False False)

endState :: Tx
endState = state (qua' False False False False False) (qua' True True True True True)

showBridges :: Ts -> Subst -> Maybe String
showBridges (V n) s = Subst.lookup n s >>= (`showBridges` s)
showBridges (C "zro" []) s = Just "0"
showBridges t@(C "suc" _) s = show <$> toInt t s
showBridges (C "nil" []) s = Just "[]"
showBridges (C "cons" [h, t]) s = concat <$> sequence [showBridges h s, Just " : ", showBridges t s]
showBridges (C name []) s = Just name
showBridges (C "move" [p]) s = showBridges p s
showBridges (C "move" [p1, p2]) s = do
  s1 <- showBridges p1 s
  s2 <- showBridges p2 s
  return $ concat ["[", s1, ", ", s2, "]"]
showBridges (C "qua" [t, a, b, c, d]) s =
  Just $
    concat
      [ "{",
        intercalate ", " $
          catMaybes
            [ if showBridges t s == Just "True" then Just "Torch" else Nothing,
              if showBridges a s == Just "True" then Just "A" else Nothing,
              if showBridges b s == Just "True" then Just "B" else Nothing,
              if showBridges c s == Just "True" then Just "C" else Nothing,
              if showBridges d s == Just "True" then Just "D" else Nothing
            ],
        "}"
      ]
showBridges (C "state" [l, r]) s = do
  sl <- showBridges l s
  sr <- showBridges r s
  return $ concat [sl, "/", sr]
showBridges t s = Just $ show t

mainBridges :: IO ()
mainBridges =
  mapM_
    (print . showBridges (V 0))
    ( takeS 1 $
        run $
          Program [evalBridgesDef, totalTimeDef, addoDef, maxoDef, lteoDef] $
            fresh2' "mainBridges" $ \moves time ->
              boundedEvalBridges startState moves endState &&& totalTime moves time
    )