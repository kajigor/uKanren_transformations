module TranslatedExamples.BridgeKarnen where

import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Def
import Eval (run)
import Program
import Stream (takeS)
import Subst (Subst, showSubst')
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

fresh1 :: (Tx -> G X) -> G X
fresh1 = fresh1' Nop

fresh2 :: (Tx -> Tx -> G X) -> G X
fresh2 = fresh2' Nop

fresh3 :: (Tx -> Tx -> Tx -> G X) -> G X
fresh3 = fresh3' Nop

fresh4 :: (Tx -> Tx -> Tx -> Tx -> G X) -> G X
fresh4 = fresh4' Nop

fresh5 :: (Tx -> Tx -> Tx -> Tx -> Tx -> G X) -> G X
fresh5 = fresh5' Nop

inj :: (Show a) => a -> Tx
inj x = C (show x) []

data Person = PA | PB | PC | PD deriving (Show, Eq)

zro :: Tx
zro = C "zro" []

suc :: Tx -> Tx
suc n = C "suc" [n]

nil :: Tx
nil = C "nil" []

cons :: Tx -> Tx -> Tx
cons h t = C "cons" [h, t]

fromInt 0 = zro
fromInt n = suc (fromInt (n - 1))

maxo :: Tx -> Tx -> Tx -> G X
maxo a b out =
  unsafeDisj
    [ (a === zro) &&& (out === b),
      (b === zro) &&& (out === a),
      fresh3 $ \a' b' out' -> (a === suc a') &&& (b === suc b') &&& (out === suc out') &&& maxo a' b' out'
    ]

addo :: Tx -> Tx -> Tx -> G X
addo a b out =
  unsafeDisj
    [ (a === zro) &&& (out === b),
      fresh2 $ \a' out' -> (a === suc a') &&& (out === suc out') &&& addo a' b out'
    ]

sumo :: Tx -> Tx -> G X
sumo l out =
  unsafeDisj
    [ (l === nil) &&& (out === zro),
      fresh3 $ \h t out' -> (l === cons h t) &&& addo h out' out &&& sumo t out'
    ]

crossingTime :: Tx -> Tx -> G X
crossingTime p t =
  unsafeDisj
    [ (p === inj PA) &&& t === fromInt 1,
      (p === inj PB) &&& t === fromInt 2,
      (p === inj PC) &&& t === fromInt 5,
      (p === inj PD) &&& t === fromInt 8
    ]

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

isTorch :: Tx -> G X
isTorch s = fresh4 $ \a b c d -> s === qua (inj True) a b c d

isPerson :: Tx -> Person -> G X
isPerson s PA = fresh4 $ \t b c d -> s === qua t (inj True) b c d
isPerson s PB = fresh4 $ \t a c d -> s === qua t a (inj True) c d
isPerson s PC = fresh4 $ \t a b d -> s === qua t a b (inj True) d
isPerson s PD = fresh4 $ \t a b c -> s === qua t a b c (inj True)

noTorch :: Tx -> G X
noTorch s = fresh4 $ \a b c d -> s === qua (inj False) a b c d

noPerson :: Tx -> Person -> G X
noPerson s PA = fresh4 $ \t b c d -> s === qua t (inj False) b c d
noPerson s PB = fresh4 $ \t a c d -> s === qua t a (inj False) c d
noPerson s PC = fresh4 $ \t a b d -> s === qua t a b (inj False) d
noPerson s PD = fresh4 $ \t a b c -> s === qua t a b c (inj False)

personToT :: (Person -> G X) -> Tx -> G X
personToT f p =
  unsafeDisj
    [ (p === inj PA) &&& f PA,
      (p === inj PB) &&& f PB,
      (p === inj PC) &&& f PC,
      (p === inj PD) &&& f PD
    ]

isPersonT :: Tx -> Tx -> G X
isPersonT p s = personToT (isPerson s) p

noPersonT :: Tx -> Tx -> G X
noPersonT p s = personToT (noPerson s) p

moveTorch :: Tx -> Tx -> G X
moveTorch oldQua newQua = fresh4 $ \a b c d -> oldQua === qua (inj False) a b c d &&& newQua === qua (inj True) a b c d

movePerson :: Tx -> Tx -> Person -> G X
movePerson oldQua newQua PA = fresh4 $ \t b c d -> oldQua === qua t (inj False) b c d &&& newQua === qua t (inj True) b c d
movePerson oldQua newQua PB = fresh4 $ \t a c d -> oldQua === qua t a (inj False) c d &&& newQua === qua t a (inj True) c d
movePerson oldQua newQua PC = fresh4 $ \t a b d -> oldQua === qua t a b (inj False) d &&& newQua === qua t a b (inj True) d
movePerson oldQua newQua PD = fresh4 $ \t a b c -> oldQua === qua t a b c (inj False) &&& newQua === qua t a b c (inj True)

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
swap s s' = fresh2 $ \l r -> (s === state l r) &&& (s' === state r l)

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
      fresh3' "eval" $ \move moves' state'' -> moves === cons move moves' &&& step state move state'' &&& call "evalBridges" [state'', moves', state']
    ]

evalBridgesDef :: Def G X
evalBridgesDef = Def "evalBridges" ["state", "moves", "state'"] (evalBridges (V "state") (V "moves") (V "state'"))

startState :: Tx
startState = state (qua' True True True True True) (qua' False False False False False)

endState :: Tx
endState = state (qua' False False False False False) (qua' True True True True True)

preL :: Tx
preL = qua' True True True True False

preR :: Tx
preR = qua' False False False False True

postL :: Tx
postL = qua' False False True True False

postR :: Tx
postR = qua' True True False False True

showBridges :: Ts -> Subst -> Maybe String
showBridges (V n) s = Subst.lookup n s >>= (`showBridges` s)
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
    (print . showSubst')
    -- (takeS 1 $ run $ Program [Def "evalBridges" ["state", "moves", "state'"] (evalBridges (V "state") (V "moves") (V "state'"))] $ fresh1' "mainBridges" $ \moves -> call "evalBridges" [state preL preR, moves, endState])
    ( takeS 1 $
        run $
          Program [evalBridgesDef] $
            fresh5' "mainBridges" $ \m s'' m' m'' s''' -> s'' === (state postL postR) &&&
              step (state preL preR) m s''
                &&& step s'' m' s'''
                &&& call "evalBridges" [s''', m'', startState]
    )
-- mainBridges =
--   mapM_
--     (print . showSubst')
--     -- (takeS 1 $ run $ Program [Def "evalBridges" ["state", "moves", "state'"] (evalBridges (V "state") (V "moves") (V "state'"))] $ fresh1' "mainBridges" $ \moves -> call "evalBridges" [state preL preR, moves, endState])
--     ( takeS 1 $
--         run $
--           Program [evalBridgesDef] $
--             fresh5' "mainBridges" $ \m s'' m' m'' s''' ->
--                 step (state postL postR) m' s'''
--                 &&& call "evalBridges" [s''', m'', startState]
--     )
-- (takeS 1 $ run $ Program [] $ fresh1' "mainBridges" $ \m -> isTorch preL &&& applyMove m preL preR endState)