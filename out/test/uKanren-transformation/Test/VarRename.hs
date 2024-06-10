module Test.VarRename where

import           Control.Monad.State
import           Data.Maybe
import           Def
import           Syntax
import           Test.Helper
import           VarRename

x, y, z, h, t, r, st, fm, u, v, w, n, m :: String
x = "x"
y = "y"
z = "z"
h = "h"
t = "t"
r = "r"
st = "st"
fm = "fm"
u = "u"
v = "v"
w = "w"
n = "n"
m = "m"

unit_enumerateVar :: Assertion
unit_enumerateVar = do
    -- No "x" in the state
    testEnumerateVar x emptyState (Left "x undefined")

    -- "x" is put in the state, thus is assigned to 0
    state' <- assertIsJustState $ execStateT (newVar x) emptyState
    testEnumerateVar x state' (Right 0)

    -- No "y" in the state
    testEnumerateVar y state' (Left "y undefined")

    -- "y" is put in the state with "x", thus is assigned to 1
    state'' <- assertIsJustState $ execStateT (newVar y) state'
    testEnumerateVar y state'' (Right 1)

    -- Second time "x" is put in the state, thus is assigned to 2
    state''' <- assertIsJustState $ execStateT (newVar x) state''
    testEnumerateVar x state''' (Right 2)
  where
    testEnumerateVar :: X -> RenameState S -> Either String S -> Assertion
    testEnumerateVar v state exp =
      evalStateT (enumerateVar v) state @?= exp

assertIsJustState state = do
  assertBool "newVar mast exec to some state" $ isJust state
  return $ fromJust state

unit_enumerateTerm :: Assertion
unit_enumerateTerm = do
    state <- assertIsJustState $ execStateT (do newVar x; newVar y) emptyState
    testEnumerateTerm (V x) state (Right $ V 0)
    testEnumerateTerm (V y) state (Right $ V 1)
    testEnumerateTerm (V z) state (Left "z undefined")

    testEnumerateTerm (C "" [V x, V x, C "" [V y, V x]]) state (Right $ C "" [V 0, V 0, C "" [V 1, V 0]])
    testEnumerateTerm (C "" [V x, V x, C "" [V z, V x]]) state (Left "z undefined")

  where
    testEnumerateTerm :: Term X -> RenameState S -> Either String (Term S) -> Assertion
    testEnumerateTerm t state exp =
      evalStateT (enumerateTerm t) state @?= exp

selfUnif :: a -> G a
selfUnif x = V x :=: V x

varUnif :: a -> a -> G a
varUnif x y = V x :=: V y

constUnif :: a -> G a
constUnif x = V x :=: C "" []

constUnif' :: a -> G a
constUnif' x = C "" [] :=: V x

complexUnif :: a -> a -> G a
complexUnif x y = C "" [V x, V y, V x] :=: C "" [C "" [V y], V x, V x]

unit_enumerateUnif :: Assertion
unit_enumerateUnif = do
    state <- assertIsJustState $ execStateT (do newVar x; newVar y) emptyState
    state' <- assertIsJustState $ execStateT (newVar x) state

    testEnumerateGoal (selfUnif x) state (Right (selfUnif 0))
    testEnumerateGoal (selfUnif x) state' (Right (selfUnif 2))
    testEnumerateGoal (selfUnif y) state (Right (selfUnif 1))
    testEnumerateGoal (selfUnif z) state (Left "z undefined")

    testEnumerateGoal (varUnif x y) state (Right (varUnif 0 1))
    testEnumerateGoal (varUnif y x) state (Right (varUnif 1 0))
    testEnumerateGoal (varUnif x y) state' (Right (varUnif 2 1))
    testEnumerateGoal (varUnif x z) state (Left "z undefined")

    testEnumerateGoal (constUnif x) state (Right (constUnif 0))
    testEnumerateGoal (constUnif y) state (Right (constUnif 1))
    testEnumerateGoal (constUnif z) state (Left "z undefined")
    testEnumerateGoal (constUnif x) state' (Right (constUnif 2))

    testEnumerateGoal (constUnif' x) state  (Right (constUnif' 0))
    testEnumerateGoal (constUnif' y) state  (Right (constUnif' 1))
    testEnumerateGoal (constUnif' z) state  (Left "z undefined")
    testEnumerateGoal (constUnif' x) state' (Right (constUnif' 2))

    testEnumerateGoal (complexUnif x y) state (Right (complexUnif 0 1))
    testEnumerateGoal (complexUnif y x) state (Right (complexUnif 1 0))
    testEnumerateGoal (complexUnif x y) state' (Right (complexUnif 2 1))
    testEnumerateGoal (complexUnif x x) state' (Right (complexUnif 2 2))

simpleCall :: a -> G a
simpleCall x = call "simple" [V x]

manyArgCall :: a -> a -> a -> G a
manyArgCall x y z = call "many" [V x, V y, V z]

constructorInCall :: a -> a -> a -> G a
constructorInCall x y z = call "constructor" [V x, C "" [V y, C "" [V z]]]

unit_enumerateCall :: Assertion
unit_enumerateCall = do
    state <- assertIsJustState $ execStateT (do newVar x; newVar y) emptyState
    state' <- assertIsJustState $ execStateT (newVar x) state

    testEnumerateGoal (simpleCall x) state (Right $ simpleCall 0)
    testEnumerateGoal (simpleCall y) state (Right $ simpleCall 1)
    testEnumerateGoal (simpleCall z) state (Left "z undefined")

    testEnumerateGoal (manyArgCall x x x) state  (Right $ manyArgCall 0 0 0)
    testEnumerateGoal (manyArgCall x x x) state' (Right $ manyArgCall 2 2 2)

    testEnumerateGoal (manyArgCall x y x) state  (Right $ manyArgCall 0 1 0)
    testEnumerateGoal (manyArgCall x y x) state' (Right $ manyArgCall 2 1 2)

    testEnumerateGoal (manyArgCall y y x) state  (Right $ manyArgCall 1 1 0)
    testEnumerateGoal (manyArgCall y y x) state' (Right $ manyArgCall 1 1 2)

    testEnumerateGoal (manyArgCall x y z) state (Left "z undefined")

    testEnumerateGoal (constructorInCall x y x) state  (Right $ constructorInCall 0 1 0)
    testEnumerateGoal (constructorInCall x y x) state' (Right $ constructorInCall 2 1 2)
    testEnumerateGoal (constructorInCall x y z) state (Left "z undefined")


disj1 :: a -> a -> G a
disj1 x y = unsafeDisj [complexUnif x y, constUnif x]

disj2 :: a -> a -> G a
disj2 x y = unsafeDisj [complexUnif x x, complexUnif x y, complexUnif y x, complexUnif y y]

conj1 :: a -> a -> G a
conj1 x y = unsafeConj [complexUnif x y, constUnif x]

conj2 :: a -> a -> G a
conj2 x y = unsafeConj [complexUnif x x, complexUnif x y, complexUnif y x, complexUnif y y]


unit_enumerateDisjuncion :: Assertion
unit_enumerateDisjuncion = do
    state <- assertIsJustState $ execStateT (do newVar x; newVar y) emptyState
    state' <- assertIsJustState $ execStateT (newVar x) state

    testEnumerateGoal (disj1 x y) state  (Right (disj1 0 1))
    testEnumerateGoal (disj1 x y) state' (Right (disj1 2 1))
    testEnumerateGoal (disj2 x y) state  (Right (disj2 0 1))
    testEnumerateGoal (disj2 x y) state' (Right (disj2 2 1))

    testEnumerateGoal (disj1 z y) state  (Left "z undefined")
    testEnumerateGoal (disj1 x z) state' (Left "z undefined")
    testEnumerateGoal (disj2 z y) state  (Left "z undefined")
    testEnumerateGoal (disj2 x z) state' (Left "z undefined")


unit_enumerateConjunction :: Assertion
unit_enumerateConjunction = do
    state <- assertIsJustState $ execStateT (do newVar x; newVar y) emptyState
    state' <- assertIsJustState $ execStateT (newVar x) state

    testEnumerateGoal (conj1 x y) state  (Right (conj1 0 1))
    testEnumerateGoal (conj1 x y) state' (Right (conj1 2 1))
    testEnumerateGoal (conj2 x y) state  (Right (conj2 0 1))
    testEnumerateGoal (conj2 x y) state' (Right (conj2 2 1))

    testEnumerateGoal (conj1 z y) state  (Left "z undefined")
    testEnumerateGoal (conj1 x z) state' (Left "z undefined")
    testEnumerateGoal (conj2 z y) state  (Left "z undefined")
    testEnumerateGoal (conj2 x z) state' (Left "z undefined")


freshUnif :: a -> G a
freshUnif x = Fresh x (selfUnif x)

doubleFresh :: a -> a -> G a
doubleFresh x y = Fresh x (Fresh y (varUnif x y))

unit_enumerateFresh :: Assertion
unit_enumerateFresh = do
    state <- assertIsJustState $ execStateT (do newVar x; newVar y) emptyState
    state' <- assertIsJustState $ execStateT (newVar x) state

    testEnumerateGoal (freshUnif x) state (Right $ freshUnif 2)
    testEnumerateGoal (freshUnif y) state (Right $ freshUnif 2)
    testEnumerateGoal (freshUnif z) state (Right $ freshUnif 2)

    testEnumerateGoal (doubleFresh x y) state (Right $ doubleFresh 2 3)
    testEnumerateGoal (doubleFresh z y) state (Right $ doubleFresh 2 3)
    -- First fresh is not visible
    testEnumerateGoal (doubleFresh y y) state (Right $ Fresh 2 $ Fresh 3 $ selfUnif 3)

    addoState <- assertIsJustState $ execStateT (do newVar x; newVar y; newVar z) emptyState
    testEnumerateGoal (appendoBody x y z h t r) addoState (Right $ appendoBody 0 1 2 3 4 5)

    evaloState <- assertIsJustState $ execStateT (do newVar st; newVar fm; newVar u) emptyState
    testEnumerateGoal (evaloBody st fm u x y v w z) evaloState (Right $ evaloBody' 0 1 2 3 4 5 4 6 3)

testEnumerateGoal :: G X -> RenameState S -> Either String (G S) -> Assertion
testEnumerateGoal goal state exp =
  evalStateT (enumerate goal) state @?= exp

unit_enumerateDef :: Assertion
unit_enumerateDef = do
  testEnumerateDef  (Def "appendo" [x, y, z] (appendoBody x y z h t r))
                    emptyState
                    (Right $ Def "appendo" [0, 1, 2] (appendoBody 0 1 2 3 4 5))

  testEnumerateDef (Def "addo" [x, y, z] (addoBody x y z n))
                   emptyState
                   (Right $ Def "addo" [0, 1, 2] (addoBody 0 1 2 3))

  testEnumerateDef (Def "addo" [x, y, z] (addoBody' x y z n m))
                  emptyState
                  (Right $ Def "addo" [0, 1, 2] (addoBody' 0 1 2 3 4))

testEnumerateDef :: Def G X -> RenameState S -> Either String (Def G S) -> Assertion
testEnumerateDef def state exp =
  evalStateT (enumerateDef def) state @?= exp

addoBody :: a -> a -> a -> a -> G a
addoBody x y z n =
  unsafeDisj
  [ unsafeConj
    [ V x :=: C "O" []
    , V y :=: V z
    ]
  , fresh [n] $
      unsafeConj
      [ V x :=: C "S" [V n]
      , call "addo" [V n, C "S" [V y], V z]
      ]
  ]

addoBody' :: a -> a -> a -> a -> a -> G a
addoBody' x y z n m =
  unsafeDisj
  [ unsafeConj
    [ V x :=: C "O" []
    , V y :=: V z
    ]
  , fresh [n, m] $
      unsafeConj
      [ V x :=: C "S" [V n]
      , call "addo" [V n, V m, V z]
      , V m :=: C "S" [V y]
      ]
  ]


appendoBody :: a -> a -> a -> a -> a -> a -> G a
appendoBody x y z h t r =
  unsafeDisj
  [ unsafeConj
    [ V x :=: C "nil" []
    , V y :=: V z
    ]
  , fresh [h, t, r] $
      unsafeConj
      [ V x :=: C "cons" [V h, V t]
      , V z :=: C "cons" [V h, V r]
      , call "addo" [V t, V y, V r]
      ]
  ]

evaloBody :: a -> a -> a -> a -> a -> a -> a -> a -> G a
evaloBody st fm u x y v w z =
  unsafeDisj
  [ fresh [x, y, v, w] $
      unsafeConj
      [ V fm :=: C "conj" [V x, V y]
      , call "evalo" [V st, V x, V v]
      , call "evalo" [V st, V y, V w]
      , call "ando" [V v, V w, V u]
      ]
  , fresh [x, y, v, w] $
      unsafeConj
      [ V fm :=: C "disj" [V x, V y]
      , call "evalo" [V st, V x, V v]
      , call "evalo" [V st, V y, V w]
      , call "oro" [V v, V w, V u]
      ]
  , fresh [x, v] $
      unsafeConj
      [ V fm :=: C "neg" [V x]
      , call "evalo" [V st, V x, V v]
      , call "noto" [V v, V u]
      ]
  , fresh [z] $
      unsafeConj
      [ V fm :=: C "var" [V z]
      , call "elemo" [V z, V st, V u]
      ]
  ]


evaloBody' :: a -> a -> a -> a -> a -> a -> a -> a -> a -> G a
evaloBody' st fm u x y v v' w z =
  unsafeDisj
  [ fresh [x, y, v, w] $
      unsafeConj
      [ V fm :=: C "conj" [V x, V y]
      , call "evalo" [V st, V x, V v]
      , call "evalo" [V st, V y, V w]
      , call "ando" [V v, V w, V u]
      ]
  , fresh [x, y, v, w] $
      unsafeConj
      [ V fm :=: C "disj" [V x, V y]
      , call "evalo" [V st, V x, V v]
      , call "evalo" [V st, V y, V w]
      , call "oro" [V v, V w, V u]
      ]
  --
  , fresh [x, v'] $
      unsafeConj
      [ V fm :=: C "neg" [V x]
      , call "evalo" [V st, V x, V v']
      , call "noto" [V v', V u]
      ]
  , fresh [z] $
      unsafeConj
      [ V fm :=: C "var" [V z]
      , call "elemo" [V z, V st, V u]
      ]
  ]
