module TranslatedExamples.EvalLoop where

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


nil :: Tx
nil = C "nil" []

cons :: Tx -> Tx -> Tx
cons h t = C "cons" [h, t]

step :: Tx -> Tx -> Tx -> G X
step s m s' = unsafeDisj [
    s === inj True &&& m === inj True &&& s' === inj False,
    s === inj False &&& m === inj False &&& s' === inj True
  ]

evalLoop :: Tx -> Tx -> Tx -> G X
evalLoop state moves state' =
  unsafeDisj
    [ moves === nil &&& (state === state'),
      fresh3' "eval" $ \move moves' state'' -> moves === cons move moves' &&& step state move state'' &&& call "evalLoop" [state'', moves', state']
    ]

evalLoopDef :: Def G X
evalLoopDef = Def "evalLoop" ["state", "moves", "state'"] (evalLoop (V "state") (V "moves") (V "state'"))

mainLoop = mapM_
    (print . showSubst') 
    (takeS 20 $ run $ Program [evalLoopDef] (fresh3' "main" $ \m s m' -> step (inj True) m s &&& evalLoop s m' (inj False)))
