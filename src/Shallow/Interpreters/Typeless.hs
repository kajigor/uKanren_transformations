{-# LANGUAGE TypeFamilies, DeriveFunctor, FlexibleInstances, ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
module Shallow.Interpreters.Typeless(
    Typeless, KVar(SynVar),
    toTypeless, toTypeless',
    freeArg, namedArg,
    ) where


import Shallow.Core.Internal.Kanren
import Shallow.Core.Internal.Logic
import qualified Def as Def
import qualified Syntax as S
import qualified Data.Map as Map
import Control.Monad.State
import Shallow.Utils.Kanren (embed)


type BaseVar = S.S
type Term = S.Term BaseVar
type Def = Def.Def S.G BaseVar

toTerm' :: (LogicType a) => Reified a (KVar Typeless) -> S.Term BaseVar
toTerm' x = let (con, elems) = quote x in S.C (name con) $ map (\(Field _ x') -> toTerm x') elems

toTerm :: (LogicType a) => Logic a (KVar Typeless) -> S.Term BaseVar
toTerm (Free v) = S.V (unsyn v)
toTerm (Ground x) = toTerm' x


data BaseKState = BaseKState { nextVar :: Int, defs :: Map.Map String Def, args :: [(BaseVar, Term)] }

nullState :: BaseKState
nullState = BaseKState { nextVar = 0, defs = Map.empty, args = [] }

data RaisedGoal t = RG { goal :: S.G BaseVar, value :: [t] } deriving (Show, Eq, Functor)

g :: S.G BaseVar -> RaisedGoal ()
g x = RG { goal = x, value = [()] }

clearingConj :: Eq a => S.G a -> S.G a -> S.G a
clearingConj g1 g2 | g1 == S.success = g2
                   | g2 == S.success = g1
                   | otherwise = S.flatConj g1 g2

instance Applicative RaisedGoal where

    pure x = RG { goal = S.success, value = [x] }

    f <*> x = RG { goal = clearingConj (goal f) (goal x), value = value f <*> value x }

clearingDisj :: Eq a => S.G a -> S.G a -> S.G a
clearingDisj g1 g2 | g1 == S.failure = g2
                   | g2 == S.failure = g1
                   | otherwise = S.flatDisj g1 g2

instance Alternative RaisedGoal where

    empty = RG { goal = S.failure, value = [] }

    x <|> y = RG { goal = clearingDisj (goal x) (goal y), value = value x <|> value y }

type Computation = State BaseKState

updateDefs :: Def -> Map.Map String Def -> Map.Map String Def
updateDefs d m = Map.insert (Def.getName d) d m

hasDef :: String -> Map.Map String Def -> Bool
hasDef = Map.member

data Typeless t = PK { runBodylessPK :: Computation (), runPK :: Computation (RaisedGoal t) } deriving (Functor)

body :: Computation (RaisedGoal t) -> Typeless t
body = PK (pure ())

instance Applicative Typeless where

    pure x = PK (pure ()) (pure $ pure x)

    (PK f h) <*> (PK x y) = PK (f *> x) (liftA2 (<*>) h y)

instance Alternative Typeless where

    empty = PK (pure ()) (pure empty)

    (PK x a) <|> (PK y b) = PK (x *> y) (liftA2 (<|>) a b)

markerDef :: String -> Def.Def g a
markerDef n = Def.Def n (error "Marker def args accessed") (error "Marker def goal accessed")

integrateDef :: String -> Typeless t -> Map.Map String Def -> Map.Map String Def
integrateDef n r m | hasDef n m = m
                   | otherwise = updateDefs def (defs s')
    where
        (rg, s') = runState (runPK r) (nullState { defs = updateDefs (markerDef n) m })
        def = Def.Def n (fst <$> args s') (goal rg)

extractArgs :: Typeless t -> [Term]
extractArgs r = snd <$> args s
    where
        s = execState (runBodylessPK r) nullState

addFresh :: FreshType rel a -> BaseVar -> RaisedGoal t -> RaisedGoal t
addFresh FreshVar v rg = rg { goal = S.Fresh v (goal rg) }
addFresh (ArgVar _) _ rg = rg

instance Kanren Typeless where

    newtype instance (KVar Typeless) t = SynVar { unsyn :: BaseVar } deriving (Eq, Ord, Show, Functor)

    fresh_ x f = PK (doFresh >>= runBodylessPK) (doFresh >>= runPK) 
        where
            updateArgs FreshVar _ s = args s
            updateArgs (ArgVar x') v s = args s ++ [(v, toTerm x')]

            doFresh = do
                v <- gets nextVar
                modify $ \s -> s { nextVar = succ v, args = updateArgs x v s }
                let (PK a b) =  f (SynVar v)
                pure $ PK a (addFresh x v <$> b)
    unify a b = body $ pure $ g $ (toTerm a) S.=== (toTerm b)
    call_ _ (Relation n pk) = body $ do
        modify $ \s -> s { defs = integrateDef n pk (defs s) }
        pure $ g $ S.call n (extractArgs pk)
    
    displayVar (SynVar x) = show x


toTypeless :: Relation Typeless -> [Def]
toTypeless r = Map.elems $ defs $ execState (runPK $ embed r) nullState

toTypeless' :: Relation Typeless -> ([Def], [Term])
toTypeless' (Relation n r) = (Map.elems $ updateDefs tld (defs s), extractArgs r)
    where
        (rg, s) = runState (runPK r) nullState
        tld = Def.Def (n ++ "_goal") (fst <$> args s) (goal rg)

freeArg :: forall a. Logic a (KVar Typeless)
freeArg = error "Free arg accessed"

namedArg :: forall a. Int -> Logic a (KVar Typeless)
namedArg n = Free $ SynVar n
