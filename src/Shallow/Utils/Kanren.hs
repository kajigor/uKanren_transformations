{-# LANGUAGE Rank2Types, ApplicativeDo #-}
module Shallow.Utils.Kanren
(
  flatteningUnify, evalFromRead
, fresh, fresh2, fresh3, fresh4, fresh5
, argument, argument2, argument3, argument4, argument5
, relation, relation2, relation3, relation4, relation5
, call, embed
, eval
, run, run2, run3, run4, run5
, (===), (<=>)
, conde
, Var', L
, occursCheck
) where
import Shallow.Core.Internal.Kanren
import Shallow.Core.Internal.Logic
import Control.Applicative (asum)
import Data.Maybe (fromMaybe)

type Var' a rel = Var a (KVar rel)
type L a rel = Logic a (KVar rel)

flatteningUnify :: (Alternative rel, LogicType a) => (forall a'. (LogicType a') => Var' a' rel -> L a' rel -> rel ()) -> L a rel -> L a rel -> rel ()
flatteningUnify unifyVar (Free a) b = unifyVar a b
flatteningUnify unifyVar a (Free b) = unifyVar b a
flatteningUnify unifyVar (Ground a) (Ground b) = unifyVal (flatteningUnify unifyVar) a b

evalFromRead :: (Kanren rel, LogicType a) => (forall a'. (LogicType a') => Var' a' rel -> rel (Maybe a')) -> L a rel -> rel a
evalFromRead readVar (Ground x) = derefVal (evalFromRead readVar) x
evalFromRead readVar (Free v) = do
    x <- readVar v
    pure $ fromMaybe (error $ "Unbound variable: " ++ displayVar v) x

fresh :: (Kanren rel, LogicType a) => (L a rel -> rel s) -> rel s
fresh f = fresh_ FreshVar $ f . Free

fresh2 :: (Kanren rel, LogicType a, LogicType b) => (L a rel -> L b rel -> rel s) -> rel s
fresh2 f = fresh $ \a -> fresh $ \b -> f a b

fresh3 :: (Kanren rel, LogicType a, LogicType b, LogicType c) => (L a rel -> L b rel -> L c rel -> rel s) -> rel s
fresh3 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> f a b c

fresh4 :: (Kanren rel, LogicType a, LogicType b, LogicType c, LogicType d) => (L a rel -> L b rel -> L c rel -> L d rel -> rel s) -> rel s
fresh4 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> fresh $ \d -> f a b c d

fresh5 :: (Kanren rel, LogicType a, LogicType b, LogicType c, LogicType d, LogicType e) => (L a rel -> L b rel -> L c rel -> L d rel -> L e rel -> rel s) -> rel s
fresh5 f = fresh $ \a -> fresh $ \b -> fresh $ \c -> fresh $ \d -> fresh $ \e -> f a b c d e

argument :: (Kanren rel, LogicType a) => L a rel -> (L a rel -> rel s) -> rel s
argument x f = fresh_ (ArgVar x) $ f . Free

argument2 :: (Kanren rel, LogicType a, LogicType b) => L a rel -> L b rel -> (L a rel -> L b rel -> rel s) -> rel s
argument2 a_ b_ f = argument a_ $ \a -> argument b_ $ \b -> f a b

argument3 :: (Kanren rel, LogicType a, LogicType b, LogicType c) => L a rel -> L b rel -> L c rel -> (L a rel -> L b rel -> L c rel -> rel s) -> rel s
argument3 a_ b_ c_ f = argument a_ $ \a -> argument b_ $ \b -> argument c_ $ \c -> f a b c

argument4 :: (Kanren rel, LogicType a, LogicType b, LogicType c, LogicType d) => L a rel -> L b rel -> L c rel -> L d rel -> (L a rel -> L b rel -> L c rel -> L d rel -> rel s) -> rel s
argument4 a_ b_ c_ d_ f = argument a_ $ \a -> argument b_ $ \b -> argument c_ $ \c -> argument d_ $ \d -> f a b c d

argument5 :: (Kanren rel, LogicType a, LogicType b, LogicType c, LogicType d, LogicType e) => L a rel -> L b rel -> L c rel -> L d rel -> L e rel -> (L a rel -> L b rel -> L c rel -> L d rel -> L e rel -> rel s) -> rel s
argument5 a_ b_ c_ d_ e_ f = argument a_ $ \a -> argument b_ $ \b -> argument c_ $ \c -> argument d_ $ \d -> argument e_ $ \e -> f a b c d e

relation :: (Kanren rel, LogicType a) => String -> (L a rel -> rel ()) -> L a rel -> Relation rel
relation n f a_ = Relation n $ argument a_ f

relation2 :: (Kanren rel, LogicType a, LogicType b) => String -> (L a rel -> L b rel -> rel ()) -> L a rel -> L b rel -> Relation rel
relation2 n f a_ b_ = Relation n $ argument2 a_ b_ f

relation3 :: (Kanren rel, LogicType a, LogicType b, LogicType c) => String -> (L a rel -> L b rel -> L c rel -> rel ()) -> L a rel -> L b rel -> L c rel -> Relation rel
relation3 n f a_ b_ c_ = Relation n $ argument3 a_ b_ c_ f

relation4 :: (Kanren rel, LogicType a, LogicType b, LogicType c, LogicType d) => String -> (L a rel -> L b rel -> L c rel -> L d rel -> rel ()) -> L a rel -> L b rel -> L c rel -> L d rel -> Relation rel
relation4 n f a_ b_ c_ d_ = Relation n $ argument4 a_ b_ c_ d_ f

relation5 :: (Kanren rel, LogicType a, LogicType b, LogicType c, LogicType d, LogicType e) => String -> (L a rel -> L b rel -> L c rel -> L d rel -> L e rel -> rel ()) -> L a rel -> L b rel -> L c rel -> L d rel -> L e rel -> Relation rel
relation5 n f a_ b_ c_ d_ e_ = Relation n $ argument5 a_ b_ c_ d_ e_ f

call :: (Kanren rel) => Relation rel -> rel ()
call = call_ Opaque

embed :: (Kanren rel) => Relation rel -> rel ()
embed = call_ Transparent

eval :: (KanrenEval rel, LogicType a) => L a rel -> rel a
eval (Free v) = derefVar v
eval (Ground x) = derefVal eval x

run :: (KanrenEval rel, LogicType a) => (L a rel -> Relation rel) -> rel a
run f = fresh $ \x -> do
    _ <- embed $ f x
    x' <- eval x
    return x'

run2 :: (KanrenEval rel, LogicType a, LogicType b) => (L a rel -> L b rel -> Relation rel) -> rel (a, b)
run2 f = fresh2 $ \x y -> do
    _ <- embed $ f x y
    a <- eval x
    b <- eval y
    return (a, b)

run3 :: (KanrenEval rel, LogicType a, LogicType b, LogicType c) => (L a rel -> L b rel -> L c rel -> Relation rel) -> rel (a, b, c)
run3 f = fresh3 $ \x y z -> do
    _ <- embed $ f x y z
    a <- eval x
    b <- eval y
    c <- eval z
    return (a, b, c)

run4 :: (KanrenEval rel, LogicType a, LogicType b, LogicType c, LogicType d) => (L a rel -> L b rel -> L c rel -> L d rel -> Relation rel) -> rel (a, b, c, d)
run4 f = fresh4 $ \x y z w -> do
    _ <- embed $ f x y z w
    a <- eval x
    b <- eval y
    c <- eval z
    d <- eval w
    return (a, b, c, d)

run5 :: (KanrenEval rel, LogicType a, LogicType b, LogicType c, LogicType d, LogicType e) => (L a rel -> L b rel -> L c rel -> L d rel -> L e rel -> Relation rel) -> rel (a, b, c, d, e)
run5 f = fresh5 $ \x y z w q -> do
    _ <- embed $ f x y z w q
    a <- eval x
    b <- eval y
    c <- eval z
    d <- eval w
    e <- eval q
    return (a, b, c, d, e)


(===) :: (LogicType a, Kanren rel) => L a rel -> Reified a (KVar rel) -> rel ()
a === b = unify a (Ground b)

(<=>) :: (LogicType a, Kanren rel) => L a rel -> L a rel -> rel ()
a <=> b = unify a b

conde :: (Kanren rel) => [rel a] -> rel a
conde = asum

occursCheck :: (LogicType b, EqVar rel) => Var' a rel -> L b rel -> Bool
occursCheck x (Free y) = x `varEq` y
occursCheck x (Ground y) = let (_, ys) = quote y in any (\(Field _ y') -> occursCheck x y') ys