module TranslatedExamples.BridgeTerm where
import Syntax(Term(V, C), Tx, Ts)
import Subst ( Subst, lookup )
import Stream
import Control.Monad (MonadPlus, guard)
import Data.Functor (($>))

inj :: (Show a) => a -> Tx
inj x = C (show x) []

data Person = PA | PB | PC | PD deriving (Show, Eq)

msum' :: (Foldable t, MonadPlus m) => t (m a) -> m a
msum' = foldr mplus mzero

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

toInt :: Ts -> Subst Int -> Maybe Int
toInt (V n) s = Subst.lookup n s >>= (`toInt` s)
toInt (C "zro" []) s = Just 0
toInt (C "suc" [t]) s = (+ 1) <$> toInt t s
toInt _ _ = Nothing

maxo :: Tx -> Tx -> Stream Tx
maxo a b = msum' [
        guard (a == zro) $> b,
        guard (b == zro) $> a,
        do
            let (C "suc" [a']) = a
            let (C "suc" [b']) = b
            out' <- maxo a' b'
            return (suc out')
    ]