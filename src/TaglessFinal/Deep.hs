module TaglessFinal.Deep where


import           TaglessFinal.Syntax
import           TaglessFinal.Term

import           Control.Monad.State
import qualified Syntax                as S
import           TaglessFinal.VarState

newtype Deep a = Deep { unDeep :: State VarState (Either (S.Term S.X) (S.G S.X)) }

fromLeft :: Either a b -> a
fromLeft (Left a) = a

fromRight :: Either a b -> b
fromRight (Right b) = b

instance Goal Deep where
  term x = Deep $ return $ Left (toSyntax x)
  unify x y = Deep $ do
    x <- unDeep x
    y <- unDeep y
    return $ Right $ (S.===) (fromLeft x) (fromLeft y)
  conj x y = Deep $ do
    x <- unDeep x
    y <- unDeep y
    return $ Right $ (S.&&&) (fromRight x) (fromRight y)
  disj x y = Deep $ do
    x <- unDeep x
    y <- unDeep y
    return $ Right $ (S.|||) (fromRight x) (fromRight y)
  fresh f = Deep $ do
    v <- nextFreshVar
    let x = toFreshVar v
    body <- unDeep $ f x
    return $ Right $ S.Fresh x (fromRight body)

  lam f = Deep $ do
    v <- nextVar
    let x = toVar v
    body <- unDeep $ f (Deep $ return (Left $ toSyntax $ Var x))
    return body

  fixP name f = Deep $ do
    body <- unDeep $ f (Deep $ return (Left $ toSyntax $ Var name))
    return body

  app f arg = Deep $ do
    application <- unDeep f
    arg <- unDeep arg
    return $ application

view :: Deep a -> S.G S.X
view goal = fromRight $ evalState (unDeep goal) (VarState 0 0)