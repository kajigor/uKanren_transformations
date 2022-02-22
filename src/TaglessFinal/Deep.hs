module TaglessFinal.Deep where


import TaglessFinal.Syntax
import TaglessFinal.Term

import Text.Printf ( printf )
import Control.Monad.State
import TaglessFinal.VarState
import qualified Syntax

newtype Deep a = Deep { unDeep :: State VarState (Either (Syntax.Tx) (Syntax.G Syntax.X)) }

fromLeft :: Either a b -> a
fromLeft (Left a) = a

fromRight :: Either a b -> b
fromRight (Right b) = b

instance Goal Deep where
  term x = Deep $ return $ Left (toSyntax x)
  unify x y = Deep $ do
    x <- unDeep x
    y <- unDeep y
    return $ Right $ (Syntax.===) (fromLeft x) (fromLeft y)
  conj x y = Deep $ do
    x <- unDeep x
    y <- unDeep y
    return $ Right $ (Syntax.&&&) (fromRight x) (fromRight y)
  disj x y = Deep $ do
    x <- unDeep x
    y <- unDeep y
    return $ Right $ (Syntax.|||) (fromRight x) (fromRight y)
  fresh f = Deep $ do
    v <- nextFreshVar
    let x = toFreshVar v
    body <- unDeep $ f x
    return $ Right $ Syntax.Fresh x (fromRight body)

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

view :: Deep a -> Syntax.G Syntax.X
view goal = fromRight $ evalState (unDeep goal) (VarState 0 0)