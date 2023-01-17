module TaglessFinal.Eval where

import           Control.Monad.State
import qualified Stream
import           TaglessFinal.Term
import qualified TaglessFinal.Unify    as Unify
import           TaglessFinal.VarState

import           TaglessFinal.Syntax

newtype Eval a = Eval { runEval :: a }

instance Goal Eval where
  term = Eval

  unify t1 t2 = Eval $ do
    return $ \s -> Stream.maybeToStream $ Unify.unify (Just s) (runEval t1) (runEval t2)

  conj g1 g2 = Eval $ do
    g1 <- runEval g1
    g2 <- runEval g2
    return $ \s -> g1 s >>= g2

  disj g1 g2 = Eval $ do
    g1 <- runEval g1
    g2 <- runEval g2
    return $ \s -> g1 s `mplus` g2 s

  fresh f = Eval $ do
    v <- toFreshVar <$> nextFreshVar
    runEval (f v)

  lam f = Eval $ \x -> runEval (f (Eval x))

  app f x = Eval $ (runEval f) (runEval x)

  fixP s f = Eval $ fx (runEval . f . Eval) where fx f = f (fx f)
