{-# LANGUAGE DeriveFunctor #-}
module Mode.Term where

newtype Var a = Var { getVar :: a }
              deriving (Show, Eq, Ord, Functor)

data FlatTerm a = FTCon String [Var a]
                | FTVar (Var a)
                deriving (Show, Eq, Ord, Functor)

varsFromTerm :: FlatTerm a -> [Var a]
varsFromTerm (FTVar v) = [v]
varsFromTerm (FTCon _ args) = args