{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitPrelude            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-# LANGUAGE UndecidableInstances       #-}
module Control.Monad.Fresh where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

{-
  A simple type class for monads which can produce fresh names

-}

class Monad m => MonadFresh m where
  freshName :: m Int

type Fresh a = FreshT Identity a

evalFresh :: Int -> Fresh a -> a
evalFresh i = runIdentity . evalFreshT i

evalFreshT :: Monad m => Int -> FreshT m a -> m a
evalFreshT i = flip evalStateT i . unFreshT

newtype FreshT m a = FreshT { unFreshT :: StateT Int m a }
  deriving (Functor, Applicative, Monad, MonadState Int, MonadTrans)

instance MonadFresh m => MonadFresh (StateT s m) where
  freshName = lift freshName

instance MonadFresh m => MonadFresh (ReaderT s m) where
  freshName = lift freshName

instance (Monoid w, MonadFresh m) => MonadFresh (WriterT w m) where
  freshName = lift freshName

instance MonadFresh m => MonadFresh (ExceptT e m) where
  freshName = lift freshName

instance Monad m => MonadFresh (FreshT m) where
  freshName = do
    next <- get
    modify (+ 1)
    return next

prefixedName :: MonadFresh m => String -> m String
prefixedName pre = (pre ++) . show <$> freshName
