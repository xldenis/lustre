-----------------------------------------------------------------------------
--
-- Module      :  Control.Monad.Unify
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitPrelude            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.Unify where

import           Data.Monoid

import           Control.Applicative
import           Control.Monad.Error.Class
import           Control.Monad.Fresh
import           Control.Monad.State
import           Control.Monad.Writer

import           Data.HashMap.Strict       as M

-- |
-- Untyped unification variables
--
type Unknown = Int

-- |
-- A type which can contain unification variables
--
class Partial t where
  unknown :: Unknown -> t
  isUnknown :: t -> Maybe Unknown
  unknowns :: t -> [Unknown]
  ($?) :: Substitution t -> t -> t

-- |
-- Identifies types which support unification
--
class (Partial t, MonadError e m, MonadFresh m) => MonadUnify e t m | t -> e where
  (=?=) :: t -> t -> m ()
  getSubst :: m (Substitution t)
  setSubst :: Substitution t -> m ()
-- |
-- A substitution maintains a mapping from unification variables to their values
--
data Substitution t = Substitution { runSubstitution :: M.HashMap Int t }
  deriving (Show)

instance (Partial t) => Monoid (Substitution t) where
  mempty = Substitution M.empty

instance Partial t => Semigroup (Substitution t) where
  s1 <> s2 = Substitution $
              M.map (s2 $?) (runSubstitution s1) `M.union`
              M.map (s1 $?) (runSubstitution s2)

-- |
-- State required for type checking
--
data UnifyState t = UnifyState {
  -- |
  -- The next fresh unification variable
  --
    unifyNextVar             :: Int
  -- |
  -- The current substitution
  --
  , unifyCurrentSubstitution :: Substitution t
  }

-- |
-- An empty @UnifyState@
--
defaultUnifyState :: (Partial t) => UnifyState t
defaultUnifyState = UnifyState 0 mempty

class UnificationError t e where
  occursCheckFailed :: t -> e

-- |
-- The type checking monad, which provides the state of the type checker, and error reporting capabilities
--
newtype UnifyT t m a = UnifyT { unUnify :: StateT (UnifyState t) m a }
  deriving (Functor, Monad, Applicative, Alternative, MonadPlus)

instance (MonadState s m) => MonadState s (UnifyT t m) where
  get = UnifyT . lift $ get
  put = UnifyT . lift . put

instance (MonadError e m) => MonadError e (UnifyT t m) where
  throwError = UnifyT . throwError
  catchError e f = UnifyT $ catchError (unUnify e) (unUnify . f)

instance (MonadWriter w m) => MonadWriter w (UnifyT t m) where
  tell = UnifyT . tell

-- |
-- Run a computation in the Unify monad, failing with an error, or succeeding with a return value and the new next unification variable
--
runUnify :: UnifyState t -> UnifyT t m a -> m (a, UnifyState t)
runUnify s = flip runStateT s . unUnify

-- |
-- Substitute a single unification variable
--
substituteOne :: (Partial t) => Unknown -> t -> Substitution t
substituteOne u t = Substitution $ M.singleton u t

-- |
-- Replace a unification variable with the specified value in the current substitution
--
(=:=) :: (UnificationError t e, Monad m, MonadUnify e t m) => Unknown -> t -> m ()
(=:=) u t' = do
  sub <- getSubst
  let t = sub $? t'
  occursCheck u t
  let current = sub $? unknown u
  case isUnknown current of
    Just u1 | u1 == u -> return ()
    _       -> current =?= t
  s <- getSubst
  setSubst (substituteOne u t <> s)

-- |
-- Perform the occurs check, to make sure a unification variable does not occur inside a value
--
occursCheck :: (UnificationError t e, Monad m, MonadError e m, Partial t, MonadUnify e t m) => Unknown -> t -> m ()
occursCheck u t =
  case isUnknown t of
    Nothing -> when (u `elem` unknowns t) $ throwError $ occursCheckFailed t
    _       -> return ()

instance Monad m => MonadFresh (UnifyT t m) where
  freshName = do
    st <- UnifyT get
    UnifyT $ modify $ \s -> s { unifyNextVar = succ (unifyNextVar s) }
    return $ unifyNextVar st

-- |
-- Generate a fresh unification variable at a specific type
--
fresh :: (MonadFresh m, Monad m, Partial t, MonadUnify e t m) => m t
fresh = do
  unknown <$> freshName


