{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Lust.Normalization where

import           Control.Monad.Fresh
import           Control.Monad.Writer
import           Control.Monad.Reader

import           Data.List.NonEmpty        ( NonEmpty(..)
                                           , nonEmpty
                                           , toList
                                           , fromList
                                           )

import           Lust.Typing.Clocks
import           Lust.Name
import           Lust.Syntax


type NormalizeM m = (MonadFresh m, MonadWriter [Clocked Equation] m)

runNormalize :: [Clocked Node] -> [Clocked Node]
runNormalize ns = evalFresh 0 $ forM ns $ \n -> do
  (n', aux) <- runWriterT $ normalizeNode n

  pure (n' { nodeEquations = aux ++ nodeEquations n' })

emit :: (MonadReader (Type, Clock) m, NormalizeM m) => Expression -> m Expression
emit exp = do
  ann <- ask
  nm  <- MkI <$> prefixedName "norm_"
  tell [MkEq ann (pure nm) exp]

  pure (Var nm)

normalizeNode :: NormalizeM m => Clocked Node -> m (Clocked Node)
normalizeNode n@MkNode {..} = do
  eqns' <- mapM normalizeEqn nodeEquations

  pure (n { nodeEquations = eqns' })

normalizeEqn :: NormalizeM m => Clocked Equation -> m (Clocked Equation)
normalizeEqn (MkEq c ids (App f args a)) = flip runReaderT c $ do
  args' <- mapM normalizeExpr args

  pure (MkEq c ids (App f args' a))
normalizeEqn (MkEq c ids e) = flip runReaderT c $ do
  e' <- normalizeExpr e

  pure (MkEq c ids e')

normalizeExpr :: (MonadReader (Type, Clock) m, NormalizeM m) => Expression -> m Expression
normalizeExpr e@(Const c) = pure e
normalizeExpr (  Arr c e) = do
  e' <- normalizeExpr e >>= emit

  pure (Arr c e')
normalizeExpr (BinOp o l r) = do
  l' <- normalizeExpr l
  r' <- normalizeExpr r

  pure (BinOp o l' r')
normalizeExpr (Not e) = do
  e' <- normalizeExpr e
  pure (Not e')
normalizeExpr e@(Var i      ) = pure e
normalizeExpr (  Merge i l r) = do
  l' <- normalizeExpr l >>= emit
  r' <- normalizeExpr r >>= emit

  pure (Merge i l' r')
normalizeExpr (When e c x) = do
  e' <- normalizeExpr e

  pure (When e' c x)
normalizeExpr (App f args a) = do
  args' <- mapM normalizeExpr args

  emit (App f args' a)
