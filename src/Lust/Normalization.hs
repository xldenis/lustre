{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Lust.Normalization where

import           Control.Monad.Fresh
import           Control.Monad.Writer
import           Control.Monad.Reader

import           Lust.Clocks
import           Lust.Name
import           Lust.Syntax

type NormalizeM m = (MonadFresh m, MonadWriter [Equation Clock] m)

runNormalize :: [Node Clock] -> [Node Clock]
runNormalize ns =
  evalFresh 0
  $ forM ns $ \n -> do
    (n', aux) <- runWriterT $ normalizeNode n

    pure (n' { nodeEquations = aux ++ nodeEquations n' })

emit :: (MonadReader Clock m, NormalizeM m) => Expression -> m Expression
emit exp = do
  nm <- prefixedName "norm_"
  clk <- ask
  tell [MkEq clk (pure (MkI nm)) exp]

  pure (Var (MkI nm))

normalizeNode :: NormalizeM m => Node Clock -> m (Node Clock)
normalizeNode n@MkNode{..} = do
  eqns' <- mapM normalizeEqn nodeEquations

  pure (n { nodeEquations = eqns' })

normalizeEqn :: NormalizeM m => Equation Clock -> m (Equation Clock)
normalizeEqn (MkEq c ids e) = flip runReaderT c $ do
  e' <- normalizeExpr e

  pure (MkEq c ids e')

normalizeExpr ::(MonadReader Clock m, NormalizeM m) => Expression -> m Expression
normalizeExpr e@(Const c) = pure e
normalizeExpr (Arr c e) = do
  e' <- normalizeExpr e >>= emit

  pure (Arr c e')
normalizeExpr (BinOp o l r) = do
  l' <- normalizeExpr l
  r' <- normalizeExpr r

  pure (BinOp o l' r')
normalizeExpr (Not e) = do
  e' <- normalizeExpr e
  pure (Not e')
normalizeExpr e@(Var i) = pure e
normalizeExpr (Merge i l r) = do
  l' <- normalizeExpr l >>= emit
  r' <- normalizeExpr r >>= emit

  pure (Merge i l' r')
normalizeExpr (When e c x)  = do
  e' <- normalizeExpr e

  pure (When e' c x)
normalizeExpr (App f args a) = do
  args' <- mapM normalizeExpr args

  emit (App f args' a)
