{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Normalization where

import           Control.Monad.Fresh
import           Control.Monad.Writer

import           Clocks
import           Name
import           Syntax               (Equation (..), Node (..))

type NormalizeM m = (MonadFresh m, MonadWriter [Equation ClockAnn] m)

runNormalize :: [Node ClockAnn] -> [Node ClockAnn]
runNormalize ns =
  evalFresh 0
  $ forM ns $ \n -> do
    (n', aux) <- runWriterT $ normalizeNode n

    pure (n' { nodeEquations = aux ++ nodeEquations n' })

emit :: NormalizeM m => ClockAnn -> m ClockAnn
emit exp = do
  nm <- prefixedName "norm_"
  tell [MkEq [MkI nm] exp]

  pure (C (clockOf exp) (Var (MkI nm)))
  where clockOf (C clk _) = clk

normalizeNode :: NormalizeM m => Node ClockAnn -> m (Node ClockAnn)
normalizeNode n@MkNode{..} = do
  eqns' <- mapM normalizeEqn nodeEquations

  pure (n { nodeEquations = eqns' })

normalizeEqn (MkEq ids e) = do
  e' <- normalizeExpr e

  pure (MkEq ids e')

normalizeExpr :: NormalizeM m => ClockAnn -> m ClockAnn
normalizeExpr e@(C _ (Const c)) = pure e
normalizeExpr (C clk (Arr c e)) = do
  e' <- normalizeExpr e >>= emit

  pure (C clk (Arr c e'))
normalizeExpr (C clk (BinOp o l r)) = do
  l' <- normalizeExpr l
  r' <- normalizeExpr r

  pure (C clk (BinOp o l' r'))
normalizeExpr (C clk (Not e)) = do
  e' <- normalizeExpr e
  pure (C clk (Not e'))
normalizeExpr e@(C clk (Var i)) = pure e
normalizeExpr (C clk (Merge i l r)) = do
  l' <- normalizeExpr l >>= emit
  r' <- normalizeExpr r >>= emit

  pure (C clk (Merge i l' r'))
normalizeExpr (C clk (When e c x))  = do
  e' <- normalizeExpr e

  pure (C clk (When e' c x))
normalizeExpr (C clk (App f args a)) = do
  args' <- mapM normalizeExpr args

  e <- emit (C clk (App f args' a))

  pure e
