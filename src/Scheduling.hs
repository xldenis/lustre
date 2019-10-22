{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Scheduling where

import           Clocks
import           Name
import           Syntax               (Equation (..), Node (..))

import           Data.Set             (Set)
import qualified Data.Set             as Set

import           Data.Graph
import           Data.Hashable

import           Data.Maybe           (fromJust, fromMaybe)

import           Control.Monad.Except

left :: ClockAnn -> Set Ident
left (C _ (Const c))     = Set.empty
left (C _ (Arr c e))     = Set.empty
left (C _ (BinOp o l r)) = left l <> left r
left (C _ (Not e))       = left e
left (C _ (Var i))       = Set.singleton i
left (C _ (Merge i l r)) = Set.insert i (left l <> left r)
left (C _ (When e c x))  = Set.insert x (left e)

def :: Equation ClockAnn -> [Ident]
def (MkEq x (C _ (Arr _ _))) = []
def (MkEq x a)               = x

scheduleNode :: Node ClockAnn -> Either () (Node ClockAnn)
scheduleNode n@MkNode{..} = do
  let comps = stronglyConnComp (map scheduleEq nodeEquations)
  eqns' <- mapM checkSCC comps

  pure $ n { nodeEquations = eqns' }
  where
  checkSCC (AcyclicSCC v) = pure v
  checkSCC (CyclicSCC vs) = throwError ()

  eqIds = nodeEquations >>= \(MkEq ids _) -> map (\ix -> (ix, hash ids)) ids

  scheduleEq :: Equation ClockAnn -> (Equation ClockAnn, Int, [Int])
  scheduleEq e@(MkEq ids exp) =  let
    deps  = Set.toList (left exp)
    deps' = map (\ix -> fromMaybe 0 (lookup ix eqIds)) deps
    in (e, hash ids, deps')
