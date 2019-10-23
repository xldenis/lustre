{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Lust.Scheduling where


import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Graph
import           Data.Hashable
import           Data.Maybe           (fromJust, fromMaybe)
import           Data.Foldable
import           Data.Bifunctor       (first)

import           Control.Monad.Except

import           Lust.Clocks
import           Lust.Name
import           Lust.Syntax               (Equation (..), Node (..))
import           Lust.Error
import           Lust.Pretty

{- | Return the immediate dependencies of an expression
  this means anything not behind a delaying operation (fby).

-}
left :: ClockAnn -> Set Ident
left (C _ (Const c))     = Set.empty
left (C _ (Arr c e))     = Set.empty
left (C _ (BinOp o l r)) = left l <> left r
left (C _ (Not e))       = left e
left (C _ (Var i))       = Set.singleton i
left (C _ (Merge i l r)) = Set.insert i (left l <> left r)
left (C _ (When e c x))  = Set.insert x (left e)
left (C _ (App _ as a))  = Set.insert a (Set.unions (map left as))

def :: Equation ClockAnn -> [Ident]
def (MkEq x (C _ (Arr _ _))) = []
def (MkEq x a)               = x

data SchedulingError
  = CausalityViolation [Ident]
  deriving (Show, Eq)

fromSchedulingError (CausalityViolation ids) = Error
  { errHeader = pretty "Scheduling Error"
  , errSummary =
      pretty "These variables form a cycle in their reads" <+> hsep (map pretty ids)
  , errKind = "scheduling"
  , errHints = []
  }

{-| Scheduling sorts the equations in order according to their syntactic dependencies
    such that every operation that reads a value is evaluted after that value
-}

scheduleNode :: Node ClockAnn -> Either (Error ann) (Node ClockAnn)
scheduleNode n@MkNode{..} = first fromSchedulingError $ do
  let comps = stronglyConnCompR (concatMap scheduleEq nodeEquations)
  eqns' <- foldrM checkSCC [] comps

  pure $ n { nodeEquations = eqns' }
  where

  -- | Because an equation can be in
  checkSCC (AcyclicSCC (v, _, _)) acc = if v `elem` acc then pure acc else pure (v : acc)
  checkSCC (CyclicSCC vs) acc = throwError . CausalityViolation $ map (\(_, i, _) -> i) vs

  eqIds = nodeEquations >>= \(MkEq ids _) -> map (\ix -> (ix, hash ids)) ids

  scheduleEq :: Equation ClockAnn -> [(Equation ClockAnn, Ident, [Ident])]
  scheduleEq e@(MkEq ids exp) =  let
    deps  = Set.toList (left exp)
    -- deps' = map (\ix -> fromMaybe 0 (lookup ix eqIds)) deps
    in map (\i -> (e, i, deps)) ids
