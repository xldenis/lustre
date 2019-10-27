{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Lust.Scheduling where

import           Data.Bifunctor       (first)
import           Data.Foldable
import           Data.Graph
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Maybe           (fromJust, fromMaybe)
import           Data.Set             (Set)
import qualified Data.Set             as Set

import           Control.Monad.Except

import           Lust.Error
import           Lust.Name
import           Lust.Pretty
import           Lust.Syntax

{- | Return the immediate dependencies of an expression
  this means anything not behind a delaying operation (fby).

-}
left :: Expression -> Set Ident
left (Const c)     = Set.empty
left (Arr c e)     = Set.empty
left (BinOp o l r) = left l <> left r
left (Not e)       = left e
left (Var i)       = Set.singleton i
left (Merge i l r) = Set.insert i (left l <> left r)
left (When e c x)  = Set.insert x (left e)
left (App _ as a)  = Set.insert a (Set.unions (map left as))

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

scheduleNode :: Node Clock -> Either (Error ann) (Node Clock)
scheduleNode n@MkNode{..} = first fromSchedulingError $ do
  let comps = stronglyConnCompR (concatMap (toList . scheduleEq) nodeEquations)
  eqns' <- foldrM checkSCC [] comps

  pure $ n { nodeEquations = eqns' }
  where

  -- | Because an equation can be in
  checkSCC (AcyclicSCC (v, _, _)) acc = if v `elem` acc then pure acc else pure (v : acc)
  checkSCC (CyclicSCC vs) acc = throwError . CausalityViolation $ map (\(_, i, _) -> i) vs

  scheduleEq :: Equation Clock -> NonEmpty (Equation Clock, Ident, [Ident])
  scheduleEq e@(MkEq _ ids exp) =  let
    deps  = Set.toList (left exp)
    in fmap (\i -> (e, i, deps)) ids
