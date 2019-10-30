{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections, ScopedTypeVariables    #-}
module Lust.Scheduling where

import           Data.Bifunctor            ( first )
import           Data.Foldable
import           Data.Graph
import           Data.List.NonEmpty        ( NonEmpty(..) )
import qualified Data.List.NonEmpty       as NE
import           Data.List                 ( sortOn
                                           , union
                                           )
import           Data.Maybe                ( fromJust )
import           Data.Set                  ( Set )
import qualified Data.Set                 as Set

import           Control.Monad.Except

import           Lust.Error
import           Lust.Name
import           Lust.Pretty
import           Lust.Syntax

{- | Return the immediate dependencies of an expression
  this means anything not behind a delaying operation (fby).

-}

left :: Expression -> Set Ident
left (Const _     ) = Set.empty
left (Arr _ _     ) = Set.empty
left (BinOp _ l r ) = left l <> left r
left (Not e       ) = left e
left (Var i       ) = Set.singleton i
left (Merge i l  r) = Set.insert i (left l <> left r)
left (When  e _  x) = Set.insert x (left e)
left (App   _ as a) = Set.insert a (Set.unions (map left as))
left (Tuple es    ) = Set.unions (fmap left es)

data SchedulingError
  = CausalityViolation [Ident]
  deriving (Show, Eq)

fromSchedulingError :: SchedulingError -> Error a
fromSchedulingError (CausalityViolation ids) = Error
  { errHeader  = pretty "Scheduling Error"
  , errSummary = pretty "These variables form a cycle in their reads" <+> hsep (map pretty ids)
  , errKind    = "scheduling"
  , errHints   = []
  }

{-| Scheduling sorts the equations in order according to their syntactic dependencies
    such that every operation that reads a value is evaluted after that value
-}
runScheduling :: Eq a => [Node a] -> Either (Error ann) [Node a]
runScheduling = mapM scheduleNode

scheduleNode :: forall  a ann . Eq a => Node a -> Either (Error ann) (Node a)
scheduleNode n@MkNode {..} = first fromSchedulingError $ do
  let graph = identsToVerts (concatMap scheduleEq nodeEquations)
      comps = stronglyConnComp graph
  eqns' <- foldlM (flip checkSCC) [] comps

  pure $ n { nodeEquations = eqns' `union` nodeEquations }
 where

  {- | Analyze each strongly connected component, if it's acyclic then we can
       continue, then we add it to the set of equations.

       Otherwise if we've found a cycle that means we can't find
       a linear schedule for the variables in the cycle.
  -}
  checkSCC :: SCC Ident -> [Equation a] -> Either SchedulingError [Equation a]
  checkSCC (AcyclicSCC v) acc =
    let eq = fromJust $ eqFromId v in if eq `elem` acc then pure acc else pure (eq : acc)
  checkSCC (CyclicSCC vs) _ = throwError . CausalityViolation $ map id vs

  -- | Lookup the equation that corresponds to an identifier
  eqFromId x = lookup x (map' nodeEquations)
   where
    map' (e@(MkEq _ ids _) : xs) = map (, e) (toList ids) ++ map' xs
    map' []                      = []

  -- | Generate the dependencies and anti-dependencies for an equation
  scheduleEq :: Eq a => Equation a -> [(Ident, Ident)]
  scheduleEq (MkEq _ (x :| []) (Arr _ e)) = let deps = Set.toList (left e) in map (, x) deps
  scheduleEq (MkEq _ ids exp) =
    let deps = Set.toList (left exp) in concatMap (\i -> map (i, ) deps) (toList ids)

  -- | Transform the list of equations into the form expected by Data.Graph
  identsToVerts :: [(Ident, Ident)] -> [(Ident, Ident, [Ident])]
  identsToVerts preE =
    let sorted_e = sortOn fst preE
        grouped  = NE.groupWith fst sorted_e
    in  map (\is -> (fst (NE.head is), fst (NE.head is), map snd $ toList is)) grouped

