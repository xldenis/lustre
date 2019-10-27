{-# LANGUAGE RecordWildCards #-}
module Lust.Machine where

import           Lust.Name
import qualified Lust.Syntax         as S

import           Control.Monad
import           Control.Monad.Fresh
import           Data.List           (sortOn)
import           Data.List.NonEmpty  (NonEmpty (..))

type ParamList = [(Ident, MType)]
type MemoryEnv = [(Ident, S.Const)]

newtype MType = MkT S.Type
  deriving (Show, Eq)

data ClassDef = Class
  { className      :: Ident
  , classInstances :: [(Ident, Ident)] -- References to any nodes called internally!
  , classMemory    :: MemoryEnv
  , classReset     :: MachExpr
  , classStep      :: (ParamList, ParamList, MachExpr)
  } deriving (Show, Eq)

data MachExpr
  = AssignLocal Ident MachSimpleExpr
  | AssignState Ident MachSimpleExpr
  | Skip
  | Seq MachExpr MachExpr
  | Case MachSimpleExpr [(Ident, MachExpr)]
  | Reset Ident
  | Step (NonEmpty Ident) Ident [MachSimpleExpr]
  | Simple MachSimpleExpr
  deriving (Show, Eq)

data MachSimpleExpr
  = Var Ident
  | State Ident
  | Val S.Const
  | Call S.Op [MachSimpleExpr]
  deriving (Show, Eq)

{-
  Compile a normalized, scheduled expression

  Maybe I should still wrap this in an error monad to catch the 'impossible'
  cases (if it's ever run on unnormalized expressions)
-}


nodeToObc :: S.Node S.Clock -> ClassDef
nodeToObc S.MkNode{..} = let
  initStep = (map (fmap MkT) nodeInputs, map (fmap MkT) nodeOutputs, Skip)
  in evalFresh 0 $ foldM translateEquation (Class nodeName [] [] Skip initStep) nodeEquations


-- create the control structures for an expression on a clock
control :: S.Clock -> MachExpr -> MachExpr
control (S.On clk c x) exp = Case (Var x) [(toPat c, control clk exp), (toPat (not c), Skip)]
  where
  toPat True  = MkI "true"
  toPat False = MkI "false"
control  _ exp             = exp

-- control fusion: if we have two expressions on the same clock
joinE :: MachExpr -> MachExpr -> MachExpr
joinE (Case c es) (Case c' es') | c == c' = let
  -- ensure both case expressions are in the same order
  sortedL = sortOn fst es
  sortedR = sortOn fst es'
  -- recursively join all the case branches
  joined  = map (\((i, e), (_, e')) -> (i, joinE e e')) (zip sortedL sortedR)
  in Case c joined
joinE e1 (Seq e2 e3) = Seq (joinE e1 e2) e3
joinE e1 e2 = Seq e1 e2

-- | This function is partial because it only compiles normalized expressions
translateExpression :: MemoryEnv -> S.Expression -> MachSimpleExpr
translateExpression _ (S.Const c) = Val c
translateExpression m (S.Var x)   = case x `lookup` m of
  Just _  -> State x
  Nothing -> Var x
translateExpression m (S.When e _ _) = translateExpression m e
translateExpression m (S.BinOp op l r) =
  Call op [translateExpression m l, translateExpression m r]

translateControlExp :: MemoryEnv -> Ident -> S.Expression -> MachExpr
translateControlExp m y (S.Merge x l r) =
  Case (Var x) [(MkI "true", translateControlExp m y l)
               ,(MkI "false", translateControlExp m y r)
               ]
translateControlExp m y a = AssignLocal y (translateExpression m a)

translateEquation :: MonadFresh m => ClassDef -> S.Equation S.Clock -> m ClassDef
translateEquation Class{..} (S.MkEq _ (x :| []) (S.Arr c a)) = do
  let
    mem = (x, c) : classMemory
    e'  = translateExpression mem a
    (i, o, exp) = classStep
  pure $ Class
     { classMemory = mem
     , classReset = Seq (AssignState x (Val c)) classReset
     , classStep = (i, o, joinE (Simple e') exp)
     , ..
     }
translateEquation Class{..} (S.MkEq ck xs (S.App f args c)) = do
  instName <- MkI <$> prefixedName "machine"
  let
    c' = case lookup c classMemory of
           Just _  -> State c
           Nothing -> Var c
    args' = map (translateExpression classMemory) args
    (i, o, exp) = classStep
    resetNode = control ck (Case c' [(MkI "true", Reset instName), (MkI "false", Skip)])

  pure $ Class
     { classReset = Seq (Reset instName) classReset
     , classInstances = (instName, f) : classInstances
     , classStep = (i, o, joinE resetNode (joinE (control ck (Step xs instName args')) exp))
     , ..
     }
translateEquation Class{..} (S.MkEq ck (x :| []) e) = do
  let (i, o, exp) = classStep
  pure $ Class
    { classStep = (i, o, joinE (control ck (translateControlExp classMemory x e)) exp)
    , ..
    }

