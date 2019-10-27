{-# LANGUAGE RecordWildCards #-}
module Lust.Machine where

import Lust.Name
import qualified Lust.Syntax as S

import Control.Monad.Fresh
type ParamList = [(Ident, MType)]
type MemoryEnv = [(Ident, S.Const)]
newtype MType = MkT S.Type
  deriving (Show, Eq)

data ClassDef = Class
  { className :: Ident
  , classInstances :: [(Ident, Ident)] -- References to any nodes called internally!
  , classMemory :: MemoryEnv
  , classReset :: MachExpr
  , classStep :: (ParamList, ParamList, MachExpr)
  } deriving (Show, Eq)

data MachExpr
  = AssignLocal Ident MachSimpleExpr
  | AssignState Ident MachSimpleExpr
  | Skip
  | Seq MachExpr MachExpr
  | Case MachSimpleExpr [(Ident, MachExpr)]
  | Reset Ident
  | Step [Ident] Ident [MachSimpleExpr]
  | Simple MachSimpleExpr
  deriving (Show, Eq)

data MachSimpleExpr
  = Var Ident
  | State Ident
  | Val S.Const
  | Call Ident [MachSimpleExpr]
  deriving (Show, Eq)

control :: S.Clock -> MachExpr -> MachExpr
control (S.On clk c x) exp = undefined
control  _ exp = undefined

{-
  Compile a normalized, scheduled expression
-}

joinE :: MachExpr -> MachExpr -> MachExpr
joinE (Case c es) (Case c' es')
  | c == c' && map fst es == map fst es'
  = Case c (map (\((i, e), (_, e')) -> (i, joinE e e')) (zip es es'))
joinE e1 (Seq e2 e3) = Seq (joinE e1 e2) e3
joinE e1 e2 = Seq e1 e2

translateExpression :: MemoryEnv -> S.Expression -> MachSimpleExpr
translateExpression _ (S.Const c) = Val c
translateExpression m (S.Var x)   = case x `lookup` m of
  Just _  -> State x
  Nothing -> Var x
translateExpression m (S.When e _ _) = translateExpression m e
translateExpression m (S.BinOp op l r) =
  Call (opToIdent op) [translateExpression m l, translateExpression m r]
  where
  opToIdent _ = undefined

translateControlExp :: MemoryEnv -> Ident -> S.Expression -> MachExpr
translateControlExp m y (S.Merge x l r) =
  Case (Var x) [(MkI "true", translateControlExp m y l)
               ,(MkI "false", translateControlExp m y r)
               ]
translateControlExp m y a = AssignLocal y (translateExpression m a)
translateControlExp _ _ _ = error "translateControlExp: unnormalized expression"

translateEquation :: ClassDef -> S.Equation S.Clock -> ClassDef
translateEquation Class{..} (S.MkEq _ [x] (S.Arr c a)) = let
mem = (x, c) : classMemory
e'  = translateExpression mem a
(i, o, exp) = classStep
in Class
   { classMemory = mem
   , classReset = Seq (AssignState x (Val c)) classReset
   , classStep = (i, o, joinE (Simple e') exp)
   , ..
   }
translateEquation Class{..} (S.MkEq ck xs (S.App f args c)) = let
c' = undefined
args' = map (translateExpression classMemory) args
instName = undefined
(i, o, exp) = classStep
resetNode = control ck (Case c' [(MkI "true", Reset instName), (MkI "false", Skip)])
in Class           -- v-- call reset on f
   { classReset = Seq (Reset instName) classReset
   , classInstances = (instName, f) : classInstances
   , classStep = (i, o, joinE resetNode (joinE (control ck (Step xs instName args')) exp))
   , ..
   }
translateEquation Class{..} (S.MkEq ck [x] e) = let
(i, o, exp) = classStep
in Class
{ classStep = (i, o, joinE (control ck (translateControlExp classMemory x e)) exp)
, ..
}

