module Lust.Machine where

import Lust.Name
import Lust.Syntax

type ParamList = [(Ident, Type)]

data ClassDef = Class
  { className :: Ident
  , classInstances :: [Ident]
  , classMemory :: ParamList
  , classReset :: MachExpr
  , classStep :: (ParamList, ParamList, MachExpr)
  } deriving (Show, Eq)

data MachExpr
  = AssignLocal Ident MachSimpleExpr
  | AssignState Ident MachSimpleExpr
  | Skip
  | Seq [MachExpr]
  | Case MachSimpleExpr [(Ident, MachExpr)]
  | Step [Ident] Ident [MachSimpleExpr]
  deriving (Show, Eq)

data MachSimpleExpr
  = Var Ident
  | State Ident
  | Val Const
  | Call Ident [MachSimpleExpr]
  deriving (Show, Eq)

-- control :: ClockAnn -> MachExpr
-- control (C (On clk c x) exp) =
-- control (C _ exp) =
