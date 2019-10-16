module Syntax where

import Name

data Node = MkNode
  { nodeName      :: Ident
  , nodeInputs    :: [(Ident, Type)]
  , nodeOutputs   :: [(Ident, Type)]
  , nodeVariables :: [(Ident, Type)]
  , nodeEquations :: [Equation]
  } deriving (Show, Eq)

data Equation = MkEq [Ident] Expression
  deriving (Show, Eq)

data Type
  = TInt
  | TBool
  | TFloat
  deriving (Show, Eq)

data Expression
  = Const Const
  | Arr Const Expression
  | BinOp Op Expression Expression
  | Not Expression
  | Var Ident
  | Ifte Expression Expression Expression
  deriving (Show, Eq)

data Op
  = Eq  | Neq | Lt  | Le  | Gt | Ge
  | Add | Sub | Mul | Div | Mod
  | And | Or
  deriving (Eq, Show)

data Const
  = Int Int
  | Bool Bool
  | Float Float
  deriving (Show, Eq)
