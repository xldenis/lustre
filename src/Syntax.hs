{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
module Syntax where

import           Name
import           Pretty

import           Data.List

data Node e = MkNode
  { nodeName      :: Ident
  , nodeInputs    :: [(Ident, Type)]
  , nodeOutputs   :: [(Ident, Type)]
  , nodeVariables :: [(Ident, Type)]
  , nodeEquations :: [Equation e]
  } deriving (Show, Eq, Functor)

instance Pretty e => Pretty (Node e) where
  pretty MkNode{..} =
    pretty "node" <+> pretty nodeName <+> prettyArgList nodeInputs
    <+> pretty "returns" <+> prettyArgList nodeOutputs <> semi
    `above` pretty "var" <+> prettyVars
    `above` pretty "let"
    `above` indent 2 (vsep (map (\x -> pretty x <> semi) nodeEquations))
    `above` pretty "end"
    where
    prettyArgList list = tupled $ map (\(i, ty) -> pretty i <+> pretty ":" <+> pretty ty) list
    prettyVars = let
      groupedVars = groupBy (\a b -> snd a == snd b) nodeVariables
      in align $ vsep (map prettyVarGroup groupedVars)

    prettyVarGroup :: [(Ident, Type)] -> Doc a
    prettyVarGroup grps = let
      (ids, tys) = unzip grps
      in hsep (punctuate comma (map pretty ids)) <+> colon <+> pretty (head tys) <> semi

instance Pretty e => Pretty (Equation e) where
  pretty (MkEq ids e) =
    hsep (punctuate comma (map pretty ids)) <+> pretty "=" <+> pretty e

instance Pretty Type where
  pretty TInt   = pretty "int"
  pretty TBool  = pretty "bool"
  pretty TFloat = pretty "float"

data Equation e = MkEq [Ident] e
  deriving (Show, Eq, Functor)

type PreNode = Node Expression
type PreEquation = Equation Expression

data Type
  = TInt
  | TBool
  | TFloat
  deriving (Show, Eq)

data Clock
  = Base
  | On Clock Bool Ident
  | CMeta Int
  deriving (Show, Eq)

data Expression
  = Const Const
  | Arr Const Expression
  | BinOp Op Expression Expression
  | Not Expression
  | Var Ident
  | Merge Ident Expression Expression
  | When Expression Bool Ident
  | App Ident [Expression]
  deriving (Show, Eq)

instance Pretty Expression where
  pretty (Const c) = pretty c
  pretty (Arr c e) = pretty c <+> pretty "->" <+> pretty e
  pretty (BinOp o l r) = parenthesize l <+> pretty o <+> parenthesize r
    where
    parenthesize e@(BinOp o' _ _)
      | opPrec o > opPrec o' = parens (pretty e)
      | otherwise = pretty e
    parenthesize e@(Const c) = pretty e
    parenthesize e@(Var v) = pretty e
    parenthesize e = parens (pretty e)

  pretty (Not e) = pretty "!" <+> pretty e
  pretty (Var i) = pretty i
  pretty (Merge i l r) =
    pretty "merge" <+> pretty i
    <+> parens (pretty "true"  <+> pretty "->" <+> pretty l)
    <+> parens (pretty "false" <+> pretty "->" <+> pretty r)
  pretty (When e c x) = pretty e <+> pretty "when" <+> pBool c <+> pretty x
    where pBool True  = pretty "true"
          pBool False = pretty "false"

instance Pretty Ident where
  pretty (MkI i) = pretty i

instance Pretty Const where
  pretty (Int i)      = pretty i
  pretty (Bool True)  = pretty "true"
  pretty (Bool False) = pretty "false"
  pretty (Float f)    = pretty f

data Op
  = Eq  | Neq | Lt  | Le  | Gt | Ge
  | Add | Sub | Mul | Div | Mod
  | And | Or
  deriving (Eq, Show)

opPrec Mul = 3
opPrec Div = 3
opPrec Mod = 3
opPrec Add = 2
opPrec Sub = 2
opPrec Le  = 1
opPrec Lt  = 1
opPrec Gt  = 1
opPrec Ge  = 1
opPrec Eq  = 1
opPrec Neq = 1

instance Pretty Op where
  pretty Eq  = pretty "=="
  pretty Neq = pretty "!="
  pretty Lt  = pretty "<"
  pretty Le  = pretty "<="
  pretty Gt  = pretty ">"
  pretty Ge  = pretty ">="
  pretty Add = pretty "+"
  pretty Sub = pretty "-"
  pretty Mul = pretty "*"
  pretty Div = pretty "/"
  pretty Mod = pretty "%"
  pretty And = pretty "&&"
  pretty Or  = pretty "||"

data Const
  = Int Int
  | Bool Bool
  | Float Float
  deriving (Show, Eq)
