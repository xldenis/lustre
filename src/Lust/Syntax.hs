{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
module Lust.Syntax where

import           Lust.Name
import           Lust.Pretty

import           Data.List
import           Data.List.NonEmpty        ( NonEmpty(..)
                                           , toList
                                           )
import qualified Data.List.NonEmpty       as NL

data Node ann = MkNode
  { nodeName      :: Ident
  , nodeInputs    :: [(Ident, Type)]
  , nodeOutputs   :: [(Ident, Type)]
  , nodeVariables :: [(Ident, Type)]
  , nodeEquations :: [Equation ann]
  } deriving (Show, Eq, Functor)

prettyArgList list = tupled $ map (\(i, ty) -> pretty i <+> pretty ":" <+> pretty ty) list

instance Pretty e => Pretty (Node e) where
  pretty MkNode {..} =
    pretty "node"
      <+>     pretty nodeName
      <+>     prettyArgList nodeInputs
      <+>     pretty "returns"
      <+>     prettyArgList nodeOutputs
      <>      semi
      `above` pretty "var"
      <+>     prettyVars
      `above` pretty "let"
      `above` indent 2 (vsep (map (\x -> pretty x <> semi) nodeEquations))
      `above` pretty "end"
   where
    prettyVars =
      let groupedVars = groupBy (\a b -> snd a == snd b) nodeVariables
      in  if null nodeVariables then mempty else align $ vsep (map prettyVarGroup groupedVars)

    prettyVarGroup :: [(Ident, Type)] -> Doc a
    prettyVarGroup grps =
      let (ids, tys) = unzip grps
      in  hsep (punctuate comma (map pretty ids)) <+> colon <+> pretty (head tys) <> semi

instance Pretty (Equation ann) where
  pretty (MkEq _ ids e) =
    hsep (punctuate comma (toList $ fmap pretty ids)) <+> pretty "=" <+> pretty e

instance Pretty Type where
  pretty TInt   = pretty "int"
  pretty TBool  = pretty "bool"
  pretty TFloat = pretty "float"

data Equation ann = MkEq ann (NonEmpty Ident) Expression
  deriving (Show, Eq, Functor)

type PreNode = Pre Node
type PreEquation = Pre Equation

type Pre f = f ()
type Typed f = f Type
type Clocked f = f (Type, Clock)

data Type
  = TInt
  | TBool
  | TFloat
  | TTuple (NonEmpty Type)
  deriving (Show, Eq)

data Clock
  = Base
  | On Clock Bool Ident
  | CMeta Int
  | CTuple (NonEmpty Clock)
  deriving (Show, Eq)

instance Pretty Clock where
  pretty Base        = pretty "base"
  pretty (On ck b i) = pretty "on" <+> pretty ck <+> pBool b <+> pretty i
   where
    pBool True  = pretty "true"
    pBool False = pretty "false"

data Expression
  = Const Const
  | Arr Const Expression
  | BinOp Op Expression Expression
  | Not Expression
  | Var Ident
  | Merge Ident Expression Expression
  | When Expression Bool Ident
  | App Ident [Expression] Ident
  | Tuple (NonEmpty Expression)
  deriving (Show, Eq)

instance Pretty Expression where
  pretty (Const c    ) = pretty c
  pretty (Arr c e    ) = pretty c <+> pretty "->" <+> pretty e
  pretty (BinOp o l r) = parenthesize l <+> pretty o <+> parenthesize r
   where
    parenthesize e@(BinOp o' _ _) | opPrec o > opPrec o' = parens (pretty e)
                                  | otherwise            = pretty e
    parenthesize e@(Const _) = pretty e
    parenthesize e@(Var   _) = pretty e
    parenthesize e           = parens (pretty e)

  pretty (Not e) = pretty "!" <+> pretty e
  pretty (Var i) = pretty i
  pretty (Merge i l r) =
    pretty "merge" <+> pretty i <+> parens (pretty "true" <+> pretty "->" <+> pretty l) <+> parens
      (pretty "false" <+> pretty "->" <+> pretty r)
  pretty (When e c x) = pretty e <+> pretty "when" <+> pBool c <+> pretty x
   where
    pBool True  = pretty "true"
    pBool False = pretty "false"
  pretty (App f args a) = pretty f <> tupled (map pretty args) <+> pretty "every" <+> pretty a
  pretty (Tuple es    ) = tupled (toList $ fmap pretty es)
instance Pretty Ident where
  pretty (MkI i) = pretty i

instance Pretty Const where
  pretty (Int   i    ) = pretty i
  pretty (Bool  True ) = pretty "true"
  pretty (Bool  False) = pretty "false"
  pretty (Float f    ) = pretty f

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
opPrec Or  = 2
opPrec And = 2
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

constTy :: Const -> Type
constTy (Bool  _) = TBool
constTy (Int   _) = TInt
constTy (Float _) = TFloat
