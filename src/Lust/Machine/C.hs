{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
module Lust.Machine.C where

import           Lust.Machine             as M
import           Lust.Pretty

import           Lust.Name
import           Lust.Syntax

import           Data.List.NonEmpty        ( toList
                                           , NonEmpty(..)
                                           )

type OutList = [(Ident, ParamList)]

generateC :: [MachDef] -> [Doc ann]
generateC ms = map (generateN outMap) ms where outMap = map (\m -> (machName m, machOuts m)) ms

generateN :: OutList -> MachDef -> Doc ann
generateN p m = vcat [machineStruct m, machineReset p m, machineStep p m]

braces' = braces . enclose softline softline

machineStruct :: MachDef -> Doc ann
machineStruct Machine {..} = machStruct `above` outputStruct
 where
  memoryType (id, c) = cTy (constTy c) <+> pretty id
  instanceType (id, ty) = pointer (pretty ty) <+> pretty id

  machStruct =
    (pretty "typedef" <+> pretty "struct")
      <+> (  hang 2
          .  braces'
          .  vcat
          .  punctuate semi
          $  map memoryType   machMemory
          ++ map instanceType machInstances
          )
      <+> pretty machName
      <>  semi

  outputStruct = case machOuts of
    []  -> pretty "typedef" <+> pretty "void" <+> machOutputStruct machName <> semi
    [x] -> pretty "typedef" <+> cTy (snd x) <+> machOutputStruct machName <> semi
    xs ->
      (pretty "typedef" <+> pretty "struct")
        <+> (hang 2 . braces' . vcat . punctuate semi $ map (\(id, ty) -> cTy ty <+> pretty id) xs)
        <+> pretty (unId machName <> "_out")
        <>  semi

cTy :: Type -> Doc a
cTy TBool      = pretty "bool"
cTy TInt       = pretty "int"
cTy TFloat     = pretty "float"
cTy (TTuple _) = error "omg"

machOutputStruct :: Ident -> Doc a
machOutputStruct i = pretty (unId i <> "_out")

stateI :: Ident -> Doc a
stateI i = pretty "self" <> pretty "->" <> pretty i

pointer :: Doc a -> Doc a
pointer d = d <> pretty "*"

-- | Get the type of the machine state struct
machPointerTy :: MachDef -> Doc a
machPointerTy m = pointer (pretty $ machName m)

-- | Generate the code corresponding to the reset method for a machine
machineReset :: OutList -> MachDef -> Doc a
machineReset ol m@Machine {..} =
  pretty "void"
    <+> pretty (unId machName <> "_reset")
    <+> parens (machPointerTy m <+> pretty "self")
    <+> braces' (cExpr ol machInstances machReset)

-- | Generate the code corresponding to the step method of a machine
machineStep :: OutList -> MachDef -> Doc a
machineStep ol m@Machine {..} =
  pretty "void" <+> pretty (unId machName <> "_step") <+> tupled (stepArgs machInps) <+> braces'
    (nest 2 $ hardline <> vsep (localDecls <> instDecls <> [cExpr ol machInstances machStep]))
 where
  localDecls = map (\(i, ty) -> cTy ty <+> pretty i <> semi) machLocals
  instDecls =
    map (\(i, ty) -> machOutputStruct ty <+> pretty (i <> MkI "$out") <> semi) machInstances

  stepArgs i =
    (machPointerTy m <+> pretty "self")
      : (pointer (machOutputStruct machName) <+> pretty "out")
      : map (\(id, ty) -> cTy ty <+> pretty id) i

cName :: MachLExp -> Doc a
cName (LocalN i) = pretty i
cName (StateN i) = stateI i
cName (OutN   i) = pretty "out" <> pretty "->" <> pretty i

assignment :: MachLExp -> Doc a -> Doc a
assignment n e = cName n <+> equals <+> e

-- | Transform an expression into C code
cExpr :: OutList -> [(Ident, Ident)] -> MachExpr MachLExp -> Doc a
cExpr _  _     Skip = mempty
cExpr ol insts e    = go ol insts e <> semi
 where
  go ol insts (Seq s1 s2)  = go ol insts s1 <> semi `above` go ol insts s2
  go _  _     Skip         = mempty
  go _  _     (Assign i s) = assignment i (cSimpExpr s)
  go _  insts (Reset i   ) = pretty (unId ty <> "_reset") <> parens (stateI i)
    where Just ty = lookup i insts
  go ol insts (Case x brs) = pretty "switch" <+> parens (cSimpExpr x) <+> braces'
    (vcat $ map printBrs brs)
   where
    printBrs (_, Skip) = mempty
    printBrs (i, br  ) = pretty "case" <+> pretty i <> colon <+> braces' (cExpr ol insts br)
  go ol insts (Step res o f args) =
    vcat . punctuate semi $ (pretty (unId ty <> "_step") <> tupled stepArgs) : assignRes res
   where
    Just pl = f `lookup` ol
    stepArgs =
      cName (StateN o) : pretty "&" <+> cName (LocalN (o <> MkI "$out")) : map cSimpExpr args

    assignRes (i :| []) = [assignment (LocalN i) (pretty (unId o <> "$out"))]
    assignRes xs = map (\(i, (a, _)) -> assignment (LocalN i) (outAccessor a)) (zip (toList xs) pl)

    outAccessor field = pretty (unId o <> "$out") <> dot <> pretty field
    Just ty = lookup o insts
  go _ _ (Simple s) = cSimpExpr s

cSimpExpr :: MachSimpleExpr MachLExp -> Doc ann
cSimpExpr (M.Var x) = cName x
cSimpExpr (Val   c) = pretty c
cSimpExpr e         = pretty e


