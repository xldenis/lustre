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
generateC ms = map (generateN outMap) ms
  where outMap = map (\m -> let (_, o, _) = machStep m in (machName m, o)) ms

generateN :: OutList -> MachDef -> Doc ann
generateN p m = vcat [machineStruct m, machineReset p m, machineStep p m]

braces' = braces . enclose softline softline

machineStruct :: MachDef -> Doc ann
machineStruct Machine {..} = machStruct `above` outputStruct
 where
  memoryType (id, c) = cTy (constTy c) <+> pretty id
  instanceType (id, ty) = pointer (pretty ty) <+> pretty id

  machStruct =
    pretty "typedef"
      <+> pretty "struct"
      <+> (  hang 2
          .  braces'
          .  vcat
          .  punctuate semi
          $  map memoryType   machMemory
          ++ map instanceType machInstances
          )
      <+> pretty machName
      <>  semi

  outputStruct =
    let (_, o, _) = machStep
    in
      case o of
        []  -> mempty
        [_] -> mempty
        xs ->
          pretty "typedef"
            <+> pretty "struct"
            <+> (hang 2 . braces' . vcat . punctuate semi $ map
                  (\(id, ty) -> cTy ty <+> pretty id)
                  xs
                )
            <+> pretty (unId machName <> "_out")
            <>  semi


cTy TBool      = pretty "bool"
cTy TInt       = pretty "int"
cTy TFloat     = pretty "float"
cTy (TTuple _) = error "omg"

machOutputStruct Machine {..} =
  pointer
    $ let (_, o, _) = machStep
      in  case o of
            []       -> pretty "void"
            [(_, x)] -> cTy x
            _        -> pretty (unId machName <> "_out")

stateI i = pretty "self" <> pretty "->" <> pretty i

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
  let (inps, _, exp) = machStep
  in  pretty "void" <+> pretty (unId machName <> "_step") <+> tupled (machInps inps) <+> braces'
        (nest 2 $ hardline <> cExpr ol machInstances exp)
 where
  machInps i =
    (machPointerTy m <+> pretty "self")
      : (machOutputStruct m <+> pretty "out")
      : map (\(id, ty) -> cTy ty <+> pretty id) i

cName (LocalN i) = pretty i
cName (StateN i) = stateI i
cName (OutN   i) = pretty "out" <> pretty "->" <> pretty i

assignment n e = cName n <+> equals <+> e

-- | Transform an expression into C code
cExpr :: OutList -> [(Ident, Ident)] -> MachExpr -> Doc a
cExpr _  _     Skip = mempty
cExpr ol insts e    = go ol insts e <> semi
 where
  go ol insts (Seq s1 s2)  = go ol insts s1 <> semi `above` go ol insts s2
  go _  _     Skip         = mempty
  go _  _     (Assign i s) = assignment i (cSimpExpr s)
  go _  _     (Reset i   ) = pretty "reset" <> parens (stateI i)
  go ol insts (Case x brs) = pretty "switch" <+> cSimpExpr x <+> braces' (vcat $ map printBrs brs)
   where
    printBrs (_, Skip) = mempty
    printBrs (i, br  ) = pretty "case" <+> pretty i <> colon <+> braces' (cExpr ol insts br)
  go ol insts (Step res o f args) =
    vcat
      . punctuate semi
      $ assignment (LocalN (o <> MkI "$out")) (pretty (unId ty <> "_step") <> tupled stepArgs)
      : assignRes res
   where
    Just pl  = f `lookup` ol
    stepArgs = cName (StateN o) : map cSimpExpr args

    assignRes (i :| []) = [assignment (LocalN i) (pretty (unId o <> "$out"))]
    assignRes xs = map (\(i, (a, _)) -> assignment (LocalN i) (outAccessor a)) (zip (toList xs) pl)

    outAccessor field = pretty (unId o <> "$out") <> pretty "->" <> pretty field
    Just ty = lookup o insts
  go _ _ (Simple s) = cSimpExpr s

cSimpExpr :: MachSimpleExpr -> Doc ann
cSimpExpr (M.Var x) = cName x
cSimpExpr (Val   c) = pretty c
cSimpExpr e         = pretty e


