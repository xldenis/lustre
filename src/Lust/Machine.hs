{-# LANGUAGE RecordWildCards #-}

module Lust.Machine where


import           Data.Maybe                ( fromMaybe )
import           Control.Monad
import           Control.Monad.Fresh
import           Data.List                 ( sortOn )
import           Data.List.NonEmpty        ( NonEmpty(..)
                                           , toList
                                           )
import qualified Data.Set                 as S
import           Data.Set                  ( Set )
import           Lust.Name
import           Lust.Pretty
import qualified Lust.Syntax              as S

type ParamList = [(Ident, S.Type)]

type MemoryEnv = [(Ident, S.Const)]

data MachDef
  = Machine
  { machName :: Ident
  , machInstances :: [(Ident, Ident)] -- References to any nodes called internally!
  , machMemory :: MemoryEnv
  , machReset :: MachExpr
  , machStep :: (ParamList, ParamList, MachExpr)
  } deriving (Show, Eq)

data MachLExp
  = LocalN Ident
  | StateN Ident
  | OutN Ident
  deriving (Show, Eq)

data MachExpr
  = Assign MachLExp MachSimpleExpr
  | Skip
  | Seq MachExpr MachExpr
  | Case MachSimpleExpr [(Ident, MachExpr)]
  | Reset Ident
  | Step (NonEmpty Ident) Ident Ident [MachSimpleExpr]
  | Simple MachSimpleExpr
  deriving (Show, Eq)

seq' e Skip = e
seq' e f    = Seq e f

data MachSimpleExpr
  = Var MachLExp
  | Val S.Const
  | Call S.Op [MachSimpleExpr]
  deriving (Show, Eq)

instance Pretty MachDef where
  pretty Machine {..} =
    let (inps, outs, stepExp) = machStep
    in
      hang 2 $ pretty "machine" <+> pretty machName <+> pretty "where" `above` vsep
        [ pretty "memory" <> colon <+> hsep
          (punctuate comma (map (\(i, c) -> pretty i <> colon <+> pretty (S.constTy c)) machMemory))
        , pretty "instances" <> colon <+> hsep
          (punctuate comma (map (\(i, t) -> pretty i <> colon <+> pretty t) machInstances))
        , group . hang 2 $ pretty "reset" <+> pretty "()" <+> equals `above` pretty machReset
        , group
        $       group (pretty "step" <+> S.prettyArgList inps)
        `above` (pretty "returns" <+> S.prettyArgList outs)

        <+>     (equals `above` indent 2 (pretty stepExp))
        ]

instance Pretty MachLExp where
  pretty (LocalN i) = pretty i
  pretty (StateN i) = pretty "state" <> parens (pretty i)
  pretty (OutN   i) = pretty i

instance Pretty MachExpr where
  pretty (Assign i e)  = pretty i <+> pretty ":=" <+> pretty e
  pretty Skip          = pretty "skip"
  pretty (Seq  e1 e2 ) = group $ vsep [pretty e1 <> semi, pretty e2]
  pretty (Case x  brs) = pretty "case" <+> pretty x <+> pretty "of" `above` indent
    2
    (vsep (map prettyBrs brs))
    where prettyBrs (i, e) = parens (pretty i <+> pretty "->" <+> pretty e)
  pretty (Reset i) = pretty i <> dot <> pretty "reset"
  pretty (Step res i _ args) =
    tupled (toList (fmap pretty res)) <+> pretty ":=" <+> pretty i <> dot <> pretty "step" <> tupled
      (map pretty args)
  pretty (Simple e) = pretty e

instance Pretty MachSimpleExpr where
  pretty (Var i        ) = pretty i
  pretty (Val c        ) = pretty c
  pretty (Call o [l, r]) = parenthesize l <+> pretty o <+> parenthesize r
   where
    parenthesize e@(Call o' args) | S.opPrec o > S.opPrec o' = parens (pretty e)
                                  | otherwise                = pretty e
    parenthesize e = pretty e
  pretty (Call o args) = pretty o <> tupled (map pretty args)

{-
  Compile a normalized, scheduled expression

  Maybe I should still wrap this in an error monad to catch the 'impossible'
  cases (if it's ever run on unnormalized expressions)
-}

nodeToObc :: S.Clocked S.Node -> MachDef
nodeToObc S.MkNode {..} =
  let initStep = (nodeInputs, nodeOutputs, Skip)
  in  evalFresh 0 $ foldM translateEquation (Machine nodeName [] [] Skip initStep) nodeEquations

-- create the control structures for an expression on a clock
control :: S.Clock -> MachExpr -> MachExpr
control (S.On clk c x) exp = Case (Var (LocalN x))
                                  [(toPat c, control clk exp), (toPat (not c), Skip)]
 where
  toPat True  = MkI "true"
  toPat False = MkI "false"
control _ exp = exp

-- control fusion: if we have two expressions on the same clock
joinE :: MachExpr -> MachExpr -> MachExpr
joinE (Case c es) (Case c' es') | c == c' =
  let -- ensure both case expressions are in the same order
      sortedL = sortOn fst es
      sortedR = sortOn fst es'
      -- recursively join all the case branches
      joined  = map (\((i, e), (_, e')) -> (i, joinE e e')) (zip sortedL sortedR)
  in  Case c joined
joinE e1 (Seq e2 e3) = seq' (joinE e1 e2) e3
joinE e1 e2          = seq' e1 e2


type NameMap = [(Ident, MachLExp)]

nameMap :: ParamList -> MemoryEnv -> NameMap
nameMap outs mem =
  map (\x -> (fst x, OutN $ fst x)) outs <> map (\x -> (fst x, StateN $ fst x)) mem

-- | This function is partial because it only compiles normalized expressions
translateExpression :: NameMap -> S.Expression -> MachSimpleExpr
translateExpression _ (S.Const c     ) = Val c
translateExpression m (S.Var   x     ) = Var $ fromMaybe (LocalN x) (lookup x m)
translateExpression m (S.When  e  _ _) = translateExpression m e
translateExpression m (S.BinOp op l r) = Call op [translateExpression m l, translateExpression m r]

translateControlExp :: NameMap -> Ident -> S.Expression -> MachExpr
translateControlExp m y (S.Merge x l r) =
  let x' = fromMaybe (LocalN x) (lookup x m)
  in  Case (Var x')
           [(MkI "true", translateControlExp m y l), (MkI "false", translateControlExp m y r)]
translateControlExp m y a = Assign (LocalN y) (translateExpression m a)

translateEquation :: MonadFresh m => MachDef -> S.Clocked S.Equation -> m MachDef
translateEquation Machine {..} (S.MkEq _ (x :| []) (S.Arr c a)) = do
  let mem         = (x, c) : machMemory
      e'          = translateExpression (nameMap o mem) a
      (i, o, exp) = machStep
  pure $ Machine
    { machMemory = mem
    , machReset  = seq' (Assign (StateN x) (Val c)) machReset
    , machStep   = (i, o, joinE (Simple e') exp)
    , ..
    }
translateEquation Machine {..} (S.MkEq (_, ck) xs (S.App f args c)) = do
  instName <- MkI <$> prefixedName "machine"
  let c'          = fromMaybe (LocalN c) (lookup c names)
      names       = nameMap o machMemory
      args'       = map (translateExpression (nameMap o machMemory)) args
      (i, o, exp) = machStep
      resetNode   = control ck (Case (Var c') [(MkI "true", Reset instName), (MkI "false", Skip)])
  pure $ Machine
    { machReset     = seq' (Reset instName) machReset
    , machInstances = (instName, f) : machInstances
    , machStep      = (i, o, joinE resetNode (joinE (control ck (Step xs instName f args')) exp))
    , ..
    }
translateEquation Machine {..} (S.MkEq (_, ck) (x :| []) e) = do
  let (i, o, exp) = machStep
  pure $ Machine
    { machStep = (i, o, joinE (control ck (translateControlExp (nameMap o machMemory) x e)) exp)
    , ..
    }
