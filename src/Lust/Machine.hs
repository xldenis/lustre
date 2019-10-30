{-# LANGUAGE RecordWildCards, DeriveTraversable, DeriveFoldable #-}

module Lust.Machine where


import           Data.Maybe                ( fromMaybe )
import           Control.Monad.Fresh
import           Data.List                 ( sortOn
                                           , (\\)
                                           )
import           Data.List.NonEmpty        ( NonEmpty(..)
                                           , toList
                                           )
import qualified Data.List.NonEmpty       as NE
import           Data.Foldable             ( foldrM )
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
  , machReset :: MachExpr MachLExp
  , machStep :: MachExpr MachLExp
  , machInps :: ParamList
  , machOuts :: ParamList
  , machLocals :: ParamList
  } deriving (Show, Eq)

data MachLExp
  = LocalN Ident
  | StateN Ident
  | OutN Ident
  deriving (Show, Eq, Ord)

data MachExpr v
  = Assign v (MachSimpleExpr v)
  | Skip
  | Seq (MachExpr v) (MachExpr v)
  | Case (MachSimpleExpr v) [(Ident, MachExpr v)]
  | Reset Ident
  | Step (NonEmpty Ident) Ident Ident [MachSimpleExpr v]
  | Simple (MachSimpleExpr v)
  deriving (Show, Eq, Functor, Traversable, Foldable)

seq' :: MachExpr v -> MachExpr v -> MachExpr v
seq' e Skip = e
seq' e f    = Seq e f

data MachSimpleExpr v
  = Var v
  | Val S.Const
  | Call S.Op [MachSimpleExpr v]
  deriving (Show, Eq, Functor, Traversable, Foldable)

instance Pretty MachDef where
  pretty Machine {..} =
    hang 2 $ pretty "machine" <+> pretty machName <+> pretty "where" `above` vsep
      [ pretty "memory" <> colon <+> hsep
        (punctuate comma (map (\(i, c) -> pretty i <> colon <+> pretty (S.constTy c)) machMemory))
      , pretty "instances" <> colon <+> hsep
        (punctuate comma (map (\(i, t) -> pretty i <> colon <+> pretty t) machInstances))
      , group . hang 2 $ pretty "reset" <+> pretty "()" <+> equals `above` pretty machReset
      , group
      $       group (pretty "step" <+> S.prettyArgList machInps)
      `above` (pretty "returns" <+> S.prettyArgList machOuts)

      <+>     (equals `above` indent 2 (pretty machStep))
      ]

instance Pretty MachLExp where
  pretty (LocalN i) = pretty i
  pretty (StateN i) = pretty "state" <> parens (pretty i)
  pretty (OutN   i) = pretty i

instance Pretty v => Pretty (MachExpr v) where
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

instance Pretty v => Pretty (MachSimpleExpr v) where
  pretty (Var i        ) = pretty i
  pretty (Val c        ) = pretty c
  pretty (Call o [l, r]) = parenthesize l <+> pretty o <+> parenthesize r
   where
    parenthesize e@(Call o' _) | S.opPrec o > S.opPrec o' = parens (pretty e)
                               | otherwise                = pretty e
    parenthesize e = pretty e
  pretty (Call o args) = pretty o <> tupled (map pretty args)

{-
  Compile a normalized, scheduled expression

  Maybe I should still wrap this in an error monad to catch the 'impossible'
  cases (if it's ever run on unnormalized expressions)
-}

nodeToObc :: S.Clocked S.Node -> MachDef
nodeToObc S.MkNode {..} = evalFresh 0 $ foldrM
  (flip translateEquation)
  (Machine nodeName [] [] Skip Skip nodeInputs nodeOutputs [])
  nodeEquations

-- | Create the control structures for an expression on a clock, joinE will then attempt to fuse these structures

control :: S.Clock -> MachExpr MachLExp -> MachExpr MachLExp
control (S.On clk c x) exp = Case (Var (LocalN x))
                                  [(toPat c, control clk exp), (toPat (not c), Skip)]
 where
  toPat True  = MkI "true"
  toPat False = MkI "false"
control _ exp = exp

-- control fusion: if we have two expressions on the same clock
joinE :: Eq v => MachExpr v -> MachExpr v -> MachExpr v
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

-- | Indicate where each variable is from, either output, state variable or local
-- | If a variable isn't present it means it's a local
nameMap :: ParamList -> MemoryEnv -> NameMap
nameMap outs mem =
  map (\x -> (fst x, OutN $ fst x)) outs <> map (\x -> (fst x, StateN $ fst x)) mem

lookupNameMap :: Ident -> NameMap -> MachLExp
lookupNameMap x nm = fromMaybe (LocalN x) (lookup x nm)

-- | This function is partial because it only compiles normalized expressions
translateExpression :: NameMap -> S.Expression -> MachSimpleExpr MachLExp
translateExpression _ (S.Const c     ) = Val c
translateExpression m (S.Var   x     ) = Var $ lookupNameMap x m
translateExpression m (S.When  e  _ _) = translateExpression m e
translateExpression m (S.BinOp op l r) = Call op [translateExpression m l, translateExpression m r]

translateControlExp :: NameMap -> Ident -> S.Expression -> MachExpr MachLExp
translateControlExp m y (S.Merge x l r) = Case
  (Var $ lookupNameMap x m)
  [(MkI "true", translateControlExp m y l), (MkI "false", translateControlExp m y r)]
translateControlExp m y a = Assign (lookupNameMap y m) (translateExpression m a)

translateEquation :: MonadFresh m => MachDef -> S.Clocked S.Equation -> m MachDef
translateEquation Machine {..} (S.MkEq (_, ck) (x :| []) (S.Arr c a)) = do
  let mem = (x, c) : machMemory
      e'  = translateExpression (nameMap machOuts mem) a
  pure $ Machine
    { machMemory = mem
    , machReset  = seq' (Assign (StateN x) (Val c)) machReset
    , machStep   = joinE (control ck (Assign (StateN x) e')) machStep
    , ..
    }
translateEquation Machine {..} (S.MkEq (ty, ck) xs (S.App f args c)) = do
  instName <- MkI <$> prefixedName "machine"
  let c'        = lookupNameMap c names
      names     = nameMap machOuts machMemory
      args'     = map (translateExpression names) args
      locals'   = toParamList ty xs \\ machOuts
      resetNode = control ck (Case (Var c') [(MkI "true", Reset instName), (MkI "false", Skip)])
  pure $ Machine
    { machReset     = seq' (Reset instName) machReset
    , machInstances = (instName, f) : machInstances
    , machStep      = joinE resetNode (joinE (control ck (Step xs instName f args')) machStep)
    , machLocals    = locals'
    , ..
    }
  where toParamList (S.TTuple ts) xs = toList $ NE.zip xs ts
translateEquation Machine {..} (S.MkEq (ty, ck) (x :| []) e) = pure $ Machine
  { machStep   = joinE (control ck (translateControlExp (nameMap machOuts machMemory) x e)) machStep
  , machLocals = (x, ty) : machLocals
  , ..
  }
