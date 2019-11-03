{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
module Lust.Typing.Types where

import           Lust.Error
import           Lust.Name
import           Lust.Pretty
import           Lust.Syntax

import           Control.Monad.State
import           Data.Bifunctor            ( first )
import           Data.List.NonEmpty        ( NonEmpty(..)
                                           , nonEmpty
                                           , toList
                                           )

{-

  Annotate binary expressions with their concrete type since they can
  be polymorphic.

-}

data VarRole = Write | Read
  deriving (Show, Eq)

data Environment = MkE
  { gamma :: [(Ident, (VarRole, Type))]
  , delta :: [(Ident, (Maybe Type, Type))]
  } deriving (Show, Eq)

instance Semigroup Environment where
  (<>) a b = MkE (gamma a <> gamma b) (delta a <> delta b)

instance Monoid Environment where
  mempty = MkE mempty mempty

data TypecheckError
  = TypeMismatch Type Type -- ^ expected type t but got type u
  | IncorrectArity [Type] [Type] -- ^ Expected a stream of type (t1, .., tn) but got a stream of type (s1, ..., sm)
  | UndefinedIdent Ident
  | MismatchedStream (NonEmpty Type) (NonEmpty Type)
  | BadBinOpStream BinOpTy [Type]
  | WriteToInput Ident
  | EmptyReturnType
  deriving (Show, Eq)

fromTypeError :: TypecheckError -> Error ann
fromTypeError err = Error { errHeader  = pretty "Typing Error"
                          , errSummary = render err
                          , errKind    = "typing"
                          , errHints   = []
                          }
 where
  render (TypeMismatch e g) = pretty "Expected type" <+> pretty e <+> pretty "but got" <+> pretty g
  render (IncorrectArity es gs) =
    pretty "Expected a stream of type"
      <+> tupled (map pretty es)
      <+> pretty "len"
      <+> pretty (length es)
      <+> pretty "but got"
      <+> tupled (map pretty gs)
      <+> pretty "len"
      <+> pretty (length es)
  render (UndefinedIdent i) = pretty "Undefined variable" <+> pretty i
  render (MismatchedStream es gs) =
    pretty "Expected a stream of type"
      <+> tupled (toList $ fmap pretty es)
      <+> pretty "len"
      <+> pretty (length es)
      <+> pretty "but got"
      <+> tupled (toList $ fmap pretty gs)
      <+> pretty "len"
      <+> pretty (length es)
  render (BadBinOpStream _ tys) =
    pretty "Binary operations can't be applied to streams of type" <+> tupled (map pretty tys)

type TypecheckM m = (MonadState Environment m, MonadError TypecheckError m)

runTyping :: [PreNode] -> Either (Error a) [Typed Node]
runTyping n = first fromTypeError . flip evalState mempty . runExceptT $ typecheckNodes n

withNames :: TypecheckM m => [(Ident, (VarRole, Type))] -> m a -> m a
withNames nms action = do
  env <- get
  put $ MkE nms mempty <> env
  res <- action
  put env
  return res

lookupName :: TypecheckM m => Ident -> m Type
lookupName i = gets (lookup i . gamma) >>= \case
  Just (_, t) -> pure t
  Nothing     -> throwError $ UndefinedIdent i

lookupWriteName :: TypecheckM m => Ident -> m Type
lookupWriteName i = gets (lookup i . gamma) >>= \case
  Just (Write, t) -> pure t
  Just (_    , _) -> throwError $ WriteToInput i
  Nothing         -> throwError $ UndefinedIdent i

lookupFunction :: TypecheckM m => Ident -> m (Maybe Type, Type)
lookupFunction i = gets (lookup i . delta) >>= \case
  Just x  -> pure x
  Nothing -> throwError $ UndefinedIdent i

addNode :: TypecheckM m => PreNode -> m ()
addNode MkNode {..} = do
  outputs <- case nonEmpty (map snd nodeOutputs) of
    Just x  -> pure (TTuple x)
    Nothing -> throwError EmptyReturnType
  let inps = TTuple <$> nonEmpty (map snd nodeInputs)
  let pair = (nodeName, (inps, outputs))

  modify $ \s -> MkE mempty [pair] <> s
 where

-- | Type Checking Nodes, Equations and Expressions

typecheckNodes :: TypecheckM m => [PreNode] -> m [Typed Node]
typecheckNodes = mapM (\n -> typecheckNode n <* addNode n)

typecheckNode :: TypecheckM m => PreNode -> m (Typed Node)
typecheckNode n@MkNode {..} = do
  eqns' <-
    withNames (map (fmap (Read, )) nodeInputs)
    $ withNames (map (fmap (Write, )) (nodeOutputs <> nodeVariables))
    $ mapM typecheckEqn nodeEquations

  return n { nodeEquations = eqns' }

typecheckEqn :: TypecheckM m => PreEquation -> m (Typed Equation)
typecheckEqn (MkEq () ids expr) = do
  idTys        <- mapM lookupWriteName ids
  (e', expTys) <- typecheckExpr expr
  if idTys == fromTTuple expTys
    then pure (MkEq expTys ids e')
    else throwError (MismatchedStream idTys (fromTTuple expTys))
 where
  fromTTuple (TTuple xs) = xs
  fromTTuple ty          = ty :| []

checkType :: TypecheckM m => Type -> Type -> m ()
checkType expected given = case given of
  t | t == expected -> pure ()
    | otherwise     -> throwError (TypeMismatch expected t)

typecheckExpr :: TypecheckM m => Expression -> m (Expression, Type)
typecheckExpr (Const c   ) = pure (Const c, constTy c)
typecheckExpr (Arr c expr) = do
  (e', ety) <- typecheckExpr expr
  checkType (constTy c) ety

  pure (Arr c e', ety)
typecheckExpr (Not e) = do
  (e', ety) <- typecheckExpr e
  checkType TBool ety
  pure (Not e', ety)
typecheckExpr (Var i       ) = (Var i, ) <$> lookupName i
typecheckExpr (BinOp op l r) = do
  (l', lty) <- typecheckExpr l
  (r', rty) <- typecheckExpr r
  case (binOpTy op, lty, rty) of
    (opty   , TTuple t1, _ ) -> throwError (BadBinOpStream opty (toList t1))
    (Pred   , t1       , t2) -> checkType t1 t2 >> pure (BinOp op l' r', TBool)
    (Arith  , t1       , t2) -> checkType t1 t2 >> pure (BinOp op l' r', lty)
    (Logical, t1, t2) -> checkType TBool t1 >> checkType TBool t2 >> pure (BinOp op l' r', TBool)
typecheckExpr (Merge c l r) = do
  lookupName c >>= checkType TBool
  (l', ltys) <- typecheckExpr l
  (r', rtys) <- typecheckExpr r

  if ltys == rtys then pure (Merge c l' r', ltys) else throwError (TypeMismatch ltys rtys)
typecheckExpr (When e c x) = do
  lookupName x >>= checkType TBool

  (e', ety) <- typecheckExpr e

  pure (When e' c x, ety)
typecheckExpr (App f args a) = do
  (atys , rettys) <- lookupFunction f
  (args', argtys) <- unzip <$> mapM typecheckExpr args
  let argTuple = fmap TTuple (nonEmpty argtys)
  when (atys /= argTuple) (throwError (IncorrectArity (argsToList atys) argtys))

  pure (App f args' a, rettys)
 where
  argsToList Nothing           = []
  argsToList (Just (TTuple x)) = toList x
  argsToList (Just x         ) = [x]

-- | Pack a non-empty list of types into the appropriate Type
tupleTy :: NonEmpty Type -> Type
tupleTy (x :| []) = x
tupleTy xs        = TTuple xs

data BinOpTy
  = Pred
  | Arith
  | Logical
  deriving (Show, Eq)

binOpTy :: Op -> BinOpTy
binOpTy Eq  = Pred
binOpTy Neq = Pred
binOpTy Lt  = Pred
binOpTy Le  = Pred
binOpTy Gt  = Pred
binOpTy Ge  = Pred
binOpTy Add = Arith
binOpTy Sub = Arith
binOpTy Mul = Arith
binOpTy Div = Arith
binOpTy Mod = Arith
binOpTy And = Logical
binOpTy Or  = Logical
