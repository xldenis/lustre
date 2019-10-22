{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
module Typing where

import           Name
import           Syntax

import           Control.Monad.Except
import           Control.Monad.State

{-

  Annotate binary expressions with their concrete type since they can
  be polymorphic.

-}

data VarRole = Write | Read
  deriving (Show, Eq)

newtype Environment = Gamma { unEnv :: [(Ident, (VarRole, Type))] }
  deriving (Show, Eq, Monoid, Semigroup)

data TypecheckError
  = TypeMismatch Type Type -- ^ expected type t but got type u
  | IncorrectArity [Type] [Type] -- ^ Expected a stream of type (t1, .., tn) but got a stream of type (s1, ..., sm)
  | UndefinedIdent Ident
  | MismatchedStream [Type] [Type]
  | BadBinOpStream BinOpTy [Type]
  | WriteToInput Ident
  deriving (Show, Eq)

type TypecheckM m = (MonadState Environment m, MonadError TypecheckError m)

runTyping :: [PreNode] -> Either TypecheckError [PreNode]
runTyping n = flip evalState (Gamma []) . runExceptT $ typecheckNodes n

withNames :: TypecheckM m => [(Ident, (VarRole, Type))] -> m a -> m a
withNames nms action = do
  env <- get
  put (Gamma nms <> env)
  res <- action
  put env
  return res

lookupName :: TypecheckM m => Ident -> m Type
lookupName i =
  gets (lookup i . unEnv) >>= \case
  Just (_, t) -> pure t
  Nothing -> throwError $ UndefinedIdent i

lookupWriteName :: TypecheckM m => Ident -> m Type
lookupWriteName i =
  gets (lookup i . unEnv) >>= \case
  Just (Write, t) -> pure t
  Just (_, _) -> throwError $ WriteToInput i
  Nothing -> throwError $ UndefinedIdent i

addNode :: TypecheckM m => PreNode -> m ()
addNode node =
  pure ()

typecheckNodes :: TypecheckM m => [PreNode] -> m [PreNode]
typecheckNodes = mapM (\n -> typecheckNode n <* addNode n)

typecheckNode :: TypecheckM m => PreNode -> m PreNode
typecheckNode n@MkNode{..} = do
  eqns' <- withNames (map (fmap (Read,)) nodeInputs) $
    withNames (map (fmap (Write,)) (nodeOutputs <> nodeVariables)) $
     mapM typecheckEqn nodeEquations

  return n { nodeEquations = eqns' }

typecheckEqn :: TypecheckM m => PreEquation -> m PreEquation
typecheckEqn (MkEq ids expr) = do
  idTys <- mapM lookupWriteName ids
  (e', expTys) <- typecheckExpr expr
  if idTys == expTys
  then pure (MkEq ids e')
  else throwError (MismatchedStream idTys expTys)


{-| Produce the types returned by an expression. The only way to get more than one type out
    is when an application returns more than one output
-}

checkType :: TypecheckM m => Type -> [Type] -> m ()
checkType expected given = case given of
  [t] |  t == expected -> pure ()
      | otherwise -> throwError (TypeMismatch expected t)
  tys  -> throwError (IncorrectArity [expected] tys)

typecheckExpr :: TypecheckM m => Expression -> m (Expression, [Type])
typecheckExpr (Const c) = pure (Const c, [constTy c])
typecheckExpr (Arr c expr) = do
  (e', ety) <- typecheckExpr expr
  checkType (constTy c) ety

  pure (Arr c e', ety)
typecheckExpr (Not e) = do
  (e', ety) <- typecheckExpr e
  checkType TBool ety
  pure (Not e', ety)
typecheckExpr (Var i) = (Var i,) . pure <$> lookupName i
typecheckExpr (BinOp op l r) = do
  (l', lty) <- typecheckExpr l
  (r', rty) <- typecheckExpr r
  case (binOpTy op, lty, rty) of
    (Pred,  [t1], t2) -> checkType t1 t2 >> pure (BinOp op l' r', [TBool])
    (Arith, [t1], t2) -> checkType t1 t2 >> pure (BinOp op l' r', lty)
    (Logical, t1, t2) ->
      checkType TBool t1 >>
      checkType TBool t2 >>
      pure (BinOp op l' r', [TBool])
    (opty, t1, _) -> throwError (BadBinOpStream opty t1)
typecheckExpr (Merge c l r) = do
  lookupName c >>= checkType TBool . pure
  (l', ltys) <- typecheckExpr l
  (r', rtys) <- typecheckExpr r

  if ltys == rtys
  then pure (Merge c l' r', ltys)
  else throwError (MismatchedStream ltys rtys)
typecheckExpr (When e c x) = do
  lookupName x >>= checkType TBool . pure

  (e', ety) <- typecheckExpr e

  pure (When e' c x, ety)

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

constTy :: Const -> Type
constTy (Bool _)  = TBool
constTy (Int _)   = TInt
constTy (Float _) = TFloat
