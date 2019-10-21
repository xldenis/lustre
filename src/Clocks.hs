{-# LANGUAGE ConstraintKinds, FlexibleContexts,
  GeneralizedNewtypeDeriving, LambdaCase, RecordWildCards,
  DeriveFunctor, FlexibleInstances, MultiParamTypeClasses #-}
module Clocks where

import Control.Monad.Except
import Control.Monad.State
import           Control.Monad.Unify
import           Syntax hiding (Expression(..))
import qualified Syntax as S (Expression(..))
import Name

import Data.Bifunctor
import qualified Data.HashMap.Strict as M
import Data.Maybe

newtype ClockEnv = H { unClockEnv :: [(Ident, Clock)] }
  deriving (Show, Eq, Semigroup, Monoid)

instance Partial Clock where
  unknown = CMeta
  isUnknown (CMeta i) = Just i
  isUnknown _ = Nothing

  unknowns (CMeta i) = [i]
  unknowns (On c _ _) = unknowns c
  unknowns _ = []

  ($?) sub m@(CMeta i) = fromMaybe m $ M.lookup i (runSubstitution sub)
  ($?) sub (On clk c x) = On (sub $? clk) c x
  ($?) _ Base = Base

instance UnificationError Clock ClockingError where
  occursCheckFailed = ClockOccursCheck

instance MonadError ClockingError m => MonadUnify ClockingError Clock (UnifyT Clock m) where
  (=?=) (CMeta i) (CMeta v) | i == v = return ()
  (=?=) (CMeta i) u = i =:= u
  (=?=) i (CMeta u) = u =:= i
  (=?=) Base Base = return ()
  (=?=) a@(On c s x) b@(On d t y) =
    if s == t && x == y
    then c =?= d
    else throwError $ MismatchClocks a b
  (=?=) u t = throwError $ MismatchClocks u t

  getSubst = UnifyT $ gets unifyCurrentSubstitution
  setSubst sub = UnifyT $ modify $ \s -> s { unifyCurrentSubstitution = sub }

type ClockingM m = (MonadState ClockEnv m, MonadError ClockingError m, MonadUnify ClockingError Clock m)

data ClockingError
  = MismatchClocks Clock Clock
  | UndefinedIdentClk Ident
  | UnexpectedClockProduct [Clock] Clock
  | TooManyClocks [Clock]
  | ClockOccursCheck Clock
  deriving (Show, Eq)

data CAnn c = C c (ClockedExp c)
  deriving (Show, Eq, Functor)

data ClockedExp c
  = Const Const
  | Arr Const (CAnn c)
  | BinOp Op (CAnn c) (CAnn c)
  | Not (CAnn c)
  | Var Ident
  | Merge Ident (CAnn c) (CAnn c)
  | When (CAnn c) Bool Ident
  deriving (Show, Eq, Functor)

type ClockAnn = CAnn Clock

eraseClocks :: ClockAnn -> S.Expression
eraseClocks (C _ (Const c))     = S.Const c
eraseClocks (C _ (Arr c e))     = S.Arr c (eraseClocks e)
eraseClocks (C _ (BinOp o l r)) = S.BinOp o (eraseClocks l) (eraseClocks r)
eraseClocks (C _ (Not e))       = S.Not (eraseClocks e)
eraseClocks (C _ (Var i))       = S.Var i
eraseClocks (C _ (Merge i l r)) = S.Merge i (eraseClocks l) (eraseClocks r)
eraseClocks (C _ (When e c x))  = S.When  (eraseClocks e) c x

withNames :: ClockingM m => [(Ident, Clock)] -> m a -> m a
withNames nms action = do
  env <- get
  put (H nms <> env)
  res <- action
  put env
  return res

lookupName :: ClockingM m => Ident -> m Clock
lookupName i = do
  gets (lookup i . unClockEnv) >>= \case
    Just t  -> pure t
    Nothing -> throwError $ UndefinedIdentClk i

checkClock :: ClockingM m => Clock -> [Clock] -> m ()
checkClock expected given =
  case given of
    [ck] -> ck =?= expected
    cks -> throwError (UnexpectedClockProduct cks expected)

runClocking :: [PreNode] -> Either ClockingError [Node ClockAnn]
runClocking ns =
  fmap fst .
  (flip evalState mempty) .
  runExceptT .
  runUnify (defaultUnifyState :: UnifyState Clock)
  $ (mapM clockOfNode ns)

addNode :: ClockingM m => (Node ClockAnn) -> m ()
addNode _ = do
  pure ()

clockOfNode :: ClockingM m => PreNode -> m (Node ClockAnn)
clockOfNode n@(MkNode{..}) = do
  varClocks <- mapM (\(n,_) -> (,) n <$> fresh) nodeVariables
  withNames varClocks $ withNames (map (fmap (const Base)) $ nodeInputs <> nodeOutputs) $ do
    eqns' <- mapM clockOfEqn nodeEquations

    return (fmap (fmap zonkMeta) $ n { nodeEquations = eqns'})
  where
  zonkMeta (CMeta _) = Base
  zonkMeta (On c t x) = On (zonkMeta c) t x
  zonkMeta i = i

clockOfEqn :: ClockingM m => PreEquation -> m (Equation ClockAnn)
clockOfEqn (MkEq ids expr) = do
  idClocks <- mapM lookupName ids
  (e', expClocks) <- clockOfExpr expr
  mapM_ (uncurry checkClock . second pure) (zip idClocks expClocks)

  pure (MkEq ids e')

clockOfExpr :: ClockingM m => S.Expression -> m (ClockAnn, [Clock])
clockOfExpr (S.Const c) = do
  clk <- fresh
  pure (C clk (Const c), [clk])
clockOfExpr (S.Arr c e) = do
  (e', clk) <- clockOfExpr e
  case clk of
    [clk'] -> pure (C clk' (Arr c e'), [clk'])
    _ -> throwError (TooManyClocks clk)
clockOfExpr (S.BinOp o l r) = do
  (l', lClk) <- clockOfExpr l
  (r', rClk) <- clockOfExpr r
  case lClk of
    [clk] -> checkClock clk rClk >> pure (C clk (BinOp o l' r'), lClk)
    clks -> throwError (TooManyClocks clks)
clockOfExpr (S.Not e) = clockOfExpr e
clockOfExpr (S.Var v) = do
  clk <- lookupName v
  pure (C clk (Var v), [clk])
clockOfExpr (S.When e c x) = do
  xClk <- lookupName x

  (e', clk) <- clockOfExpr e
  checkClock xClk clk

  pure (C (On xClk c x) (When e' c x), [On xClk c x])
clockOfExpr (S.Merge x l r) = do
  xClk <- lookupName x

  (l', lClk) <- clockOfExpr l
  checkClock (On xClk True x) lClk

  (r', rClk) <- clockOfExpr r
  checkClock (On xClk False x) rClk

  pure (C xClk (Merge x l' r'), [xClk])


