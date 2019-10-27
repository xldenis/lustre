{-# LANGUAGE ConstraintKinds            #-}

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
module Lust.Clocks where

import           Control.Monad.State
import           Control.Monad.Unify

import           Data.Bifunctor
import qualified Data.HashMap.Strict as M
import           Data.List.NonEmpty  (NonEmpty (..), toList)
import qualified Data.List.NonEmpty  as NE
import           Data.Maybe
import           Lust.Error
import           Lust.Name
import           Lust.Pretty
import           Lust.Syntax

newtype ClockEnv = H
  { unClockEnv :: [(Ident, Clock)] }
  deriving (Show, Eq, Semigroup, Monoid)

instance Partial Clock where
  unknown = CMeta
  isUnknown (CMeta i) = Just i
  isUnknown _         = Nothing

  unknowns (CMeta i)  = [i]
  unknowns (On c _ _) = unknowns c
  unknowns (CTuple c) = toList c >>= unknowns
  unknowns _          = []

  ($?) sub m@(CMeta i)  = fromMaybe m $ M.lookup i (runSubstitution sub)
  ($?) sub (On clk c x) = On (sub $? clk) c x
  ($?) sub (CTuple xs)  = CTuple (fmap (sub $?) xs)
  ($?) _ Base           = Base

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
  (=?=) (CTuple x) (CTuple y) =
    mapM_ (uncurry =?=) (NE.zip x y)
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

fromClockingError err = Error
  { errHeader   = pretty "Clocking Error"
  , errKind     = "clocking"
  , errSummary  = render err
  }
  where

  render (MismatchClocks ck1 ck2) =
    pretty "Could not unify clock" <+> pretty ck1 <+> pretty "with" <+> pretty ck2
  render (UndefinedIdentClk id) =
    pretty "Could not find a boolean variable" <+> pretty id <+> pretty "in scope"

withNames :: ClockingM m => [(Ident, Clock)] -> m a -> m a
withNames nms action = do
  env <- get
  put (H nms <> env)
  res <- action
  put env
  return res

lookupName :: ClockingM m => Ident -> m Clock
lookupName i =
  gets (lookup i . unClockEnv) >>= \case
  Just t  -> pure t
  Nothing -> throwError $ UndefinedIdentClk i

checkClock :: ClockingM m => Clock -> Clock -> m ()
checkClock expected given = expected =?= given

runClocking :: [PreNode] -> Either (Error ann) [Node Clock]
runClocking ns =
  first fromClockingError .
  flip evalState mempty .
  runExceptT .
  fmap fst .
  runUnify (defaultUnifyState :: UnifyState Clock)
  $ mapM clockOfNode ns

addNode :: ClockingM m => Node Clock -> m ()
addNode _ =
  pure ()

clockOfNode :: ClockingM m => PreNode -> m (Node Clock)
clockOfNode n@MkNode{..} = do
  varClocks <- mapM (\(n,_) -> (,) n <$> fresh) nodeVariables
  withNames varClocks $ withNames (map (fmap (const Base)) $ nodeInputs <> nodeOutputs) $ do
    eqns' <- mapM clockOfEqn nodeEquations

    return $ n { nodeEquations = eqns'}

clockOfEqn :: ClockingM m => PreEquation -> m (Equation Clock)
clockOfEqn (MkEq _ ids expr) = do
  idClocks <- mapM lookupName ids
  (e', expClocks) <- clockOfExpr expr

  toTuple idClocks `checkClock` expClocks

  pure (MkEq (toTuple idClocks) ids e')
  where toTuple (x :| []) = x
        toTuple xss       = CTuple xss

clockOfExpr :: ClockingM m => Expression -> m (Expression, Clock)
clockOfExpr (Const c) = do
  clk <- fresh
  pure (Const c, clk)
clockOfExpr (Arr c e) = do
  (e', clk) <- clockOfExpr e

  pure (Arr c e', clk)
clockOfExpr (BinOp o l r) = do
  (l', lClk) <- clockOfExpr l
  (r', rClk) <- clockOfExpr r
  lClk =?= rClk
  pure (BinOp o l' r', lClk)
clockOfExpr (Not e) = clockOfExpr e
clockOfExpr (Var v) = do
  clk <- lookupName v
  pure (Var v, clk)
clockOfExpr (When e c x) = do
  xClk <- lookupName x

  (e', clk) <- clockOfExpr e
  checkClock xClk clk

  pure (When e' c x, On xClk c x)
clockOfExpr (Merge x l r) = do
  xClk <- lookupName x

  (l', lClk) <- clockOfExpr l
  checkClock (On xClk True x) lClk

  (r', rClk) <- clockOfExpr r
  checkClock (On xClk False x) rClk

  pure (Merge x l' r', xClk)
clockOfExpr (App f args a) = do
  lookupName a
  (args', clks) <- unzip <$> mapM clockOfExpr args
  ck' <- fresh
  pure (App f args' a, ck')


