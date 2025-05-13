{-# LANGUAGE NoImplicitPrelude #-}
module Imports (
  module Relude
, module Bluefin
, module Classes
, module Data.Conduit
, module Data.Strict.Wrapper
, module Test.Tasty
, module Test.Tasty.HUnit
, Eff, Effects, runPureEff, (:&), (:>), (>>=)
, sinkFold, sinkFoldM, sinkFold_, sinkFoldM_
, tasty
) where

import Relude hiding (STM, State, atomically, evalState, get, modify, put, runState, throwSTM, uncons, withState, (>>=))

import Bluefin.EarlyReturn as Bluefin
import Bluefin.Eff (Eff, Effects, runPureEff, (:&), (:>))
import Bluefin.Jump as Bluefin
import Bluefin.State as Bluefin
import Bluefin.StateSource as Bluefin

import Control.Monad.Class.MonadAsync as Classes
import Control.Monad.Class.MonadFork as Classes
import Control.Monad.Class.MonadSay as Classes
import Control.Monad.Class.MonadSTM as Classes
import Control.Monad.Class.MonadThrow as Classes
import Control.Monad.Class.MonadTime as Classes
import Control.Monad.Class.MonadTimer as Classes

import Control.Foldl qualified as L
import Data.Conduit hiding (yield)
import Data.Conduit.Combinators qualified as C
import Data.Strict.Wrapper

import Test.Tasty
import Test.Tasty.HUnit

(>>=) ::
  ((forall e. f e -> Eff (e :& es) a) -> Eff es a) ->
  (forall e. f e -> Eff (e :& es) a) ->
  Eff es a
(>>=) handler = handler

sinkFold :: Monad m => L.Fold a b -> ConduitT a o m b
sinkFold = L.purely sinkFold_

sinkFoldM :: Monad m => L.FoldM m a b -> ConduitT a o m b
sinkFoldM = L.impurely sinkFoldM_

sinkFold_ :: Monad m => (x -> a -> x) -> x -> (x -> b) -> ConduitT a o m b
sinkFold_ combine seed extract = fmap extract (C.foldl combine seed)

sinkFoldM_ :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> ConduitT a o m b
sinkFoldM_ combine seed extract =
  lift . extract =<< C.foldM combine =<< lift seed

tasty :: TestTree -> IO ()
tasty action =
  bracket
    (hGetBuffering stdout)
    (hSetBuffering stdout)
    (const $ defaultMain action)
