{-# LANGUAGE NoImplicitPrelude #-}
module Imports (
  module Relude
, module Bluefin
, module Classes
, module Data.Conduit
, module Test.Tasty
, module Test.Tasty.HUnit
, Eff, Effects, runPureEff, (:&), (:>), (>>=)
, stdoutBuffering
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

import Data.Conduit hiding (yield)

import Test.Tasty
import Test.Tasty.HUnit

(>>=) ::
  ((forall e. f e -> Eff (e :& es) a) -> Eff es a) ->
  (forall e. f e -> Eff (e :& es) a) ->
  Eff es a
(>>=) handler = handler

stdoutBuffering :: IO a -> IO a
stdoutBuffering action =
  bracket
    (hGetBuffering stdout)
    (hSetBuffering stdout)
    (const action)
