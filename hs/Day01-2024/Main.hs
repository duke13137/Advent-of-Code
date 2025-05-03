#!/usr/bin/env cabal
{- cabal:
   build-depends:
     base, breakpoint, bluefin, conduit, io-classes, optics, relude
-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021               #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QualifiedDo           #-}
{-# LANGUAGE RecordWildCards       #-}
import Debug.Breakpoint

import Control.Monad.Class.MonadSay
import Data.Conduit hiding (await, yield)
import Data.Conduit.Combinators qualified as C
import Relude hiding (State, evalState, get, modify, put)

import Data.Char
import Optics

import Bluefin.Consume
import Bluefin.EarlyReturn
import Bluefin.Eff
import Bluefin.Exception
import Bluefin.IO
import Bluefin.Jump
import Bluefin.State
import Bluefin.StateSource
import Bluefin.Stream hiding (catMaybes, mapMaybe)

main :: IO ()
main = do
  let input = [Just "Hello", Nothing, Just " world", Just "!!!"]
  print $ mapMaybe (viaNonEmpty head) (catMaybes input)
  print $ input ^.. each % _Just % _head
  print stateSourceExample
  print returnEarlyExample
  consumeStreamExample >>= print
  -- breakpointIO

returnEarlyExample :: String
returnEarlyExample = runPureEff $ withEarlyReturn \e -> do
  for_ [1 :: Int .. 10] $ \i -> do
    when (i >= 5) do
      returnEarly e ("Returned early with " ++ show i)
  pure "End of loop"

stateSourceExample :: Int
stateSourceExample = runPureEff $ withStateSource \source -> do
  n <- newState source 5
  total <- newState source 0

  withJump $ \done -> forever do
    n' <- get n
    modify total (+ n')
    when (n' == 0) $ jumpTo done
    modify n (subtract 1)

  get total

consumeStreamExample :: IO (Either String String)
consumeStreamExample = runEff_ $ \io -> do
  try $ \ex -> do
    consumeStream
      ( \r -> bracket
          (effIO io (say "Starting 2"))
          (\_ -> effIO io (say "Leaving 2"))
          $ \_ -> do
            for_ [1 :: Int .. 9] $ \n -> do
              b <- await r
              effIO io $ say ("Consumed body " ++ show b ++ " at time " ++ show n)
            pure "Consumer finished first"
      )
      ( \y -> bracket
          (effIO io $ say "Starting 1")
          (\_ -> effIO io $ say "Leaving 1")
          $ \_ -> do
            for_ [1 :: Int .. 10] $ \n -> do
              effIO io (say ("Sending " ++ show n))
              yield y n
              when (n > 5) $ do
                effIO io (say "Aborting...")
                throw ex "Aborted"

            pure "Yielder finished first"
      )
