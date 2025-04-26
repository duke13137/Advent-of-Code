#!/usr/bin/env cabal
{- cabal:
   build-depends: base, optics, relude, breakpoint, bluefin, unliftio
-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021               #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}
import Debug.Breakpoint

import Data.Map.Strict as Map
import Optics
import Relude

import Bluefin.Compound (Handle (mapHandle), useImplIn)
import Bluefin.Eff
import Bluefin.State


main :: IO ()
main = do
  let input = [Just "Hello", Nothing, Just " world", Just "!!!"]
  -- breakpointIO
  print $ input ^.. each % _Just % _head @Text
