#!/usr/bin/env cabal
{- cabal:
   build-depends:
     base,breakpoint,bluefin,lens,optics,relude,unliftio
-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021               #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE RecordWildCards       #-}

import Debug.Breakpoint
import Optics
import Relude hiding (scanr)

import Control.Lens qualified as Lens
import Data.Char

import Bluefin.Compound (Handle (mapHandle), useImplIn)
import Bluefin.Eff
import Bluefin.State

main :: IO ()
main = do
  let _ = Lens.over (Lens.taking 2 Lens.worded . Lens.traversed) toUpper "testing one two three"
  let input = [Just "Hello", Nothing, Just " world", Just "!!!"]
  -- breakpointIO
  print $ mapMaybe (viaNonEmpty head) (catMaybes input)
  print $ input ^.. each % _Just % _head
