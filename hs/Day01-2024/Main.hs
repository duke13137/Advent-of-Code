#!/usr/bin/env cabal
{- cabal:
   build-depends:
     base,breakpoint,bluefin,lens,optics,relude,streamly,unliftio
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

import Streamly.Data.Array qualified as Array
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Scanl qualified as Scanl
import Streamly.Data.Scanr qualified as Scanr
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.StreamK qualified as StreamK
import Streamly.Data.Unfold qualified as Unfold
import Streamly.Internal.Data.Stream (scanr)


main :: IO ()
main = do
  let _ = Lens.over (Lens.taking 2 Lens.worded . Lens.traversed) toUpper "testing one two three"
  let input = [Just "Hello", Nothing, Just " world", Just "!!!"]
  -- breakpointIO
  print $ mapMaybe (viaNonEmpty head) (catMaybes input)
  print $ input ^.. each % _Just % _head
  scanExample

scanExample :: IO ()
scanExample =
    Stream.enumerateFromTo @Int 1 5        -- Stream IO Int
      & scanr scan4                        -- Stream IO Int
      & fmap (+4)                          -- Stream IO Int
      & Stream.fold (Fold.drainMapM print) -- IO ()
  where
    scan1 = Scanr.function (\x -> x * x)
    scan2 = Scanr.function (\x -> 3 * x)
    scan3 = Scanr.teeWith (+) scan1 scan2 -- Compute x^2 + 3x
    scan4 = Scanr.compose scan1 scan3     -- compute x^2 then pass it to scan3
