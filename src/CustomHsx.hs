{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CustomHsx where

import Imports

import Data.Set qualified as Set
import IHP.HSX.Parser
import IHP.HSX.QQ (customHsx)
import Language.Haskell.TH.Quote

hsx :: QuasiQuoter
hsx= customHsx
    (HsxSettings
        { checkMarkup = True
        , additionalTagNames = Set.fromList ["book", "heading", "name"]
        , additionalAttributeNames = Set.fromList ["_", "hx-boost"]
        }
    )
