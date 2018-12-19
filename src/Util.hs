module Util where

import BasicPrelude
import Data.Aeson.TH (Options(..), defaultOptions)
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Maybe (fromJust)


-- Used to generate Aeson instances
discardingPrefix :: String -> Options
discardingPrefix p = defaultOptions { fieldLabelModifier = flm } where
  flm x = let c0:cs = fromJust $ stripPrefix p x
          in  toLower c0 : cs

