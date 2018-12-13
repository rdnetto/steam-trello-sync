{-# LANGUAGE DeriveGeneric #-}

module Config where

import BasicPrelude
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Steam.API

-- Serialization format for persistent config. Enables us to avoid hardcoding secrets.
data Config = Config {
    steamApiKey :: ApiKey,
    steamId     :: SteamID
} deriving (Eq, Show, Generic)

instance FromJSON Config
instance ToJSON Config
