{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module AppState where

import BasicPrelude
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLenses, makeFields)
import Servant.Client (ClientEnv)

import Steam.API as S
import Trello.API as T


-- State available through out program
-- Like all good functional programmers, our "state" is actually immutable :)
data AppState = AppState {
    _config    :: Config,
    _trelloEnv :: ClientEnv,
    _steamEnv  :: ClientEnv
}

-- Serialization format for persistent config. Enables us to avoid hardcoding secrets.
data Config = Config {
    _steam  :: SteamConfig,
    _trello :: TrelloConfig
} deriving (Eq, Show, Generic)

instance FromJSON Config
instance ToJSON Config


data SteamConfig = SteamConfig {
    steamConfigApiKey  :: S.ApiKey,
    steamConfigSteamID :: SteamID
} deriving (Eq, Show, Generic)

instance FromJSON SteamConfig
instance ToJSON SteamConfig


data TrelloConfig = TrelloConfig {
    trelloConfigApiKey    :: T.ApiKey,
    trelloConfigAuthToken :: AuthToken
} deriving (Eq, Show, Generic)

instance FromJSON TrelloConfig
instance ToJSON TrelloConfig

-- Default values for generating file
templateConfig :: Config
templateConfig = Config (SteamConfig "UNKNOWN" "UNKNOWN") (TrelloConfig "UNKNOWN" "UNKNOWN")

-- Lenses (makeFields handles overloaded fields)
makeLenses ''AppState
makeLenses ''Config
makeFields ''SteamConfig
makeFields ''TrelloConfig

