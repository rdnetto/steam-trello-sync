{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module AppState where

import BasicPrelude
import Data.Aeson.TH (deriveJSON)
import Lens.Micro.TH (makeLenses, makeFields)
import Servant.Client (ClientEnv)

import Steam.API as S
import Trello.API as T
import Util


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
} deriving (Eq, Show)

data SteamConfig = SteamConfig {
    steamConfigApiKey  :: S.ApiKey,
    steamConfigSteamID :: SteamID
} deriving (Eq, Show)

data TrelloConfig = TrelloConfig {
    trelloConfigApiKey    :: T.ApiKey,
    trelloConfigAuthToken :: AuthToken
} deriving (Eq, Show)

-- Default values for generating file
templateConfig :: Config
templateConfig = Config (SteamConfig "UNKNOWN" "UNKNOWN") (TrelloConfig "UNKNOWN" "UNKNOWN")

-- Lenses (makeFields handles overloaded fields)
makeLenses ''AppState
makeLenses ''Config
makeFields ''SteamConfig
makeFields ''TrelloConfig

-- Aeson instances
deriveJSON (discardingPrefix "_")            ''Config
deriveJSON (discardingPrefix "steamConfig")  ''SteamConfig
deriveJSON (discardingPrefix "trelloConfig") ''TrelloConfig
