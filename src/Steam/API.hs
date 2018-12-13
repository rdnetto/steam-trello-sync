{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}

module Steam.API (
    ApiKey(..),
    SteamID(..),
    GamesResponse(..),
    GameInfo(..),
    getGames
) where

import BasicPrelude
import Control.Monad.Except (MonadError, liftEither)
import Data.Aeson (FromJSON(..), ToJSON, Value(Object), (.:), (.:?), (.!=), withObject)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API ((:>), Get, JSON, QueryParam', Required, ToHttpApiData(..))
import Servant.Client (BaseUrl(..), ClientM, Scheme(Https), ServantError, client, mkClientEnv, runClientM)


-- Newtypes
newtype ApiKey  = ApiKey Text      -- ^| Steam API key
    deriving (Eq, Show, ToHttpApiData, FromJSON, ToJSON)

newtype SteamID = SteamID Text      -- ^| Steam account ID
    deriving (Eq, Show, ToHttpApiData, FromJSON, ToJSON)

data IBool = ITrue          -- ^| Needed because Steam insists on representing these as ints
           | IFalse
           deriving (Eq, Show)

instance ToHttpApiData IBool where
    toQueryParam ITrue  = "1"
    toQueryParam IFalse = "0"

newtype AppId = AppId Int         -- ^| Game/application ID
    deriving (Eq, Show, FromJSON, ToJSON)

newtype SteamImageHash = SteamImageHash Text   -- ^| Hash representing a steam image
    deriving (Eq, Show, FromJSON, ToJSON)


-- owned games endpoint

-- https://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=$KEY&steamid=76561198093111172&include_appinfo=1&include_played_free_games=1
type OwnedGamesEndpoint
    = "IPlayerService"
    :> "GetOwnedGames"
    :> "v0001"
    :> QueryParam' '[Required] "key" ApiKey
    :> QueryParam' '[Required] "steamid" SteamID
    :> QueryParam' '[Required] "include_appinfo" IBool
    :> QueryParam' '[Required] "include_played_free_games" IBool
    :> Get '[JSON] GamesResponse

ownedGames :: ApiKey
           -> SteamID
           -> IBool
           -> IBool
           -> ClientM GamesResponse

data GamesResponse = GamesResponse {
    gamesCount :: Int,
    gamesList :: [GameInfo]
} deriving (Eq, Show)

instance FromJSON GamesResponse where
    parseJSON = withObject "response" parseRoot where
        parseRoot obj = (withObject "GamesResponse" parseResponse) =<< (obj .: "response")
        parseResponse obj = GamesResponse
            <$> obj .: "game_count"
            <*> obj .: "games"

data GameInfo = GameInfo {
    gameId   :: AppId,
    gameName :: Text,
    playtimeForever :: DiffTime,
    playtimeRecent :: DiffTime,
    gameIcon :: SteamImageHash,
    gameLogo :: SteamImageHash
} deriving (Eq, Show)

instance FromJSON GameInfo where
    parseJSON = withObject "GameInfo" $ \obj -> GameInfo
        <$> obj .: "appid"
        <*> obj .: "name"
        <*> (minutesToDiffTime <$> obj .: "playtime_forever")
        <*> (minutesToDiffTime <$> obj .:? "playtime_2weeks" .!= 0)
        <*> obj .: "img_icon_url"
        <*> obj .: "img_logo_url"


-- Compile the API

type SteamAPI = OwnedGamesEndpoint

steamAPI :: Proxy SteamAPI
steamAPI = Proxy

ownedGames = client steamAPI

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.steampowered.com" 443 "/"


-- Misc functions

minutesToDiffTime :: Integer -> DiffTime
minutesToDiffTime = secondsToDiffTime . (*60)

getGames :: (MonadIO m, MonadError ServantError m)
         => ApiKey -> SteamID -> m GamesResponse
getGames apiKey steamId = translateResultType $ do
    -- Managers are relatively expensive - if we start doing more requests, should factor this out
    manager <- newTlsManager
    let clientEnv = mkClientEnv manager baseUrl
    let q = ownedGames apiKey steamId ITrue ITrue
    runClientM q clientEnv

-- Helper function for converting from Servant's types to our transformer stack
translateResultType :: (MonadIO m, MonadError l m)
                    => IO (Either l r) -> m r
translateResultType = (liftEither =<<) . liftIO

steamImageUrl :: AppId -> SteamImageHash -> Text
steamImageUrl (AppId appId) (SteamImageHash hash) = concat [
        "http://media.steampowered.com/steamcommunity/public/images/apps/",
        tshow appId,
        "/",
        hash,
        ".jpg"
    ]
