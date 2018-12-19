{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}

module Steam.API where

import BasicPrelude
import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSON(..), ToJSON, Value(Object), (.:), (.:?), (.!=), withObject)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Servant.API ((:>), Get, JSON, QueryParam', Required, ToHttpApiData(..))
import Servant.Client (BaseUrl(..), ClientM, Scheme(Https), ServantError, client, hoistClient)


class Monad m => SteamMonad m where
  liftSteamClient :: ClientM a -> m a

-- Newtypes
newtype ApiKey  = ApiKey Text      -- ^| Steam API key
    deriving (Eq, Show, ToHttpApiData, FromJSON, ToJSON, IsString)

newtype SteamID = SteamID Text      -- ^| Steam account ID
    deriving (Eq, Show, ToHttpApiData, FromJSON, ToJSON, IsString)

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

ownedGames :: SteamMonad m
           => ApiKey
           -> SteamID
           -> IBool
           -> IBool
           -> m GamesResponse

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

ownedGames = hoistClient steamAPI liftSteamClient (client steamAPI)

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.steampowered.com" 443 "/"


-- Misc functions

minutesToDiffTime :: Integer -> DiffTime
minutesToDiffTime = secondsToDiffTime . (*60)

steamImageUrl :: AppId -> SteamImageHash -> Text
steamImageUrl (AppId appId) (SteamImageHash hash) = concat [
        "http://media.steampowered.com/steamcommunity/public/images/apps/",
        tshow appId,
        "/",
        hash,
        ".jpg"
    ]
