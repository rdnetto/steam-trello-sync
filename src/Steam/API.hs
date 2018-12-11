{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}

module Steam.API where

import BasicPrelude
import Data.Aeson (FromJSON(..), (.:), withObject)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Servant.API ((:>), Get, JSON, QueryParam, ToHttpApiData(..))
import Servant.Client (BaseUrl(..), ClientM, Scheme(Https), ServantError, client)


-- Types

-- https://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=$KEY&steamid=76561198093111172&include_appinfo=1&include_played_free_games=1
type SteamAPI =  "IPlayerService"
              :> "GetOwnedGames"
              :> "v0001"
              :> QueryParam "key" ApiKey
              :> QueryParam "steamid" SteamID
              :> QueryParam "include_appinfo" IBool
              :> QueryParam "include_played_free_games" IBool
              :> Get '[JSON] GamesResponse

steamBaseUrl :: BaseUrl
steamBaseUrl = BaseUrl Https "api.steampowered.com" 443 "/"

newtype ApiKey  = ApiKey Text      -- ^| Steam API key
    deriving (Eq, Show, ToHttpApiData)

newtype SteamID = SteamID Text      -- ^| Steam account ID
    deriving (Eq, Show, ToHttpApiData)

data IBool = ITrue          -- ^| Needed because Steam insists on representing these as ints
           | IFalse
           deriving (Eq, Show)

instance ToHttpApiData IBool where
    toQueryParam ITrue  = "1"
    toQueryParam IFalse = "0"

newtype AppId = AppId Int         -- ^| Game/application ID
    deriving (Eq, Show, FromJSON)

newtype SteamImageHash = SteamImageHash Text   -- ^| Hash representing a steam image
    deriving (Eq, Show, FromJSON)

data GamesResponse = GamesResponse {
    gamesCount :: Int,
    games :: [GameInfo]
} deriving (Eq, Show)

instance FromJSON GamesResponse where
    parseJSON = withObject "GamesResponse" $ \obj -> GamesResponse
        <$> obj .: "game_count"
        <*> obj .: "games"

data GameInfo = GameInfo {
    gameId   :: AppId,
    gameName :: Text,
    playtimeForever :: DiffTime,
    gameIcon :: SteamImageHash,
    gameLogo :: SteamImageHash
} deriving (Eq, Show)

instance FromJSON GameInfo where
    parseJSON = withObject "GameInfo" $ \obj -> GameInfo
        <$> obj .: "app_id"
        <*> obj .: "name"
        <*> (minutesToDiffTime <$> obj .: "playtime_forever")
        <*> obj .: "img_icon_url"
        <*> obj .: "img_logo_url"


-- Functions

minutesToDiffTime :: Integer -> DiffTime
minutesToDiffTime = secondsToDiffTime . (*60)

steamAPI :: Proxy SteamAPI
steamAPI = Proxy

getGames :: Maybe ApiKey
         -> Maybe SteamID
         -> Maybe IBool
         -> Maybe IBool
         -> ClientM GamesResponse
getGames = client steamAPI

steamImageUrl :: AppId -> SteamImageHash -> Text
steamImageUrl (AppId appId) (SteamImageHash hash) = concat [
        "http://media.steampowered.com/steamcommunity/public/images/apps/",
        tshow appId,
        "/",
        hash,
        ".jpg"
    ]
