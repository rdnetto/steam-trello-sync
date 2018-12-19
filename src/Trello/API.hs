{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}

module Trello.API where

import BasicPrelude
import Data.Aeson (FromJSON(..), ToJSON, Value(Object), (.:), (.:?), (.!=), withObject)
import Data.Proxy (Proxy(..))
import Servant.API ((:>), Capture, Get, JSON, QueryParam, QueryParam', Required, ToHttpApiData(..))
import Servant.Client (BaseUrl(..), ClientM, Scheme(Https), ServantError, client, hoistClient)


class Monad m => TrelloMonad m where
  liftTrelloClient   :: ClientM a -> m a
  askTrelloAuthToken :: m AuthToken
  askTrelloApiKey    :: m ApiKey

-- Authentication types

newtype ApiKey  = ApiKey Text
    deriving (Eq, Show, ToHttpApiData, FromJSON, ToJSON, IsString)

newtype AuthToken  = AuthToken Text
    deriving (Eq, Show, ToHttpApiData, FromJSON, ToJSON, IsString)

newtype UserID  = UserID Text
    deriving (Eq, Show, ToHttpApiData, FromJSON, ToJSON, IsString)


-- Tokens endpoint

type TokenInfoEndpoint
    = "tokens"
    :> Capture "token" AuthToken
    :> QueryParam' '[Required] "key" ApiKey
    :> QueryParam' '[Required] "token" AuthToken
    :> QueryParam "fields" Text                     -- actually a CSV list
    :> Get '[JSON] TokenInfo

tokenInfo :: TrelloMonad m
          => AuthToken
          -> ApiKey
          -> AuthToken
          -> Maybe Text
          -> m TokenInfo

data TokenInfo = TokenInfo {
  tokenId   :: AuthToken,
  tokenUser :: UserID
}

instance FromJSON TokenInfo where
    parseJSON = withObject "TokenInfo" $ \obj -> TokenInfo
        <$> obj .: "id"
        <*> obj .: "idMember"


-- Finally, compile the API

type TrelloAPI = TokenInfoEndpoint

trelloAPI :: Proxy TrelloAPI
trelloAPI = Proxy

tokenInfo = hoistClient trelloAPI liftTrelloClient (client trelloAPI)

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.trello.com" 443 "/1/"

