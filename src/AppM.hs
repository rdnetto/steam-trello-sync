{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types #-}

module AppM (AppM, runAppM) where

import BasicPrelude
import Control.Monad.Except (MonadError, ExceptT, liftEither, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Lens.Micro (_Left, over)
import Lens.Micro.Mtl (view)
import Lens.Micro.Type (Lens')
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (ClientEnv, ClientM, ServantError, mkClientEnv, runClientM)

import AppState
import Steam.API as S (SteamMonad(..), baseUrl)
import Trello.API as T (TrelloMonad(..), baseUrl)


-- The monad transformer stack for the application.
newtype AppM a = AppM (ReaderT AppState (ExceptT String IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppState, MonadError String)

instance SteamMonad AppM where
  liftSteamClient = liftClient steamEnv

instance TrelloMonad AppM where
  liftTrelloClient = liftClient trelloEnv
  askTrelloApiKey    = view $ config . trello . apiKey
  askTrelloAuthToken = view $ config . trello . authToken


-- Helper function for lifting ClientM into our transformer stack
liftClient :: Lens' AppState ClientEnv -> ClientM a -> AppM a
liftClient envLens clientM = do
  env <- view envLens
  res :: Either ServantError a
      <- liftIO $ runClientM clientM env
  liftEither $ (_Left `over` show) res

-- Actually run the transformer stack
runAppM :: Config -> AppM a -> IO a
runAppM cfg (AppM appM) = do
  let mkEnv url = flip mkClientEnv url <$> newTlsManager
  appState <- AppState cfg
            <$> mkEnv T.baseUrl
            <*> mkEnv S.baseUrl
  res <- runExceptT $ runReaderT appM appState
  case res of
       Right x  -> pure x
       Left err -> error err

