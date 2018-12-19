{-# LANGUAGE FlexibleContexts #-}

module Trello.Manager where

import BasicPrelude

import Control.Monad.Except (MonadError, liftEither)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (BaseUrl(..), ClientM, Scheme(Https), ServantError, client, mkClientEnv, runClientM)

import AppState
import Trello.API


runMigration :: TrelloMonad m => m ()
runMigration = undefined

initBoard :: TrelloMonad m => m ()
initBoard = undefined

