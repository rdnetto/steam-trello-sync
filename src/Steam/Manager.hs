module Steam.Manager where

import BasicPrelude
import Lens.Micro.Mtl (view)

import AppM
import AppState
import Steam.API


getGames :: AppM GamesResponse
getGames = join $ ownedGames
          <$> (view $ config . steam . apiKey)
          <*> (view $ config . steam . steamID)
          <*> pure ITrue
          <*> pure ITrue

