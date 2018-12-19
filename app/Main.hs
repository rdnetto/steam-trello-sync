module Main where

import BasicPrelude
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import System.Directory (doesFileExist)

import AppM
import AppState
import Steam.API (gamesList)
import Steam.Manager
import Trello.Manager


configPath :: FilePath
configPath = "config.json"

main :: IO ()
main = do
    configExists <- doesFileExist configPath

    if configExists
       then do
           cfg <- unsafeRight <$> eitherDecodeFileStrict configPath

           runAppM cfg $ do
               print . gamesList =<< getGames

       else do
           liftIO $ encodeFile configPath templateConfig
           error "No config file provided - creating template. Please populate the template with the correct values."

unsafeRight :: Either String r -> r
unsafeRight (Right r) = r
unsafeRight (Left msg) = error msg

