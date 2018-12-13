module Main where

import BasicPrelude
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import System.Directory (doesFileExist)

import Config
import Steam.API


configPath :: FilePath
configPath = "config.json"

main :: IO ()
main = handleErrors $ do
    configExists <- liftIO $ doesFileExist configPath

    if configExists
       then do
           Config apiKey steamID <- liftIO $ forceRight =<< eitherDecodeFileStrict configPath
           resp <- getGames apiKey steamID
           print . gamesList $ resp
       else do
           liftIO $ encodeFile configPath $ Config (ApiKey "UNKNOWN") (SteamID "UNKNOWN")
           error "No config file provided - creating template. Please populate the template with the correct values."

handleErrors :: Show e => ExceptT e IO a -> IO a
handleErrors x = handle =<< runExceptT x where
    handle (Left err)  = error (show err)
    handle (Right val) = pure val

forceRight :: Either String a -> IO a
forceRight (Left err) = error err
forceRight (Right x)  = pure x

