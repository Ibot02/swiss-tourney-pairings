{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Common

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toLower)
import Data.Maybe (fromJust, fromMaybe)
import Data.Proxy
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Servant
import qualified Lucid as L
import Lucid.Base
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import Network.HTTP.Types hiding (Header)
import qualified System.IO as IO
import qualified System.Environment as Env
import Text.Read (readMaybe)
import System.FilePath

import Control.Monad (when, forM_)
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import GHC.Conc

import Miso

addResult (PageData players []) = PageData players $ [RoundData $ replicate (length players) Nothing]
addResult d = d

main = do
    p' <- Env.lookupEnv "PORT"
    path <- Env.getExecutablePath
    let p = case (p' >>= readMaybe @Int) of
            Nothing -> 3002
            Just p -> p
        staticPath = fst (splitFileName path) ++ "/../static"
        dataPath = "./data/"
    IO.hPutStrLn IO.stderr $ "Starting Server on port " ++ show p
    IO.hPutStrLn IO.stderr $ "Serving static files from " ++ staticPath
    IO.hPutStrLn IO.stderr $ "Looking for data in " ++ dataPath
    (maybe (PageData [] []) addResult -> (PageData players results)) <- decodeFileStrict $ dataPath <> "pageData.json"
    (playersV, resultsV) <- atomically $ do
        p <- newTVar players
        r <- newTVar results
        return (p,r)
    run p $ serve (Proxy @ API) (static staticPath :<|> serverHandlers playersV resultsV :<|> dataHandlers playersV resultsV :<|> pure manifest :<|> Tagged handle404) where
        static path = serveDirectoryWith (defaultWebAppSettings path)

type ServerRoutes = QueryFlag "interactive" :> ToServerRoutes ClientRoutes Wrapper Action

type API = ("static" :> Raw)
        :<|> ServerRoutes
        :<|> ServersideExtras
        :<|> ("manifest.json" :> Get '[JSON] Manifest)
        :<|> Raw

newtype Wrapper a = Wrapper (Text, Bool, a)
    deriving (Show, Eq)

data Manifest = Manifest {
    manifestName :: Text
  , manifestShort_Name :: Text
  , manifestStart_Url :: Text
  , manifestDisplay :: Text
  , manifestTheme_Color :: Text
  , manifestBackground_Color :: Text
  , manifestDescription :: Text
  , manifestIcons :: [ManifestIcon]
  } deriving (Show, Eq, Generic)

data ManifestIcon = ManifestIcon {
      manifestIconSrc :: Text
    , manifestIconType :: Text
    , manifestIconSizes :: Text
    } deriving (Show, Eq, Generic)

instance L.ToHtml a => L.ToHtml (Wrapper a) where
    toHtmlRaw = L.toHtml
    toHtml (Wrapper (root, interactive, x)) = do
        L.doctype_
        L.html_ [ L.lang_ "en" ] $ do
            L.head_ $ do
                L.title_ "TMCR Swiss Pairing Generator"
                L.link_ [ L.rel_ "manifest"
                        , L.href_ (root <> "manifest.json")
                        ]
                L.meta_ [ L.charset_ "utf-8" ]
                L.meta_ [ L.name_ "viewport"
                        , L.content_ "width=device-width, initial-scale=1"
                        ]
                L.meta_ [ L.name_ "description"
                        , L.content_ "Generating Pairings for the swiss round of the TMCR tournament, accounting for the special 3-way match rule to resolve an odd number of players"
                        ]
                when interactive $ 
                    L.with (L.script_ mempty) [
                          makeAttribute "src" (root <> "static/all.js")
                        , makeAttribute "async" mempty
                        , makeAttribute "defer" mempty
                        ]
                L.link_ [ L.rel_ "stylesheet"
                        , L.type_ "text/css"
                        , L.href_ (root <> "static/style.css")
                        ]
                L.link_ [ L.rel_ "icon"
                        , L.type_ "image/png"
                        , L.href_ (root <> "static/favicon.png")
                        ]
            L.body_ (L.toHtml x)

manifest :: Manifest
manifest = Manifest {
      manifestName = "TMCR Swiss Pairings Generator"
    , manifestShort_Name = "Swiss Pairings"
    , manifestStart_Url = "."
    , manifestDisplay = "standalone"
    , manifestTheme_Color = "1D2731"
    , manifestBackground_Color = "1D2731"
    , manifestDescription = "Generating Pairings for the swiss round of the TMCR tournament, accounting for the special 3-way match rule to resolve an odd number of players"
    , manifestIcons = [] -- [ManifestIcon "/static/favicon.png" "image/png" "32x32"]
    }

handle404 :: Application
handle404 _ respond = respond $ responseLBS
    status404
    [("Content-Type", "text/html")] $
        renderBS $ toHtml $ Wrapper ("/", True, the404)

serverHandlers :: TVar [PlayerName] -> TVar [RoundData] -> Server ServerRoutes
serverHandlers playersV resultsV interactive = mainPage :<|> roundPage :<|> standingsPage :<|> playerPage :<|> playersPage where
    mainPage = send "./" interactive $ PageStandings Nothing
    roundPage r = send "../../" interactive $ PageRound r
    standingsPage r = send "../" interactive $ PageStandings $ Just r
    playerPage r = send "../../" interactive $ PagePlayer r
    playersPage = send' "../../" interactive $ \players -> PagePlayerInput (unlines players) Nothing
    send root interactive p = send' root interactive (const p)
    send' root interactive p = liftIO $ do
        (players, results) <- atomically $ do
            p <- readTVar playersV
            r <- readTVar resultsV
            return (p,r)
        pure $ Wrapper (root, interactive, viewPage root (Page (PageData players results) interactive (p players)))

dataHandlers :: TVar [PlayerName] -> TVar [RoundData] -> Server ServersideExtras
dataHandlers playersV resultsV = getPlayers :<|> putPlayers :<|> getResults :<|> putResults :<|> postNextRound where
    getPlayers = liftIO $ atomically $ readTVar playersV
    putPlayers players = liftIO $ do
        atomically $ do
            oldPlayers <- readTVar playersV
            if length oldPlayers == length players then
                writeTVar playersV players
            else do
                writeTVar playersV players
                writeTVar resultsV [RoundData $ replicate (length players) Nothing]
        return ()
    getResults = liftIO $ atomically $ readTVar resultsV
    putResults round results = liftIO $ atomically $ do
                forM_ results $ \(p, r) ->
                    modifyTVar resultsV $ ix round . roundData . ix p .~ r
                readTVar resultsV
    postNextRound = liftIO $ atomically $ do
            p <- length <$> readTVar playersV
            modifyTVar resultsV $ (<> [RoundData $ replicate p Nothing])
        

$(deriveJSON Data.Aeson.TH.defaultOptions{fieldLabelModifier = (camelTo2 '_' . drop (length ("manifestIcon" :: String)))} ''ManifestIcon)
$(deriveJSON Data.Aeson.TH.defaultOptions{fieldLabelModifier = (camelTo2 '_' . drop (length ("manifest" :: String)))} ''Manifest)

