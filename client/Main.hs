{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Common
import Data.Proxy

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (maybe, fromMaybe, catMaybes)

import Miso hiding (at)
import Miso.String (fromMisoString, toMisoString, MisoString())

import Control.Monad
import Control.Lens

import Control.Monad.Writer (tell)

import Language.Javascript.JSaddle as J

import Servant.API
import Servant.Client.Ghcjs

import Data.Aeson (decode)

main :: IO ()
main = do
        pData' :: Maybe JSString <- getElementById ("page-data" :: JSString) ^. J.js ("dataset" :: JSString) ^. J.js ("pageData" :: JSString) >>= J.fromJSVal
        let pData = pData' >>= decode . fromMisoString 
        miso $ \currentURI -> App {
          model = getPageWithData pData currentURI
        , Miso.view = maybe the404 (uncurry viewPage)
        , initialAction = NoOp
        , update = fromTransition . updatePage
        , mountPoint = Nothing
        , events = defaultEvents
        , subs = [uriSub HandleURI]
        , logLevel = Off}

getPlayers :<|> {- putPlayerDrop :<|> -} getData :<|> putResult :<|> nextRound = client (Proxy @ ServersideExtras)

getPageWithData :: (Maybe PageData) -> URI -> Maybe (RootLink, Page)
getPageWithData pData' uri = do
                pData <- pData'
                (l, nav) <- getPageNav uri
                return (l, Page pData True nav)
                

updatePage :: Action -> Transition Action (Maybe (RootLink, Page)) ()
updatePage NoOp = pure ()
updatePage (ChangeURI u) = scheduleIO (pushURI u >> pure NoOp)
updatePage (HandleURI u) = case route (Proxy :: Proxy ClientRoutes) clientPages u of
                                Left _ -> pure ()
                                Right p -> _Just %= switchPage p
-- updatePage (PlayerDrop player) =
updatePage (ChangeResult matchID result) = scheduleIO $ do
        newMatch <- runClientM $ putResult matchID result
        case newMatch of
            Right (Just newMatch) -> pure $ HandleResult matchID newMatch
            _ -> pure NoOp
updatePage (HandleResult matchID matchData) = _Just . _2 . pageData . onMatchID matchID .= matchData
updatePage ComputeNextPairings = scheduleIO $ (runClientM nextRound) >>= \case
        Right (Just (roundID, matches)) -> pure $ HandleNextPairings roundID matches
        _ -> pure NoOp
updatePage (HandleNextPairings roundID matches) = do
    _Just . _2 . pageData . currentRound .= roundID
    _Just . _2 . pageData . rounds <>= [matches]
