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
import Data.Maybe (maybe, fromMaybe)

import Miso hiding (at)
import Miso.String (fromMisoString, toMisoString, MisoString())

import Control.Monad
import Control.Lens

import Control.Monad.Writer (tell)

import Language.Javascript.JSaddle as J

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
                                Right (p@(r,p')) -> id %= Just . maybe (r, Page (PageData [] []) True p') (switchPage p)
updatePage (PlayerEntryAction (ChangePlayerInputField s)) = _Just . _2 . pageNav . _PagePlayerInput . _1 .= s
updatePage (PlayerEntryAction ApplyPlayerInput) = do
                    p <- preuse $ _Just . _2 . pageNav . _PagePlayerInput
                    case p of
                        Nothing -> pure ()
                        Just ((filter (/= "") . lines -> players), (fromMaybe (PageStandings Nothing) -> nextPage)) -> do
                            _Just . _2 . pageData . pagePlayers .= players
                            scheduleIO $ return $ ChangeURI $ goPage nextPage
updatePage (ChangeResult round changes) =
                    forM_ changes $ \(p, r) ->
                        _Just . _2 . pageData . pageResults . ix round . roundData . ix p %= (case r of
                                (Just r) -> Just . maybe (r, False) (_1 .~ r)
                                Nothing -> const Nothing
                            )
updatePage AddRound = _Just . _2 . pageData %= (\(PageData players results) -> PageData players $ results <> [RoundData $ replicate (length players) Nothing])
--     updatePage (PlayerDrop player) =
--     updatePage FinalizeRound =
