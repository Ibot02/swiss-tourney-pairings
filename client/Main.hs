{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
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

main :: IO ()
main = miso $ \currentURI -> App {
          model = getPage currentURI
        , Miso.view = maybe the404 (uncurry viewPage)
        , initialAction = NoOp
        , update = fromTransition . updatePage
        , mountPoint = Nothing
        , events = defaultEvents
        , subs = [uriSub HandleURI]
        , logLevel = Off}


updatePage :: Action -> Transition Action (Maybe (RootLink, Page)) ()
updatePage NoOp = pure ()
updatePage (ChangeURI u) = scheduleIO (pushURI u >> pure NoOp)
updatePage (HandleURI u) = case route (Proxy :: Proxy ClientRoutes) clientPages u of
                                Left _ -> pure ()
                                Right p -> id %= Just . maybe p (switchPage p)
updatePage (PlayerEntryAction (ChangePlayerInputField s)) = _Just . _2 . pageNav . _PagePlayerInput . _1 .= s
updatePage (PlayerEntryAction ApplyPlayerInput) = do
                    p <- preuse $ _Just . _2 . pageNav . _PagePlayerInput . _1
                    case p of
                        Nothing -> pure ()
                        Just (filter (/= "") . lines -> players) -> scheduleIO $ return $ ChangeURI $ goMainPage players [RoundData (replicate (length players) Nothing)] True
updatePage (ChangeResult round changes) = do
                    forM_ changes $ \(p, r) ->
                        _Just . _2 . pageResults . ix round . roundData . ix p %= (case r of
                                (Just r) -> Just . maybe (r, False) (_1 .~ r)
                                Nothing -> const Nothing
                            )
                    preuse (_Just . _2 . to goPage) >>= (\case
                        Nothing -> pure ()
                        Just uri -> scheduleIO $ return $ ChangeURI uri
                        )

--     updatePage (PlayerDrop player) =
--     updatePage FinalizeRound =
