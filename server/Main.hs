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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Common

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toLower)
import Data.Maybe (fromJust, fromMaybe)
import Data.Proxy
import GHC.Generics hiding (to)
import Data.Aeson hiding ((.=))
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

import Data.List (sort, groupBy)

import Control.Monad (when, forM_)
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import GHC.Conc

import Miso

import Control.Monad.State
import Control.Monad.Writer
import System.Random

import Debug.Trace

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
    let pageDataPath = dataPath <> "pageData.json"
    pageData <- decodeFileStrict pageDataPath
    let defaultPageData = PageData [ParticipantData "Example" 1 Nothing] 2 [] 1 Nothing (Top8Data mempty) 0
    case pageData of
        Nothing -> do
            IO.hPutStrLn IO.stderr $ "No pageData found, aborting"
        Just pageData -> do
            pageDataV <- atomically $ newTVar pageData
            let incrementRound = do
                    pageData <- readTVar pageDataV
                    if (pageData ^. currentRound) < (pageData ^. numRounds)
                    then do
                        case makePairings pageData of
                            Nothing -> return Nothing
                            Just (nextRound, matches) -> do
                                let newPageData = pageData
                                        & currentRound .~ nextRound
                                        & rounds <>~ [matches]
                                writeTVar pageDataV newPageData
                                return $ Just (nextRound, matches)
                    else return Nothing
                incrementTillRoundOneOrGreater = do
                    pageData <- readTVar pageDataV
                    if (pageData ^. currentRound) < 1 then do
                        incrementRound
                        incrementTillRoundOneOrGreater
                    else return ()
            atomically $ incrementTillRoundOneOrGreater
            run p $ serve (Proxy @ API) (static staticPath :<|> serverHandlers pageDataV :<|> dataHandlers pageDataV pageDataPath :<|> pure manifest :<|> Tagged handle404) where
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

serverHandlers :: TVar PageData -> Server ServerRoutes
serverHandlers pageDataV interactive = mainPage :<|> roundPage :<|> standingsPage :<|> playerPage :<|> playersPage where
    mainPage = send "./" interactive $ PageStandings Nothing
    roundPage r = send "../../" interactive $ PageRound r
    standingsPage r = send "../" interactive $ PageStandings $ Just r
    playerPage r = send "../../" interactive $ PagePlayer r
    playersPage = send "../../" interactive $ PagePlayers
    send root interactive p = liftIO $ do
        pageData <- atomically $ readTVar pageDataV
        pure $ Wrapper (root, interactive, viewPage root (Page pageData interactive p))

dataHandlers :: TVar PageData -> FilePath -> Server ServersideExtras
dataHandlers pageDataV pageDataPath = getPlayers :<|> {- putPlayerDrop :<|> -} getData :<|> putResult :<|> postNextRound :<|> writeToDisk where
    getPlayers = liftIO $ fmap (^.. participants . traverse . participantName) $ atomically $ readTVar pageDataV
    putPlayerDrop playerID = undefined
    getData = liftIO $ atomically $ readTVar pageDataV
    putResult matchID result = liftIO $ atomically $ do
            pageData <- readTVar pageDataV
            case pageData ^? onMatchID matchID of
                Nothing -> return Nothing
                Just oldMatch -> do
                    if --validation
                        (length result <= oldMatch ^. seriesLength)
                     && (all ((== (oldMatch ^. players . to length)) . length) result)
                    then do
                        let newMatch = results .~ result $ oldMatch
                            newPageData = onMatchID matchID .~ newMatch $ pageData
                        writeTVar pageDataV newPageData
                        return $ Just newMatch
                    else return $ Just oldMatch
    postNextRound = liftIO $ atomically $ do
        pageData <- readTVar pageDataV
        if (pageData ^. currentRound) < (pageData ^. numRounds)
        then do
            case makePairings pageData of
                Nothing -> return Nothing
                Just (nextRound, matches) -> do
                    let newPageData = pageData
                            & currentRound .~ nextRound
                            & rounds <>~ [matches]
                    writeTVar pageDataV newPageData
                    return $ Just (nextRound, matches)
        else return Nothing
    writeToDisk = liftIO $ do
        pageData <- atomically $ readTVar pageDataV
        encodeFile pageDataPath pageData


makePairings :: PageData -> Maybe (RoundID, [MatchData])
makePairings pageData = (,) (round + 1) <$> pairings where
    round = pageData ^. currentRound
    newRound = (pageData ^. rounds . to length) + 1
    ranks = listStandings pageData round ^.. traverse . _1
    dropped = pageData ^.. participants . traverse . roundDropped . to (maybe False (<= newRound))
    skipping = pageData ^.. participants . traverse . roundSkipped . to (== newRound)
    rng = fst $ split $ foldl (\g _ -> snd $ split g) (pageData ^. rngSeed . to mkStdGen) $ pageData ^. rounds --different starting prng for each round, but deterministic
    ignore = zipWith (||) dropped skipping
    groupedByRank = fmap (fmap snd) $ groupBy (\x y -> fst x == fst y) $ sort $ fmap (\(x,y) -> (x,fst y)) $ filter (not . snd . snd) $ zip ranks $ ignore ^.. itraversed . withIndex
    need3Way = (length (concat groupedByRank) `mod` 2) == 1
    hasBeenIn3Way = pageData ^.. rounds . traverse . traverse . players . filtered ((>2) . length) . traverse
    previousPairings = pageData ^.. rounds . traverse . traverse . players
    isPreviousPairing p1 p2 = any (\xs -> p1 `elem` xs && p2 `elem` xs) previousPairings
    get3Way = zoom (alongside reversed id) $ do
        x <- popFirstRandom
        guard $ x `notElem` hasBeenIn3Way
        y <- popInnerRandom
        guard $ y `notElem` hasBeenIn3Way
        z <- popInnerRandom
        guard $ z `notElem` hasBeenIn3Way
        guard $ not $ all (uncurry isPreviousPairing) [(x,y),(x,z),(y,z)]
        m <- return $ MatchData [z,y,x] 1 []
        () <- traceShow m $ return ()
        return m
    getMatch = do
        x <- popFirstRandom
        y <- popInnerRandom
        guard $ not $ isPreviousPairing x y
        m <- return $ MatchData [x,y] 1 []
        () <- traceShow m $ return ()
        return m
    getMatches = do
        moreMatches <- gets $ has $ _1 . traverse . traverse
        if moreMatches then do
            x <- getMatch
            xs <- getMatches
            return $ x:xs
        else return []
    pairings = (^? _head) $ (id :: [a] -> [a]) $ flip evalStateT (groupedByRank, rng) $ do
        traceM $ "Generating pairings for round " <> show newRound
        if need3Way
        then do
            x <- get3Way
            xs <- getMatches
            return $ xs <> [x]
        else getMatches

popFirstRandom :: (MonadPlus m) => StateT ([[a]], StdGen) m a
popFirstRandom = do
    _1 %= filter (not . null)
    xs'' <- zoom _1 popS
    xs <- case xs'' of
        Nothing -> mzero
        Just xs -> return xs
    (x,xs') <- zoom _2 $ popOneRandomly xs
    when (not (null xs')) $ _1 %= (xs':)
    pure x

popS :: (Monad m) => StateT [a] m (Maybe a)
popS = state $ \case
    [] -> (Nothing, [])
    x:xs -> (Just x,xs)

--pop an element from the first inner list randomly, if that fails try the second and so on
popInnerRandom :: (MonadPlus m) => StateT ([[a]], StdGen) m a
popInnerRandom = do
    _1 %= filter (not . null)
    xs <- use _1
    (x, xs') <- zoom _2 $ do
        foldr mplus mzero $ fmap (\(ys, x, xs) -> do
            (r, x') <- popAnyRandomly x
            return (r, unzipper (ys, x', xs))
            ) $ zippered xs
    _1 .= xs'
    return x

zippered :: [a] -> [([a],a,[a])]
zippered [] = []
zippered (x:xs) = helper ([],x,xs) where
    helper x@(_,_,[]) = [x]
    helper x@(ys,y,y':xs) = x:helper (y:ys,y',xs)

unzipper :: ([a],a,[a]) -> [a]
unzipper ([],x,xs) = x:xs
unzipper (y:ys,x,xs) = unzipper (ys,y,x:xs)

popAnyRandomly :: (MonadPlus m) => [a] -> StateT StdGen m (a, [a])
popAnyRandomly xs = do
    let l = length xs
    if l == 0 then mzero else do
        order <- randomPermutation (l-1)
        foldr mplus mzero $ fmap (\i -> case pop i xs of
            Nothing -> mzero
            Just (x,xs') -> return (x,xs')) order

popOneRandomly :: (MonadPlus m) => [a] -> StateT StdGen m (a, [a])
popOneRandomly xs = do
    i <- state $ randomR (0, length xs - 1)
    case pop i xs of
        Nothing -> mzero
        Just (x,xs') -> return (x,xs')

randomPermutation :: (Monad m) => Int -> StateT StdGen m [Int]
randomPermutation 0 = return [0]
randomPermutation n = do
    i <- state $ randomR (0,n)
    p <- randomPermutation (n-1)
    return $ insertTo i p where
        insertTo 0 p = let incremented = increment p in incremented `seq` 0:incremented
        insertTo n (!p:ps) = p:insertTo (n-1) ps
        increment [] = []
        increment (!x:xs) = let incremented = increment xs in incremented `seq` succ x : incremented

pop :: Int -> [a] -> Maybe (a, [a])
pop _ [] = Nothing
pop 0 (x:xs) = Just (x,xs)
pop n (x:xs) = _Just . _2 %~ (x:) $ pop (n-1) xs

$(deriveJSON Data.Aeson.TH.defaultOptions{fieldLabelModifier = (camelTo2 '_' . drop (length ("manifestIcon" :: String)))} ''ManifestIcon)
$(deriveJSON Data.Aeson.TH.defaultOptions{fieldLabelModifier = (camelTo2 '_' . drop (length ("manifest" :: String)))} ''Manifest)
