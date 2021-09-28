{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.List (sortOn, intersperse)
import Data.Maybe (catMaybes)

import Options.Applicative as OP

import Servant.API
import Servant.Client

import Control.Lens

import System.Exit
import System.IO
import GHC.IO.Encoding

import Network.HTTP.Client hiding (Proxy(..))

import Data.Proxy (Proxy(..))

import Common

data Command = EnterResults
             | EnterResult (Maybe String)
             | NextRound
             | InitPlayoffs
             | WriteToDisk
             deriving (Eq, Ord, Show)

data Options = Options {
               url :: BaseUrl
             } deriving (Eq, Ord, Show)

parseCommand :: Parser (Options, Command)
parseCommand = (,)
                <$> parseOptions
                <*> subparser
                  (  command "results" (info (pure EnterResults) ( progDesc "Batch enter multiple match results" ))
                  <> command "result" (info resultCommand ( progDesc "Enter a match result" ))
                  <> command "initPlayoffs" (info (pure InitPlayoffs) ( progDesc "Initialize the playoff phase" ))
                  <> command "advance" (info (pure NextRound) ( progDesc "Generate Pairings for the following round"))
                  <> command "write" (info (pure WriteToDisk) ( progDesc "Write current state to disk")))

parseOptions :: Parser Options
parseOptions = pure $ Options $ BaseUrl Http "localhost" 3002 "" --Options <$> option auto (long "host" <> short "h" <> metavar "URL" <> help "Server to connect to" <> value "localhost:3002")

resultCommand :: Parser Command
resultCommand = EnterResult <$> OP.argument (Just <$> auto) (metavar "[QUERY]" <> help "match QUERY to restrict which match to enter results for" <> value Nothing)


run c e = runClientM c e >>= \case
            Left error -> do
                hPutStrLn stderr $ show error
                exitFailure
            Right a -> return a

api :: Proxy ServersideExtras
api = Proxy

getPlayers :: ClientM [PlayerName]
getData :: ClientM PageData
putResult :: MatchID -> [[Int]] -> ClientM (Maybe MatchData)
nextRound :: ClientM (Maybe (RoundID, [MatchData]))
initPlayoffs :: [PlayerID] -> ClientM ()
getMatches :: ClientM [MatchID]
writeToDisk :: ClientM ()
(getPlayers :<|> getData :<|> putResult :<|> nextRound :<|> initPlayoffs :<|> getMatches :<|> writeToDisk) = client api

main :: IO ()
main = do
    setLocaleEncoding utf8
    (o,c) <- execParser $ info parseCommand ( progDesc "Update Tournament Standings" )
    m <- newManager defaultManagerSettings
    let e = mkClientEnv m (url o)
    case c of
        WriteToDisk -> run writeToDisk e
        NextRound -> run nextRound e >> return ()
        EnterResult Nothing -> do
            p <- run getData e
            ms <- run getMatches e
            m <- queryMatch p ms
            r <- queryResult m p
            run (putResult m r) e
            return ()
        EnterResult (Just query) -> do
            p <- run getData e
            putStrLn "Not Yet Implemented"
        EnterResults -> putStrLn "Not Yet Implemented"
        InitPlayoffs -> do
            p <- run getData e
            players <- queryPlayers 4 p
            run (initPlayoffs players) e

queryMatch :: PageData -> [MatchID] -> IO MatchID
queryMatch p ms =
    choice $ catMaybes $ fmap (\matchID -> p ^? onMatchID matchID . to (\m -> (showMatch m p, matchID))) ms

queryPlayers :: Int -> PageData -> IO [PlayerID]
queryPlayers n p = choose' n (imap (\i a -> (a,i)) $ p ^.. participants . folded . participantName) [] where
        choose' :: (Eq a) => Int -> [(String, a)] -> [a] -> IO [a]
        choose' 0 _ r = return $ reverse r
        choose' n options results = do
            c <- choice options
            choose' (n-1) (filter ((/= c) . snd) options) (c:results)

isSwissDone :: PageData -> Bool
isSwissDone pData = has (playoffParticipants . _Just) pData

showMatch :: MatchData -> PageData -> String
showMatch m p = concat $ intersperse " vs " $ m ^.. players . traverse . to (\pl -> p ^?! participants . ix pl . participantName)

queryResult :: MatchID -> PageData -> IO [[Int]]
queryResult m p = do
    let md = p ^?! onMatchID m
        prevResult = md ^. results
        pls = md ^. players
        sl = md ^. seriesLength
    (prevResult <>) <$> choice (showPossibleResults pls p)

choice :: [(String, a)] -> IO a
choice options = do
    itraverse (\i (s, _) -> putStrLn (show i <> ") " <> s)) options
    putStr "> "
    r <- getLine
    return $ snd $ options !! read r

showPossibleResults :: [PlayerID] -> PageData -> [(String, [[Int]])]
showPossibleResults ps p = fmap (\r -> (showResult r p, pointValues ps r : [])) $ sortOn (maximum . fmap length) $ possibleResults ps

showResult :: [[PlayerID]] -> PageData -> String
showResult ps p = concat $ intersperse " > " $ concatMap (intersperse " = " . fmap (\player -> p ^?! participants . ix player . participantName)) ps

pointValues :: [PlayerID] -> [[PlayerID]] -> [Int]
pointValues [x,y] [[a],[b]] = [if x == a then 3 else 0, if y == a then 3 else 0]
pointValues [x,y] [[a,b]] = [1, 1]
pointValues [x,y,z] [[a],[b],[c]] = let f n = if n == a then 3 else if n == b then 1 else 0 in [f x, f y, f z]
pointValues [x,y,z] [[a,b,c]] = [1,1,1]
pointValues [x,y,z] [[a,b],[c]] = let f n = if n == c then 0 else 1 in [f x, f y, f z]
pointValues [x,y,z] [[a],[b,c]] = let f n = if n == a then 3 else 0 in [f x, f y, f z]

possibleResults :: [PlayerID] -> [[[PlayerID]]]
possibleResults [] = [[]]
possibleResults (x:xs) = do
    r <- possibleResults xs
    insertAnywhere x r

insertAnywhere :: a -> [[a]] -> [[[a]]]
insertAnywhere x [] = return [[x]]
insertAnywhere x (xs:xss) = [0,1,2] >>= \case
    0 -> return $ [x] : xs : xss
    1 -> return $ (x:xs):xss
    2 -> (xs :) <$> insertAnywhere x xss
