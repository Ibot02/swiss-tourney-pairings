{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.List (sortOn, intersperse)

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
writeToDisk :: ClientM ()
(getPlayers :<|> getData :<|> putResult :<|> nextRound :<|> writeToDisk) = client api

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
            m <- queryMatch p
            r <- queryResult m p
            run (putResult m r) e
            return ()
        EnterResult (Just query) -> do
            p <- run getData e
            putStrLn "Not Yet Implemented"
        EnterResults -> putStrLn "Not Yet Implemented"

queryMatch :: PageData -> IO MatchID
queryMatch p = do
    case isSwissDone p of
        True -> putStrLn "Not Yet Implemented" >> return undefined
        False -> choice $ fmap (\(i,m) -> (showMatch m p, SwissMatch ((p ^. currentRound) - 1) i)) $ filter (\(_, m) -> (m ^. results) == []) $ imap (,) $ p ^.. rounds . ix ((p ^. currentRound) - 1) . traverse

isSwissDone :: PageData -> Bool
isSwissDone _ = False

showMatch :: MatchData -> PageData -> String
showMatch m p = concat $ intersperse " vs " $ m ^.. players . traverse . to (\pl -> p ^?! participants . ix pl . participantName)

queryResult :: MatchID -> PageData -> IO [[Int]]
queryResult m p = do
    let md = p ^?! onMatchID m
        pls = md ^. players
        sl = md ^. seriesLength
    choice (showPossibleResults pls sl p)

choice :: [(String, a)] -> IO a
choice options = do
    itraverse (\i (s, _) -> putStrLn (show i <> ") " <> s)) options
    putStr "> "
    r <- getLine
    return $ snd $ options !! read r

showPossibleResults :: [PlayerID] -> Int -> PageData -> [(String, [[Int]])]
showPossibleResults ps 1 p = fmap (\r -> (showResult r p, pointValues ps r : [])) $ sortOn (maximum . fmap length) $ possibleResults ps

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
