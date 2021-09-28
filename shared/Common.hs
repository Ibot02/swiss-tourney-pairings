{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ApplicativeDo #-}
module Common where

import Data.Proxy

import Data.Map (Map())
import qualified Data.Map as Map
import Data.List (sort, sortBy, sortOn, findIndex, groupBy, intersperse, transpose)
import Data.Maybe (fromMaybe, catMaybes)
import Text.Read (readMaybe)
import Data.Char (toLower)

import Control.Monad
import Control.Applicative ((<|>), ZipList(..))
import Control.Lens as L
import Control.Lens.TH

import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A

import Miso
import Miso.String (MisoString(..), toMisoString, fromMisoString)
import Servant.API
import Servant.Links

import qualified Data.Text as T

import qualified Data.Attoparsec.Text as Ap

--import qualified Static.Icons as Ic


data PageData = PageData {
      _participants :: [ParticipantData]
    , _numRounds :: Int
    , _rounds :: [[MatchData]]
    , _currentRound :: RoundID
    , _playoffParticipants :: Maybe [PlayerID]
    , _playoffData :: PlayoffData
    , _rngSeed :: Int
    } deriving (Eq, Ord, Show)

data PlayoffData = PlayoffData {
      _playoffMatches :: Map PlayoffMatchID (Either (Int, [Either (PlayoffMatchID, Bool) PlayerID]) MatchData)
    } deriving (Eq, Ord, Show)

type PlayerName = String

data ParticipantData = ParticipantData {
      _participantName :: PlayerName
    , _roundSkipped :: RoundID
    , _roundDropped :: Maybe RoundID
    } deriving (Eq, Ord, Show)

type RoundID = Int
type PlayerID = Int
data MatchID = SwissMatch RoundID Int
             | PlayoffMatch PlayoffMatchID
             deriving (Eq, Ord, Show)

data PlayoffMatchID = Finals
                    | Semifinals Int
                    | MatchForThird
                    deriving (Eq, Ord, Show)

data MatchData = MatchData {
          _players :: [PlayerID]
        , _seriesLength :: Int
        , _results :: [[Int]]
        } deriving (Eq, Ord, Show)

type ClientRoutes = MainPage
                    :<|> PlayoffsPage
                    :<|> RoundPage
                    :<|> StandingsPage
                    :<|> PlayerPage
                    :<|> PlayersPage

type ServersideExtras = GetPlayers
                      -- :<|> PutPlayerDrop
                      :<|> GetData
                      :<|> PutResult
                      :<|> NextRound
                      :<|> InitPlayoffs
                      :<|> OpenMatches
                      :<|> WriteToDisk

type MainPage = View Action
type PlayoffsPage = "round" :> "top4" :> View Action
type RoundPage = "round" :> Capture "round" Int :> View Action
type StandingsPage = "standings" :> Capture "round" Int :> View Action
type PlayerPage = "player" :> Capture "player" PlayerName :> View Action
type PlayersPage = "players" :> "edit" :> View Action

type GetPlayers = "data" :> "players" :> Get '[JSON] [PlayerName]
-- type PutPlayerDrop = "data" :> "players" :> "drop" :> Capture "playerID" PlayerID :> PutNoContent '[JSON] ()
type GetData = "data" :> Get '[JSON] PageData
type PutResult = "data" :> "results" :> Capture "match" MatchID :> ReqBody '[JSON] [[Int]] :> Put '[JSON] (Maybe MatchData)
type NextRound = "data" :> "results" :> "nextRound" :> Post '[JSON] (Maybe (RoundID, [MatchData]))
type InitPlayoffs = "data" :> "results" :> "nextRound" :> "playoffs" :> ReqBody '[JSON] [PlayerID] :> Post '[JSON] ()
type OpenMatches = "data" :> "matches" :> Get '[JSON] [MatchID]
type WriteToDisk = "data" :> "write" :> Put '[JSON] ()

data Action = NoOp
            | ChangeURI URI
            | HandleURI URI
            -- | DropPlayer PlayerID
            | ChangeResult MatchID [[Int]]
            | HandleResult MatchID MatchData
            | ComputeNextPairings
            | HandleNextPairings RoundID [MatchData]
            deriving (Eq, Ord, Show)

data Page = Page {
              _pageData :: PageData
            , _pageInteractive :: Bool
            , _pageNav :: PageNav
            }
            deriving (Eq, Ord, Show)

data PageNav = PageRound Int
             | PagePlayoffs
             | PageStandings (Maybe Int)
             | PagePlayer PlayerName
             | PagePlayers
            deriving (Eq, Ord, Show)

$(makeLenses ''Page)
$(makeLenses ''PageData)
$(makeLenses ''ParticipantData)
$(makeLenses ''PlayoffData)
$(makeLenses ''MatchData)
$(makePrisms ''MatchID)
$(makePrisms ''PlayoffMatchID)
$(makePrisms ''PageNav)
$(makePrisms ''Action)
$(A.deriveJSON A.defaultOptions{ A.sumEncoding = A.UntaggedValue, A.constructorTagModifier = map toLower } ''PlayoffMatchID)
$(A.deriveJSON A.defaultOptions{ A.sumEncoding = A.UntaggedValue, A.constructorTagModifier = map toLower } ''MatchID)
instance A.ToJSON PlayoffData where
    toEncoding (PlayoffData matches) = A.toEncoding $ Map.toList matches
    toJSON (PlayoffData matches) = A.toJSON $ Map.toList matches
instance A.FromJSON PlayoffData where
    parseJSON = fmap (PlayoffData . Map.fromList) . A.parseJSON
$(A.deriveJSON A.defaultOptions{ A.fieldLabelModifier = A.camelTo2 '_' . drop 1 } ''MatchData)
$(A.deriveJSON A.defaultOptions{ A.fieldLabelModifier = A.camelTo2 '_' . drop 1 } ''ParticipantData)
$(A.deriveJSON A.defaultOptions{ A.fieldLabelModifier = A.camelTo2 '_' . drop 1 } ''PageData)

instance FromHttpApiData MatchID where
    parseUrlPiece = either (Left . T.pack) Right . Ap.parseOnly parser where
        parser = parseSwissMatchID <|> (PlayoffMatch <$> parsePlayoffMatchID)
        parseSwissMatchID = do
            Ap.char 'R'
            roundNumber <- Ap.decimal
            Ap.char 'M'
            matchIndex <- Ap.decimal
            return $ SwissMatch roundNumber matchIndex
        parsePlayoffMatchID = (Finals <$ Ap.string "finals") <|> (Semifinals <$> (Ap.string "semifinals" >> Ap.decimal)) <|> (MatchForThird <$ (Ap.string "matchForThird"))

instance ToHttpApiData MatchID where
    toUrlPiece (SwissMatch r i) = T.pack $ "R" <> show r <> "M" <> show i
    toUrlPiece (PlayoffMatch Finals) = "finals"
    toUrlPiece (PlayoffMatch (Semifinals i)) = T.pack $ "semifinals" <> show i
    toUrlPiece (PlayoffMatch MatchForThird) = "matchForThird"

onMatchID (SwissMatch round index) = rounds . ix round . ix index
onMatchID (PlayoffMatch index) = playoffData . playoffMatches . ix index . _Right

-- clientPages [] _ = (PagePlayerInput $ PagePlayerInputData "" Nothing)
--                    :<|> (\r -> PagePlayerInput $ PagePlayerInputData "" $ Just $ PageRound r)
--                    :<|> (\r -> PagePlayerInput $ PagePlayerInputData "" $ Just $ PageStandings r)
--                    :<|> (\p -> PagePlayerInput $ PagePlayerInputData "" $ Just $ PagePlayer p)
--                    :<|> (PagePlayerInput $ PagePlayerInputData "" Nothing)
clientPages = ("./", PageStandings Nothing)
            :<|> ("../../", PagePlayoffs)
            :<|> (\r -> ("../../", PageRound r))
            :<|> (\r -> ("../../", PageStandings (Just r)))
            :<|> (\p -> ("../../", PagePlayer p))
            :<|> ("../../", PagePlayers)

instance HasLink Page where
    type MkLink Page a = a
    toLink toA _ = toA

-- linkMainPage :<|> linkRoundPage :<|> linkStandingsPage :<|> linkPlayerPage :<|> linkPlayersPage = allLinks (Proxy @ ClientRoutes)
-- goMainPage = linkURI linkMainPage
(linkURI -> goMainPage) :<|> (linkURI -> goPlayoffsPage) :<|> (fmap linkURI -> goRoundPage) :<|> (fmap linkURI -> goStandingsPage) :<|> (fmap linkURI -> goPlayerPage) :<|> (linkURI -> goPlayersPage) = allLinks (Proxy @ ClientRoutes)
-- goMainPage :: [PlayerName] -> [RoundData] -> Bool -> URI
-- goMainPage players results interactive = let (r :<|> _ :<|> _ :<|> _ :<|> _) = allLinks (Proxy @ ClientRoutes) players results (not interactive) in linkURI' LinkArrayElementPlain r
-- goRoundPage :: [PlayerName] -> [RoundData] -> Bool -> Int -> URI
-- goRoundPage players results interactive round = let (_ :<|> r :<|> _ :<|> _ :<|> _) = allLinks (Proxy @ ClientRoutes) players results (not interactive) in linkURI' LinkArrayElementPlain $ r round
-- goStandingsPage :: [PlayerName] -> [RoundData] -> Bool -> Maybe Int -> URI
-- goStandingsPage players results interactive round = let (_ :<|> _ :<|> r :<|> _ :<|> _) = allLinks (Proxy @ ClientRoutes) players results (not interactive) in linkURI' LinkArrayElementPlain $ r round
-- goPlayerPage :: [PlayerName] -> [RoundData] -> Bool -> PlayerName -> URI
-- goPlayerPage players results interactive player = let (_ :<|> _ :<|> _ :<|> r :<|> _) = allLinks (Proxy @ ClientRoutes) players results (not interactive) in linkURI' LinkArrayElementPlain $ r player
-- goPlayersPage :: [PlayerName] -> [RoundData] -> Bool -> URI
-- goPlayersPage players results interactive = let (_ :<|> _ :<|> _ :<|> _ :<|> r) = allLinks (Proxy @ ClientRoutes) players results (not interactive) in linkURI' LinkArrayElementPlain r

goPage :: PageNav -> URI
goPage (PageRound r) = goRoundPage r
goPage PagePlayoffs = goPlayoffsPage
goPage (PageStandings Nothing) = goMainPage
goPage (PageStandings (Just r)) = goStandingsPage r
goPage (PagePlayer p) = goPlayerPage p
goPage PagePlayers = goPlayersPage

hrefPage :: RootLink -> PageNav -> [Attribute Action]
hrefPage root p = [hrefURI root (goPage p), onWithOptions (Options True False) "click" emptyDecoder (\() -> ChangeURI $ goPage p)]

type RootLink = MisoString
hrefURI :: RootLink -> URI -> Attribute action
hrefURI root = href_ . (root <>) . toMisoString . show

viewPage :: RootLink -> Page -> View Action
viewPage root page@(Page pData interactive nav) = div_ [] ([
                  header
                ] <> content
                ) where
                    players = pData ^.. participants . traverse . participantName
                    havePlayoffs = has (playoffParticipants . _Just) pData
                    header = header_ [] [
                          nav_ [] [
                              ul_ []
                                [ li_ ((if has _PageStandings nav then (class_ "current" :) else id ) []) [
                                      a_ (hrefPage root $ PageStandings Nothing) [text "Standings"]
                                    ]
                                , li_ ((if has _PageRound nav || has _PagePlayoffs nav then (class_ "current" :) else id ) []) [
                                      a_ (hrefPage root $ (if havePlayoffs then PagePlayoffs else PageRound 0)) [text "Rounds"]
                                    , ul_ [] (flip fmap [1..(pData ^. numRounds)] (\r ->
                                        li_ ((if (\r' -> r' == Just r || (r' == Just 0 && r == (pData ^. currentRound))) (nav ^? _PageRound) then (class_ "current" :) else id) []) [
                                              a_ (hrefPage root $ PageRound r) [text $ toMisoString $ "Round " <> show r]
                                        ]) <> if havePlayoffs then [
                                            li_ (if has _PagePlayoffs nav then [class_ "current"] else []) [
                                                a_ (hrefPage root $ PagePlayoffs) [text $ "Top 4"]
                                            ]
                                        ] else []
                                        )
                                    ]
                                , li_ ((if has _PagePlayer nav || has _PagePlayers nav then (class_ "current" :) else id ) []) [
                                      a_ (if interactive then (hrefPage root $ PagePlayers) else []) [text "Players"]
                                    , ul_ [] $ flip fmap players $ \p ->
                                        li_ ((if (nav ^? _PagePlayer) == Just p then (class_ "current" :) else id) []) [
                                              a_ (hrefPage root $ PagePlayer p) [text $ toMisoString p]
                                            ]
                                    ]
                                ]
                            ]
                        ]
                    icon file cl = img_ [src_ (root <> "static/" <> file), class_ cl]
                    content = (if interactive then (nodeHtml "data" [id_ "page-data", data_ "page-data" (toMisoString $ A.encode pData)] [] :) else id) $ (case nav of
                        PageStandings r -> (if havePlayoffs then [playoffs] else []) <> [standings r]
                        PageRound r -> round r
                        PagePlayoffs -> [playoffs]
                        PagePlayer p -> [playerPage p]
                        PagePlayers -> [playersPage]
                        )
                    playoffs = div_ [class_ "top4"] $ pData ^.. playoffData . playoffMatches . to Map.toList . traverse . to playoffMatch
                    playoffMatch (ident, match) = let 
                        (className, matchHeading) = resolveMatchID ident
                        resolveMatchID :: PlayoffMatchID -> (String, String)
                        resolveMatchID ident = case ident of
                            Semifinals 0 -> ("semifinals-one", "Semifinal One")
                            Semifinals 1 -> ("semifinals-two", "Semifinal Two")
                            Finals -> ("finals", "Final")
                            MatchForThird -> ("match-for-third", "Match for 3rd")
                        (matchLength, leftName, leftScore, rightName, rightScore) = case match of
                            Left (matchLength, [leftParticipant, rightParticipant]) -> (matchLength, playerDisplayName leftParticipant, 0, playerDisplayName rightParticipant, 0)
                            Right (MatchData [leftParticipant, rightParticipant] matchLength results) -> let
                                (leftScore, rightScore) = foldr (\[a,b] (x,y) -> if a > b then (x+1, y) else if a < b then (x, y+1) else (x,y)) (0,0) results
                                in (matchLength, playerDisplayName $ Right leftParticipant, leftScore, playerDisplayName $ Right rightParticipant, rightScore)
                        playerDisplayName (Right player) = fromMaybe "Failed to resolve participant ID" $ pData ^? participants . ix player . participantName
                        playerDisplayName (Left (matchId, win)) = (if win then "Winner" else "Loser") <> " of " <> snd (resolveMatchID matchId)
                        leftParticipant = participant leftName leftScore
                        rightParticipant = participant rightName rightScore
                        participant name score = div_ [class_ "player"] [
                                              span_ [class_ "name"] [text $ toMisoString name]
                                            , span_ [class_ "result"] $ result matchLength score
                                            ]
                        result (((+) 1) . (`div` 2) -> n) m = replicate m winImg <> replicate (n - m) noWinImg
                        winImg = icon "seriesWin.png" "series-win"
                        noWinImg = icon "seriesNoWin.png" "series-no-win"
                        in div_ [class_ (toMisoString className)] [
                              span_ [class_ "match-heading"] [text $ toMisoString $ matchHeading]
                            , div_ [class_ "match"] [
                                  leftParticipant
                                , divider
                                , rightParticipant
                                ]
                            ]
                    standings Nothing = standings $ Just $ pData ^. currentRound
                    standings (Just r) = table_ [] $ header : content where
                        header = tr_ [] ([
                                  th_ [] [text "#"]
                                , th_ [] [text "Player"]
                                , th_ [] [text "Points"]
                                , th_ [] [text "Opp. Avg."]
                                ] <> fmap (\r' -> th_ [] [text $ toMisoString $ "R" <> show r']) [1..r])
                        content = withFoldable (sort $ listStandings pData r) $ \(pos, name, points, oppAvg, dropped, perRound) ->
                            tr_ (if dropped then [class_ "dropped"] else []) ([
                                  td_ [] [text $ toMisoString $ show pos]
                                , td_ [] [playerName name]
                                , td_ [] [text $ toMisoString $ show points]
                                , td_ [] [text $ toMisoString $ take 5 $ show oppAvg]
                                ] <> withFoldable (take r $ fmap show perRound <> repeat "-") (\r ->
                                    td_ [] [text $ toMisoString r]
                                ))
                    round 0 = round $ pData ^. currentRound
                    round r = let
                        matches = pData ^? rounds . ix (r - 1)
                        matchesDisplay = case matches of
                            Just matches -> fmap match matches
                            Nothing -> [span_ [class_ "note"] [text "Pairings for this round have yet to be determined"]] <> if interactive then [button_ [onClick ComputeNextPairings] [text "Generate Pairings!"]] else []
                        in [div_ [class_ ("round" <> (if r == pData ^. currentRound then " current-round" else " past-round"))] matchesDisplay]
                    match (MatchData players bo results) = let
                        playerNames = fmap (\i -> pData ^? participants . ix i . participantName) players
                        playerResults = if not $ null results then transpose results else replicate (length players) []
                        playerBoxes = flip fmap (zip playerNames playerResults) $ \(name, results) ->
                            div_ [class_ "player"] $ [
                                  maybe (span_ [] ["Failed to resolve player ID"]) (\name -> span_ [class_ "name"] [playerName name]) name
                                , displayResults bo results
                                ]
                        displayResults 1 [] = span_ [class_ "result tbd"] []
                        displayResults 1 [0] = span_ [class_ "result loss"] [text "+0"]
                        displayResults 1 [1] = span_ [class_ "result tie"] [text "+1"]
                        displayResults 1 [3] = span_ [class_ "result win"] [text "+3"]
                        displayResults _ _ = text "TODO"
                        in div_ [class_ "match match3"] $ intersperse divider playerBoxes
                    divider = div_ [class_ "divider"] [
                              span_ [] [text "vs"]
                            , span_ [] []
                            ]
                    playerName player = a_ (hrefPage root (PagePlayer player)) [text $ toMisoString player]
                    playerPage player = case findIndex (==player) players of
                        Nothing -> div_ [] [text "Not found: That player does not seem to exist"]
                        Just i -> div_ [class_ "player-matches"] $ fmap match $ pData ^.. rounds . traverse . traverse . filtered (\(MatchData players _ _) -> i `elem` players)
                    playersPage = text "TODO"

dropdown :: (Eq a, Show a, Read a) => [Attribute Action] -> [(MisoString, a)] -> a -> (a -> Action) -> View Action
dropdown attrs options selected action = select_ ((onChange (\s -> fromMaybe NoOp $ action <$> readMaybe (fromMisoString s))) : attrs) $ withFoldable options $ \(t,v) ->
                    option_ [selected_ (v == selected), value_ (toMisoString $ show v)] [text t]

listStandings :: PageData -> RoundID -> [(Int, PlayerName, Int, Float, Bool, [Int])]
listStandings pData round = let
    numPlayers = length $ pData ^. participants
    playerMatches = flip fmap [0..numPlayers - 1] $ \i ->
        (,) i $ flip fmap [0..round - 1] $ \r -> pData ^? rounds . ix r . traverse .  filtered (elemOf (players . traverse) i)
    playerPointsByMatch = flip fmap playerMatches $ \(i, matches) -> flip fmap (giveSkippedPoints i matches) $ \case
        Left n -> n
        Right (MatchData players _ results) ->
            sum $ flip fmap results $ \result -> sumOf (traverse . filtered ((==i) . fst) . _2) $ zip players result
    giveSkippedPoints i matches = flip fmap (zip [(1::RoundID)..] matches) $ \case
        (r, Nothing) -> if (pData ^? participants . ix i . roundSkipped) == Just r then Left 2 else Left 0
        (_, Just m) -> Right m
    playerPoints = fmap sum playerPointsByMatch
    playerOppPoints = flip fmap playerMatches $ \(i, catMaybes -> matches) -> case length matches of
        0 -> 0
        n -> sumOf (traverse . players . to (filter (/= i)) . to (\players -> sumOf (traverse . to (playerPoints !!) . to fromIntegral) players / (fromIntegral $ length players))) matches / fromIntegral n
    playerDropped = pData ^.. participants . traverse . roundDropped . to (\case
        Nothing -> False
        Just r' -> r' <= round)
    ranks = fmap fst $ sortOn snd $ concatMap (\(i, p) -> fmap ((,) i) p) $ (\ranks -> zip (scanl (\r p -> r + length p) 1 ranks) ranks) $ reverse $ fmap (fmap snd) $ groupBy (\x y -> fst x == fst y) $ sort $ zip (zip playerPoints playerOppPoints) [0..numPlayers - 1]
    in getZipList $ do
        r <- ZipList ranks
        name <- ZipList $ pData ^.. participants . traverse . participantName
        points <- ZipList playerPoints
        oppPoints <- ZipList playerOppPoints
        dropped <- ZipList playerDropped
        pointsPerMatch <- ZipList playerPointsByMatch
        pure (r, name, points, oppPoints, dropped, pointsPerMatch)

switchPage :: (RootLink, PageNav) -> (RootLink, Page) -> (RootLink, Page)
switchPage (l, n) (_, (Page d i _)) = (l, Page d i n)

getPageNav :: URI -> Maybe (RootLink, PageNav)
getPageNav u = case route (Proxy @ ClientRoutes) clientPages u of
            Right page -> Just page
            Left _ -> Nothing

the404 :: View Action
the404 = text "404 Page Not Found"
