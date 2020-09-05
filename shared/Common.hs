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
module Common where

import Data.Proxy

import Data.Map (Map())
import qualified Data.Map as Map
import Data.List (sort, sortBy, sortOn)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)


import Control.Monad
import Control.Applicative ((<|>))
import Control.Lens as L
import Control.Lens.TH

import Control.Monad.State

import Miso
import Miso.String (MisoString(..), toMisoString, fromMisoString)
import Servant.API
import Servant.Links

import qualified Data.Text as T

--import qualified Static.Icons as Ic

type PlayerName = String
type Drop = Bool
data Result = ThreePoints | OnePoint | NoPoints
            deriving (Eq, Ord, Show, Read)

data RoundData = RoundData {
                _roundData :: [Maybe (Result, Drop)] --in the order of the player data, without missing any
                } deriving (Eq, Ord, Show)

instance ToHttpApiData RoundData where
    toUrlPiece (RoundData d) = T.pack $ fmap encodeEntry d where
                encodeEntry Nothing = '-'
                encodeEntry (Just (ThreePoints, False)) = '3'
                encodeEntry (Just (OnePoint, False)) = '1'
                encodeEntry (Just (NoPoints, False)) = '0'
                encodeEntry (Just (ThreePoints, True)) = 'W'
                encodeEntry (Just (OnePoint, True)) = 'T'
                encodeEntry (Just (NoPoints, True)) = 'L'

instance FromHttpApiData RoundData where
    parseUrlPiece string = fmap RoundData $ forM (T.unpack string) $ \case
                        '-' -> Right $ Nothing
                        '3' -> Right $ Just (ThreePoints, False)
                        '1' -> Right $ Just (OnePoint, False)
                        '0' -> Right $ Just (NoPoints, False)
                        'W' -> Right $ Just (ThreePoints, True)
                        'T' -> Right $ Just (OnePoint, True)
                        'L' -> Right $ Just (NoPoints, True)
                        _   -> Left "Not a valid RoundData entry"

type ClientRoutes = QueryParams "players" PlayerName :> 
                    QueryParams "results" RoundData :> 
                    QueryFlag "no-interact" :>
                    (MainPage
                     :<|> RoundPage
                     :<|> StandingsPage
                     :<|> PlayerPage
                     :<|> PlayersPage
                    )

type MainPage = View Action
type RoundPage = "round" :> Capture "round" Int :> View Action
type StandingsPage = "standings" :> QueryParam "round" Int :> View Action
type PlayerPage = "player" :> Capture "player" PlayerName :> View Action
type PlayersPage = "players" :> "edit" :> View Action

data Action = NoOp
            | ChangeURI URI
            | HandleURI URI
            | PlayerEntryAction PlayerEntryAction
            | ChangeResult Int [(Int, Maybe Result)]
            deriving (Eq, Ord, Show)

data PlayerEntryAction = ChangePlayerInputField String | ApplyPlayerInput
            deriving (Eq, Ord, Show)

data Page = Page {
              _pagePlayers :: [PlayerName]
            , _pageResults :: [RoundData]
            , _pageInteractive :: Bool
            , _pageNav :: PageNav
            }
            deriving (Eq, Ord, Show)

data PageNav = PageRound Int
             | PageStandings (Maybe Int)
             | PagePlayer PlayerName
             | PagePlayerInput String (Maybe PageNav)
            deriving (Eq, Ord, Show)

$(makeLenses ''Page)
$(makePrisms ''PageNav)
$(makeLenses ''RoundData)
$(makePrisms ''Action)
$(makePrisms ''PlayerEntryAction)

-- clientPages [] _ = (PagePlayerInput $ PagePlayerInputData "" Nothing)
--                    :<|> (\r -> PagePlayerInput $ PagePlayerInputData "" $ Just $ PageRound r)
--                    :<|> (\r -> PagePlayerInput $ PagePlayerInputData "" $ Just $ PageStandings r)
--                    :<|> (\p -> PagePlayerInput $ PagePlayerInputData "" $ Just $ PagePlayer p)
--                    :<|> (PagePlayerInput $ PagePlayerInputData "" Nothing)
clientPages players results (not -> interactive) = ("./", Page players results interactive $ PageStandings Nothing)
                                                 :<|> (\r -> ("../../", Page players results interactive $ PageRound r))
                                                 :<|> (\r -> ("../", Page players results interactive $ PageStandings r))
                                                 :<|> (\p -> ("../../", Page players results interactive $ PagePlayer p))
                                                 :<|> ("../../", Page players results interactive $ PagePlayerInput (unlines players) Nothing)

instance HasLink Page where
    type MkLink Page a = a
    toLink toA _ = toA

goMainPage :: [PlayerName] -> [RoundData] -> Bool -> URI
goMainPage players results interactive = let (r :<|> _ :<|> _ :<|> _ :<|> _) = allLinks (Proxy @ ClientRoutes) players results (not interactive) in linkURI' LinkArrayElementPlain r
goRoundPage :: [PlayerName] -> [RoundData] -> Bool -> Int -> URI
goRoundPage players results interactive round = let (_ :<|> r :<|> _ :<|> _ :<|> _) = allLinks (Proxy @ ClientRoutes) players results (not interactive) in linkURI' LinkArrayElementPlain $ r round
goStandingsPage :: [PlayerName] -> [RoundData] -> Bool -> Maybe Int -> URI
goStandingsPage players results interactive round = let (_ :<|> _ :<|> r :<|> _ :<|> _) = allLinks (Proxy @ ClientRoutes) players results (not interactive) in linkURI' LinkArrayElementPlain $ r round
goPlayerPage :: [PlayerName] -> [RoundData] -> Bool -> PlayerName -> URI
goPlayerPage players results interactive player = let (_ :<|> _ :<|> _ :<|> r :<|> _) = allLinks (Proxy @ ClientRoutes) players results (not interactive) in linkURI' LinkArrayElementPlain $ r player
goPlayersPage :: [PlayerName] -> [RoundData] -> Bool -> URI
goPlayersPage players results interactive = let (_ :<|> _ :<|> _ :<|> _ :<|> r) = allLinks (Proxy @ ClientRoutes) players results (not interactive) in linkURI' LinkArrayElementPlain r

goPage :: Page -> URI
goPage (Page players results interactive nav) = case nav of
        PageRound r -> goRoundPage players results interactive r
        PageStandings Nothing -> goMainPage players results interactive
        PageStandings r -> goStandingsPage players results interactive r
        PagePlayer p -> goPlayerPage players results interactive p
        PagePlayerInput _ _ -> goPlayersPage players results interactive

hrefPage :: RootLink -> Page -> [Attribute Action]
hrefPage root p = [hrefURI root (goPage p), onWithOptions (Options True False) "click" emptyDecoder (\() -> ChangeURI $ goPage p)]

type RootLink = MisoString
hrefURI :: RootLink -> URI -> Attribute action
hrefURI root = href_ . (root <>) . toMisoString . show


viewPage :: RootLink -> Page -> View Action
viewPage root page@(Page players results interactive nav) = div_ [] ([
                  header
                ] <> content
                ) where
                    header = header_ [] [
                          nav_ [] [
                              ul_ []
                                [ li_ ((if has _PageStandings nav then (class_ "current" :) else id ) []) [
                                      a_ (hrefPage root $ Page players results interactive (PageStandings Nothing)) [text "Standings"]
                                    ]
                                , li_ ((if has _PageRound nav then (class_ "current" :) else id ) []) [
                                      a_ (hrefPage root $ Page players results interactive $ PageRound 0) [text "Rounds"]
                                    , ul_ [] $ flip fmap [1..length results] $ \r ->
                                        li_ ((if (\r' -> r' == Just r || (r' == Just 0 && r == length results)) (nav ^? _PageRound) then (class_ "current" :) else id) []) [
                                              a_ (hrefPage root $ Page players results interactive $ PageRound r) [text $ toMisoString $ "Round " <> show r]
                                            ]
                                    ]
                                -- , li_ ((if has _PagePlayer nav || has _PagePlayerInput nav then (class_ "current" :) else id ) []) [
                                --       a_ (hrefPage root $ Page players results interactive $ PagePlayerInput (unlines players) (Just nav)) [text "Players"]
                                --     , ul_ [] $ flip fmap players $ \p ->
                                --         li_ ((if (nav ^? _PagePlayer) == Just p then (class_ "current" :) else id) []) [
                                --               a_ (hrefPage root $ Page players results interactive $ PagePlayer p) [text $ toMisoString p]
                                --             ]
                                --     ]
                                ]
                            ]
                        ]
                    content = case nav of
                        PageStandings r -> [standings r]
                        PageRound r -> round r
                        _ -> [span_ [] [text "TODO"]]
                    standings Nothing = standings (Just $ length results)
                    standings (Just r) = table_ [] $ (
                        tr_ [] ([
                              th_ [] [text "#"]
                            , th_ [] [text "Player"]
                            , th_ [] [text "Points"]
                            , th_ [] [text "Opp. Avg."]
                            ] <> fmap (\r' -> th_ [] [text $ toMisoString $ "R" <> show r']) [1..r]) :
                        ) $ withFoldable (listStandings players (take r results)) $ \(pos, name, points, oppAvg, dropped, perRound) ->
                            tr_ (if dropped then [class_ "dropped"] else []) ([
                                  td_ [] [text $ toMisoString $ show pos]
                                , td_ [] [text $ toMisoString name]
                                , td_ [] [text $ toMisoString $ show points]
                                , td_ [] [text $ toMisoString $ take 5 $ show oppAvg]
                                ] <> withFoldable (take r $ fmap show perRound <> repeat "-") (\r ->
                                    td_ [] [text $ toMisoString r]
                                ))
                    nextRoundPromt = span_ [class_ "next-round-prompt"] [
                                      text "All results were entered."
                                    , br_ []
                                    , a_ (hrefPage root $ Page players (results <> [RoundData (replicate (length players) Nothing)]) interactive $ PageRound 0) [
                                          text "Compute next round's pairings."
                                        ]
                                    ]
                    round 0 = (if interactive && hasn't (traverse . roundData . traverse . _Nothing) results then (nextRoundPromt:) else id) $ round (length results)
                    round r = let
                        (RoundData currentRound) = results !! (r-1)
                        (threeWayMatch, pairs) = makePairings $ fromRoundData (length players) (take (r-1) results)
                        in [div_ [class_ ("round" <> (if r == length results then " current-round" else " past-round"))] ((withFoldable pairs $ \(a,b) ->
                                match a b r currentRound
                            ) <> (withFoldable threeWayMatch $ \(a,b,c) ->
                                match3 a b c r currentRound
                            ))]
                    match a b r currentRound = let
                        ra = currentRound ^? ix a . _Just . _1
                        rb = currentRound ^? ix b . _Just . _1
                        pa = players !! a
                        pb = players !! b
                        in div_ [class_ "match"] [
                              div_ [class_ "player"] [
                                  span_ [class_ "name"] [text $ toMisoString pa]
                                , case ra of
                                    Nothing -> span_ [class_ "result tbd"] []
                                    Just NoPoints -> span_ [class_ "result loss"] [text "+0"]
                                    Just OnePoint -> span_ [class_ "result tie"] [text "+1"]
                                    Just ThreePoints -> span_ [class_ "result win"] [text "+3"]
                                ]
                            , div_ [class_ "divider"] [
                                      span_ [] [text "vs"]
                                    , span_ [] (if interactive then [dropdown [] [("", Nothing), ("3-0", Just (ThreePoints, NoPoints)), ("0-3", Just (NoPoints, ThreePoints)), ("1-1", Just (OnePoint, OnePoint)), ("0-0", Just (NoPoints, NoPoints))] ((,) <$> ra <*> rb) (\se -> ChangeResult (r - 1) [(a, se ^? _Just . _1), (b, se ^? _Just . _2)])] else [])
                                ]
                            , div_ [class_ "player"] [
                                  span_ [class_ "name"] [text $ toMisoString pb]
                                , case rb of
                                    Nothing -> span_ [class_ "result tbd"] []
                                    Just NoPoints -> span_ [class_ "result loss"] [text "+0"]
                                    Just OnePoint -> span_ [class_ "result tie"] [text "+1"]
                                    Just ThreePoints -> span_ [class_ "result win"] [text "+3"]
                                ]
                            ]
                    match3 a b c r currentRound = let
                        ra = currentRound ^? ix a . _Just . _1
                        rb = currentRound ^? ix b . _Just . _1
                        rc = currentRound ^? ix c . _Just . _1
                        pa = players !! a
                        pb = players !! b
                        pc = players !! c
                        in div_ [class_ "match match3"] [
                              div_ [class_ "player"] [
                                  span_ [class_ "name"] [text $ toMisoString pa]
                                , case ra of
                                    Nothing -> span_ [class_ "result tbd"] []
                                    Just NoPoints -> span_ [class_ "result loss"] [text "+0"]
                                    Just OnePoint -> span_ [class_ "result tie"] [text "+1"]
                                    Just ThreePoints -> span_ [class_ "result win"] [text "+3"]
                                ]
                            , div_ [class_ "divider"] [
                                      span_ [] [text "vs"]
                                    , span_ [] (if interactive then [dropdown [] [
                                          ("", Nothing)
                                        , ("3-1-0", Just (ThreePoints, OnePoint, NoPoints))
                                        , ("3-0-1", Just (ThreePoints, NoPoints, OnePoint))
                                        , ("1-3-0", Just (OnePoint, ThreePoints, NoPoints))
                                        , ("0-3-1", Just (NoPoints, ThreePoints, OnePoint))
                                        , ("1-0-3", Just (OnePoint, NoPoints, ThreePoints))
                                        , ("0-1-3", Just (NoPoints, OnePoint, ThreePoints))
                                        , ("3-0-0", Just (ThreePoints, NoPoints, NoPoints))
                                        , ("0-3-0", Just (NoPoints, ThreePoints, NoPoints))
                                        , ("0-0-3", Just (NoPoints, NoPoints, ThreePoints))
                                        , ("1-1-1", Just (OnePoint, OnePoint, OnePoint))
                                        , ("1-1-0", Just (OnePoint, OnePoint, NoPoints))
                                        , ("1-0-1", Just (OnePoint, NoPoints, OnePoint))
                                        , ("0-1-1", Just (NoPoints, OnePoint, OnePoint))
                                        , ("0-0-0", Just (NoPoints, NoPoints, NoPoints))
                                        ] ((,,) <$> ra <*> rb <*> rc) (\se -> ChangeResult (r - 1) [(a, se ^? _Just . _1), (b, se ^? _Just . _2), (c, se ^? _Just . _3)])] else [])
                                ]
                            , div_ [class_ "player"] [
                                  span_ [class_ "name"] [text $ toMisoString pb]
                                , case rb of
                                    Nothing -> span_ [class_ "result tbd"] []
                                    Just NoPoints -> span_ [class_ "result loss"] [text "+0"]
                                    Just OnePoint -> span_ [class_ "result tie"] [text "+1"]
                                    Just ThreePoints -> span_ [class_ "result win"] [text "+3"]
                                ]
                            , div_ [class_ "divider"] [
                                      span_ [] [text "vs"]
                                    ]
                            , div_ [class_ "player"] [
                                  span_ [class_ "name"] [text $ toMisoString pc]
                                , case rc of
                                    Nothing -> span_ [class_ "result tbd"] []
                                    Just NoPoints -> span_ [class_ "result loss"] [text "+0"]
                                    Just OnePoint -> span_ [class_ "result tie"] [text "+1"]
                                    Just ThreePoints -> span_ [class_ "result win"] [text "+3"]
                                ]
                            ]

dropdown :: (Eq a, Show a, Read a) => [Attribute Action] -> [(MisoString, a)] -> a -> (a -> Action) -> View Action
dropdown attrs options selected action = select_ ((onChange (\s -> fromMaybe NoOp $ action <$> readMaybe (fromMisoString s))) : attrs) $ withFoldable options $ \(t,v) ->
                    option_ [selected_ (v == selected), value_ (toMisoString $ show v)] [text t]

type PlayerIndex = Int
makePairings :: [(Int, Int, [PlayerIndex], Bool, Bool)] -> (Maybe (PlayerIndex, PlayerIndex, PlayerIndex), [(PlayerIndex, PlayerIndex)])
makePairings prev = pairings where
                toMatch =  fmap snd $ filter (not . fst) $ imap (\i x@(_,_,_,_,b) -> (b,(i,x))) prev
                sorted :: [PlayerIndex]
                sorted = sortBy (\a b -> compare (points b) (points a) <> compare (oppPoints b) (oppPoints a) <> compare (initialSeed a) (initialSeed b)) $ fmap (^. _1) toMatch
                points :: PlayerIndex -> Int
                points i = toMatch ^?! traverse . filtered ((==i) . (^. _1)) . _2 . _1
                oppPoints :: PlayerIndex -> Int
                oppPoints i = toMatch ^?! traverse . filtered ((==i) . (^. _1)) . _2 . _2
                pastOppontents :: PlayerIndex -> [PlayerIndex]
                pastOppontents i = toMatch ^.. traverse . filtered ((==i) . (^. _1)) . _2 . _3 . traverse
                in3WayBefore :: PlayerIndex -> Bool
                in3WayBefore i = toMatch ^?! traverse . filtered ((==i) . (^. _1)) . _2 . _4
                initialSeed :: PlayerIndex -> Int
                initialSeed = id
                popFirst :: (Monad m) => StateT [PlayerIndex] m PlayerIndex
                popFirst = state (\(x:xs) -> (x,xs))
                pop' [] = []
                pop' (x:xs) = (x,xs):map (\(y,ys) -> (y,x:ys)) (pop' xs)
                pop :: StateT [PlayerIndex] [] PlayerIndex
                pop = StateT pop'
                getPairs = do
                    b <- gets null
                    if b
                    then return []
                    else do
                        x <- popFirst
                        y <- pop
                        guard (not $ y `elem` pastOppontents x)
                        pairs <- getPairs
                        return ((x,y):pairs)
                getMatches = do
                    b <- gets $ even . length
                    threeWay <- if b
                        then return Nothing
                        else fmap Just get3Way
                    pairs <- getPairs
                    return (threeWay, pairs)
                get3Way = do
                    zoom reversed $ do
                        x <- pop
                        guard $ not $ in3WayBefore x
                        y <- pop
                        guard $ not $ in3WayBefore y
                        z <- pop
                        guard $ not $ in3WayBefore z
                        guard $ not $ (y `elem` pastOppontents x) && (z `elem` pastOppontents x) && (z `elem` pastOppontents y) --not every pair of them has played each other before
                        return $ (z,y,x)
                pairings = head $ evalStateT getMatches sorted

applyResults :: (Maybe (PlayerIndex, PlayerIndex, PlayerIndex), [(PlayerIndex, PlayerIndex)]) -> RoundData -> [(Int, Int, [PlayerIndex], Bool, Bool)] -> [(Int, Int, [PlayerIndex], Bool, Bool)]
applyResults (threeWayMatch, matches) (RoundData r) = calcPastOppontentsPoints . imap applyPartially where
                                applyPartially player (points, _, pastOppontents, hasPlayed3Way, hasDropped) = (points', undefined, pastOppontents', hasPlayed3Way', hasDropped') where
                                    points' = points + fromMaybe 0 (r ^? ix player . _Just . _1 . to resultPoints)
                                    pastOppontents' = oppontents <> pastOppontents
                                    oppontents = if isIn3Way then threeWayMatch ^.. _Just . each . filtered (/= player)
                                                             else duplicate $ matches ^.. traverse . filtered (\(x,y) -> x == player || y == player) . each . filtered (/= player)
                                    hasPlayed3Way' = hasPlayed3Way || isIn3Way
                                    isIn3Way = has (_Just . each . filtered (== player)) threeWayMatch
                                    hasDropped' = hasDropped || fromMaybe False (r ^? ix player . _Just . _2)
                                duplicate [] = []
                                duplicate [x] = [x,x]
                                calcPastOppontentsPoints partialResult = flip imap partialResult $ \i x -> x & _2 .~ (sum $ fmap (\i -> partialResult ^?! ix i . _1) (x ^. _3))

resultPoints :: Result -> Int
resultPoints NoPoints = 0
resultPoints OnePoint = 1
resultPoints ThreePoints = 3

fromRoundData :: Int -> [RoundData] -> [(Int, Int, [PlayerIndex], Bool, Bool)]
fromRoundData numPlayers = foldl (\a r -> applyResults (makePairings a) r a) [(0,0,[],False,False) | _ <- [1..numPlayers]]

listStandings :: [PlayerName] -> [RoundData] -> [(Int, PlayerName, Int, Float, Bool, [Int])]
listStandings players rounds = fmap (\(rank, ((player, i), (points, oppTotal, _, _, dropped))) -> (rank, player, points, (fromIntegral oppTotal / (fromIntegral (length rounds) * 2)), dropped, perRoundResults i rounds)) $ rankOn (\(_,(points, oppPoints, _, _, dropped)) -> (dropped, -points, -oppPoints)) $ zip (zip players [0..length players - 1]) $ fromRoundData (length players) rounds where
                            rankOn _ [] = []
                            rankOn f xs = reverse $ ranks $ sortOn fst $ fmap (\x -> (f x, x)) xs
                            ranks (x:xs) = rankHelper (1,1) x xs []
                            rankHelper (!r, _) x [] rs = (r,snd x):rs
                            rankHelper (!r, !n) x (x':xs) rs | fst x < fst x' = rankHelper (succ n, succ n) x' xs ((r,snd x):rs)
                                                             | otherwise = rankHelper (r, succ n) x' xs ((r,snd x):rs)
                            perRoundResults i rounds = rounds ^.. traverse . roundData . ix i . _Just . _1 . to resultPoints

switchPage :: (RootLink, Page) -> (RootLink, Page) -> (RootLink, Page)
switchPage = const --maybe keep some data?

getPage :: URI -> Maybe (RootLink, Page)
getPage u = case route (Proxy @ ClientRoutes) clientPages u of
            Right page -> Just page
            Left _ -> Nothing

the404 :: View Action
the404 = text "404 Page Not Found"
