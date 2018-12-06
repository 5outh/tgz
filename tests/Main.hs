{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Prelude                        hiding (round)

import           Control.Lens
import           Data.Foldable
import qualified Data.List                      as L
import qualified Data.Map.Strict                as M
import           Data.Monoid
import qualified Data.Set                       as S
import           Data.Validation
import           Test.Hspec
import           TheGreatZimbabwe
import           TheGreatZimbabwe.Database.User (UserId (..))
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.Game
import           TheGreatZimbabwe.MapLayout
import           TheGreatZimbabwe.NewGame
import           TheGreatZimbabwe.Types

main :: IO ()
main = hspec $ do
  describe "pre-setup" $ do
    it "lets a player choose an empire" $ do
      game0 <- refineEither =<< getGameEvent <$> newGame
        [ (PlayerId 1, (PlayerInfo (Username "5outh") "bkovach13@gmail.com"))
        , (PlayerId 2, (PlayerInfo (Username "arcas") "arcas@example.com"))
        ]
      let game = game0 & round . currentPlayer .~ Just (PlayerId 1)
      steppedGame <-
        refineEither $ getPlayerAction $ (chooseEmpire Zulu (PlayerId 1) game)
      newPlayer <- refineEither $ getPlayer (PlayerId 1) steppedGame

      playerEmpire newPlayer `shouldBe` Just Zulu

  describe "generosity of kings" $ do
    it "lets a player pass" $ do
      let vSteppedGame = getPlayerAction (pass (PlayerId 1) emptyGame)
      case vSteppedGame of
        Left  err         -> error $ show err
        Right steppedGame -> do
          let gok = steppedGame ^. round . generosityOfKingsState
          gok ^. playersPassed `shouldBe` [PlayerId 1]

    it "lets a player bid" $ do
      let vSteppedGame = getPlayerAction (bid 3 (PlayerId 1) emptyGame)
      case vSteppedGame of
        Left  err         -> error $ show err
        Right steppedGame -> do
          let gok     = steppedGame ^. round . generosityOfKingsState
          let player1 = getPlayer (PlayerId 1) steppedGame
          gok ^. playersPassed `shouldBe` []
          case player1 of
            Left  err    -> error $ show err
            Right player -> player ^. cattle `shouldBe` 0

    it "does not let a player bid if they don't have enough money" $ do
      let vSteppedGame = getPlayerAction (bid 4 (PlayerId 1) emptyGame)
      vSteppedGame `shouldFailWith` "You do not have enough cattle (need 4)."

    it "does not let a player bid if they have already passed" $ do
      let eSteppedGame = do
            nextGame <- getPlayerAction $ pass (PlayerId 1) emptyGame
            getPlayerAction $ bid 1 (PlayerId 1) nextGame

      eSteppedGame `shouldFailWith` "You cannot bid; you have already passed."

    it "does not let a player bid if it is not their turn" $ do
      let eSteppedGame = getPlayerAction $ bid 1 (PlayerId 2) emptyGame
      eSteppedGame `shouldFailWith` "It's not your turn."

  testMapGraph

-- brittany-disable-next-binding

testMapGraph :: Spec
testMapGraph = do
  let
    -- unsafely create a location
    l [a,b] = Location (read [b]) a
    water = Water
    blank = Land BlankLand
    connectionExists graph node1 node2 = do
      let
        mNodes = M.lookup node1 (mapGraphEdges graph)
      case mNodes of
        Nothing -> error (show node1 <> " is not connected to " <> show node2)
        Just nodes -> (node2 `S.member` nodes) `shouldBe` True

  describe "mapConnections" $ do
    it "should return all connections from a land space" $ do
      let
        layout = MapLayout $ M.fromList
          [ (l "a1" , water), (l "a2", water), (l "a3", water)
          , (l "b1" , water), (l "b2", blank), (l "b3", water)
          , (l "c1" , water), (l "c2", water), (l "c3", water)
          ]
        graph = constructMapGraph layout

      mapConnections graph (l "b2") `shouldBe`
        (S.fromList $ map l ["a1","a2","a3","b1","b3","c1","c2","c3"])

    it "should return all connections from a water space" $ do
      let
        layout = MapLayout $ M.fromList
          [ (l "a1" , blank), (l "a2", blank), (l "a3", blank)
          , (l "b1" , blank), (l "b2", water), (l "b3", blank)
          , (l "c1" , blank), (l "c2", blank), (l "c3", blank)
          ]
        graph = constructMapGraph layout

      mapConnections graph (l "b2") `shouldBe`
        (S.fromList $ map l ["a1","a2","a3","b1","b3","c1","c2","c3"])

    it "should be able to reach all spaces" $ do
      let
        layout = MapLayout $ M.fromList
          [ (l "a1" , blank), (l "a2", blank), (l "a3", blank)
          , (l "b1" , blank), (l "b2", blank), (l "b3", blank)
          , (l "c1" , blank), (l "c2", blank), (l "c3", blank)
          ]
        graph = constructMapGraph layout

      mapConnectionsN 3 graph (S.singleton (l "a1")) `shouldBe`
        (S.fromList $ map l ["a1", "a2","a3","b1","b2","b3","c1","c2","c3"])

    it "should be reasonably efficient" $ do
      let
        layout = MapLayout $ M.fromList $ map (,blank) $ Location <$> [1..99] <*> ['a' .. 'z']
        graph = constructMapGraph layout
      -- this is kind of a silly test, but i know the implementation isn't optimal
      -- and just want to do something bogus to validate that it runs in a reasonable
      -- amount of time.
      length (mapConnectionsN 15 graph (S.singleton (Location 99 'g'))) `shouldBe`
        352

  describe "constructMapGraph" $ do
    it "treats an all-water layout properly" $ do
      let
        layout = MapLayout $ M.fromList
          [ (l "a1" , water), (l "a2", water), (l "a3", water)
          , (l "b1" , water), (l "b2", water), (l "b3", water)
          , (l "c1" , water), (l "c2", water), (l "c3", water)
          ]
        graph = constructMapGraph layout
        locations = Location <$> [1..3] <*> "abc"
      for_ locations $ \location ->
        M.lookup location (mapGraphNodes graph) `shouldBe` Just (WaterNode 0)

      M.lookup 0 (mapGraphWaterNodes graph) `shouldBe` Just (S.fromList locations)

    it "creates a connection between water and land" $ do
      let
        layout = MapLayout $ M.fromList
          [ (l "a1" , water), (l "a2", water), (l "a3", water)
          , (l "b1" , water), (l "b2", water), (l "b3", water)
          , (l "c1" , water), (l "c2", water), (l "c3", blank)
          ]
        graph = constructMapGraph layout
        waterLocations = (Location <$> [1..3] <*> "abc") L.\\ [Location 3 'c']

      M.lookup 0 (mapGraphWaterNodes graph) `shouldBe` Just (S.fromList waterLocations)
      mapGraphEdges graph `shouldBe`
        M.fromList
          [ (WaterNode 0, S.singleton $ LandNode (Location 3 'c'))
          , (LandNode (Location 3 'c'), S.singleton $ WaterNode 0)
          ]

    it "creates connections between all beach squares" $ do
      let
        layout = MapLayout $ M.fromList
          [ (l "a1" , blank), (l "a2", blank), (l "a3", blank)
          , (l "b1" , blank), (l "b2", water), (l "b3", blank)
          , (l "c1" , blank), (l "c2", blank), (l "c3", blank)
          ]
        graph = constructMapGraph layout
        landLocations = (Location <$> [1..3] <*> "abc") L.\\ [Location 2 'b']

      M.lookup 0 (mapGraphWaterNodes graph) `shouldBe` Just (S.singleton (Location 2 'b'))
      for_ landLocations $ \location ->
        M.lookup location (mapGraphNodes graph) `shouldBe` Just (LandNode location)
      M.lookup (WaterNode 0) (mapGraphEdges graph)
        `shouldBe` Just (S.fromList (map LandNode landLocations))

    it "creates two WaterNodes" $ do
      let
        layout = MapLayout $ M.fromList
          [ (l "a1" , water), (l "a2", blank), (l "a3", blank)
          , (l "b1" , water), (l "b2", blank), (l "b3", water)
          , (l "c1" , blank), (l "c2", blank), (l "c3", water)
          ]
        graph = constructMapGraph layout

      M.lookup 0 (mapGraphWaterNodes graph) `shouldBe` Just (S.fromList [l "a1", l "b1"] )
      M.lookup 1 (mapGraphWaterNodes graph) `shouldBe` Just (S.fromList [l "b3", l "c3"] )

      connectionExists graph (LandNode (l "a3")) (WaterNode 1)
      connectionExists graph (LandNode (l "c1")) (WaterNode 0)

    it "creates two WaterNodes from diagonals" $ do
      let
        layout = MapLayout $ M.fromList
          [ (l "a1" , water), (l "a2", blank), (l "a3", blank)
          , (l "b1" , blank), (l "b2", water), (l "b3", blank)
          , (l "c1" , blank), (l "c2", blank), (l "c3", blank)
          ]
        graph = constructMapGraph layout

      M.lookup 0 (mapGraphWaterNodes graph) `shouldBe` Just (S.singleton ( l "a1" ) )
      M.lookup 1 (mapGraphWaterNodes graph) `shouldBe` Just (S.singleton ( l "b2" ) )

refineEither :: Show err => Either err a -> IO a
refineEither = \case
  Left  err -> error (show err)
  Right a   -> pure a

shouldFailWith thing message = case thing of
  Left  err -> err `shouldBe` InvalidAction message
  Right _   -> error "Got 'Right', expected 'Left'"

emptyGame = mempty
  { gamePlayers   = M.fromList [(PlayerId 1, p1), (PlayerId 2, p2)]
  , gameRound     =
    Round
      [PlayerId 1, PlayerId 2]
      (Just (PlayerId 1))
      (M.empty)
      (GenerosityOfKingsState [PlayerPlaque Kilwa, PlayerPlaque Zulu]
                              0
                              (Just 0)
                              []
      )
      (Just GenerosityOfKings)
  , gameMapLayout = MapLayout M.empty
  }

p1 :: Player
p1 = mempty
  { playerInfo = Just $ PlayerInfo (Username "5outh") "bkovach13@gmail.com"
  , playerVictoryRequirement = Points 20 0
  , playerEmpire             = Just Kilwa
  , playerCattle             = 3
  , playerGod                = Nothing
  }

p2 :: Player
p2 = mempty
  { playerInfo = (Just $ PlayerInfo (Username "arcas") "arcas@example.com")
  , playerVictoryRequirement = Points 20 0
  , playerEmpire             = (Just Zulu)
  , playerCattle             = 3
  , playerGod                = Nothing
  }
