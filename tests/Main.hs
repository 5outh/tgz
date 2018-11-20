{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude                        hiding (round)

import           Control.Lens
import qualified Data.Map.Strict                as M
import           Data.Monoid
import qualified Data.Set                       as S
import           Data.Validation
import           Database.Persist.Sql           (toSqlKey)
import           Test.Hspec
import           TheGreatZimbabwe
import           TheGreatZimbabwe.Database.User (UserId (..))
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.NewGame
import           TheGreatZimbabwe.Types

main :: IO ()
main = hspec $ do
  describe "pre-setup" $ do
    it "lets a player choose an empire" $ do
      Right game <- getGameEvent <$> newGame
        [ (toSqlKey 1, (PlayerInfo (Username "5outh") "bkovach13@gmail.com"))
        , (toSqlKey 2, (PlayerInfo (Username "arcas") "arcas@example.com"))
        ]
      let Right steppedGame =
            getPlayerAction $ (chooseEmpire Zulu (toSqlKey 1) game)
          Right newPlayer = getPlayer (toSqlKey 1) steppedGame

      playerEmpire newPlayer `shouldBe` Alt (Just Zulu)

  describe "generosity of kings" $ do
    it "lets a player pass" $ do
      let vSteppedGame = getPlayerAction (pass (toSqlKey 1) emptyGame)
      case vSteppedGame of
        Left  err         -> error $ show err
        Right steppedGame -> do
          let gok = steppedGame ^. round . generosityOfKingsState
          gok ^. playersPassed `shouldBe` [toSqlKey 1]

    it "lets a player bid" $ do
      let vSteppedGame = getPlayerAction (bid 3 (toSqlKey 1) emptyGame)
      case vSteppedGame of
        Left  err         -> error $ show err
        Right steppedGame -> do
          let gok     = steppedGame ^. round . generosityOfKingsState
          let player1 = getPlayer (toSqlKey 1) steppedGame
          gok ^. playersPassed `shouldBe` []
          case player1 of
            Left  err    -> error $ show err
            Right player -> player ^. cattle `shouldBe` 0

    it "does not let a player bid if they don't have enough money" $ do
      let vSteppedGame = getPlayerAction (bid 4 (toSqlKey 1) emptyGame)
      vSteppedGame `shouldFailWith` "You do not have enough cattle (need 4)."

    it "does not let a player bid if they have already passed" $ do
      let eSteppedGame = do
            nextGame <- getPlayerAction $ pass (toSqlKey 1) emptyGame
            getPlayerAction $ bid 1 (toSqlKey 1) nextGame

      eSteppedGame `shouldFailWith` "You cannot bid; you have already passed."

    it "does not let a player bid if it is not their turn" $ do
      let eSteppedGame = getPlayerAction $ bid 1 (toSqlKey 2) emptyGame
      eSteppedGame `shouldFailWith` "It is not your turn."

shouldFailWith thing message = case thing of
  Left  err -> err `shouldBe` InvalidAction message
  Right _   -> error "Got 'Right', expected 'Left'"

emptyGame = mempty
  { gamePlayers   = Merge $ M.fromList [(toSqlKey 1, p1), (toSqlKey 2, p2)]
  , gameRound     = Round
    [toSqlKey 1, toSqlKey 2]
    (Last (Just (toSqlKey 1)))
    (Merge M.empty)
    (GenerosityOfKingsState [Kilwa, Zulu] 0 (Last (Just 0)) [])
    (Last (Just GenerosityOfKings))
  , gameMapLayout = First (Just (MapLayout M.empty))
  }

p1 :: Player
p1 = mempty
  { playerInfo               = Alt
    (Just $ PlayerInfo (Username "5outh") "bkovach13@gmail.com")
  , playerVictoryRequirement = Sum 20
  , playerEmpire             = Alt (Just Kilwa)
  , playerCattle             = Sum 3
  , playerGod                = Alt Nothing
  }

p2 :: Player
p2 = mempty
  { playerInfo = Alt (Just $ PlayerInfo (Username "arcas") "arcas@example.com")
  , playerVictoryRequirement = Sum 20
  , playerEmpire             = Alt (Just Zulu)
  , playerCattle             = Sum 3
  , playerGod                = Alt Nothing
  }
