{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude                  hiding (round)

import           Control.Lens
import qualified Data.Map.Strict          as M
import           Data.Monoid
import qualified Data.Set                 as S
import           Data.Validation
import           Test.Hspec
import           TheGreatZimbabwe
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.NewGame
import           TheGreatZimbabwe.Types

main :: IO ()
main = hspec $ do
  describe "pre-setup" $ do
    it "lets a player choose an empire" $ do
      Right game <- getGameEvent <$> newGame
        [ (PlayerId 1, (PlayerInfo (Username "5outh") "bkovach13@gmail.com"))
        , (PlayerId 2, (PlayerInfo (Username "arcas") "arcas@example.com"))
        ]
      let Right steppedGame =
            getPlayerAction $ (chooseEmpire Zulu (PlayerId 1) game)
          Right newPlayer = getPlayer (PlayerId 1) steppedGame

      playerEmpire newPlayer `shouldBe` Alt (Just Zulu)

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
      eSteppedGame `shouldFailWith` "It is not your turn."

shouldFailWith thing message = case thing of
  Left  err -> err `shouldBe` InvalidAction message
  Right _   -> error "Got 'Right', expected 'Left'"

emptyGame = Game
  { gamePlayers   = M.fromList [(PlayerId 1, p1), (PlayerId 2, p2)]
  , gameRound     = Round [PlayerId 1, PlayerId 2]
                          (PlayerId 1)
                          M.empty
                          (GenerosityOfKingsState [Kilwa, Zulu] 0 0 [])
                          GenerosityOfKings
  , gameMapLayout = MapLayout M.empty
  , gameCraftsmen = M.empty
  , gameWinner    = Nothing
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
