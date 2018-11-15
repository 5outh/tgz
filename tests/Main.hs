{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude                hiding (round)

import           Control.Lens
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
import           Data.Validation
import           Test.Hspec
import           TheGreatZimbabwe
import           TheGreatZimbabwe.Types

main :: IO ()
main = hspec $ do
  describe "test suite" $ do
    it "runs" $ do
      True `shouldBe` True

  describe "generosity of kings" $ do
    it "lets a player pass" $ do
      let vSteppedGame = getPlayerAction (pass (PlayerId 1) emptyGame)
      case vSteppedGame of
        Failure err         -> error $ show err
        Success steppedGame -> do
          let gok = roundGenerosityOfKingsState (gameRound steppedGame)
          steppedGame
            ^.         round
            .          generosityOfKingsState
            .          playersPassed
            `shouldBe` [PlayerId 1]

    it "lets a player bid" $ do
      pending

emptyGame = Game
  { gamePlayers   = M.fromList [(PlayerId 1, p1), (PlayerId 2, p2)]
  , gameRound     = Round []
                          M.empty
                          (GenerosityOfKingsState [(Kilwa, 0), (Zulu, 0)] 0 [])
                          GenerosityOfKings
  , gameMapLayout = MapLayout M.empty
  , gameCraftsmen = M.empty
  , gameWinner    = Nothing
  }

p1 :: Player
p1 = Player
  { playerInfo = PlayerInfo (Username "5outh") "bkovach13@gmail.com"
  , playerVictoryRequirement = 20
  , playerVictoryPoints      = 0
  , playerEmpire             = Kilwa
  , playerCattle             = 3
  , playerMonuments          = M.empty
  , playerCraftsmen          = S.empty
  , playerTechnologyCards    = M.empty
  , playerSpecialists        = S.empty
  , playerGod                = Nothing
  }

p2 :: Player
p2 = Player
  { playerInfo               = PlayerInfo (Username "arcas") "arcas@example.com"
  , playerVictoryRequirement = 20
  , playerVictoryPoints      = 0
  , playerEmpire             = Zulu
  , playerCattle             = 3
  , playerMonuments          = M.empty
  , playerCraftsmen          = S.empty
  , playerTechnologyCards    = M.empty
  , playerSpecialists        = S.empty
  , playerGod                = Nothing
  }
