{-# LANGUAGE OverloadedStrings #-}
module TheGreatZimbabwe.Game where

import           Control.Lens
import           Data.List              (elemIndex)
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Elm.Derive
import           GHC.Natural
import           Prelude                hiding (round)
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.Text
import           TheGreatZimbabwe.Types

getPlayer :: PlayerId -> Game -> Either GameError Player
getPlayer playerId game = case M.lookup playerId (game ^. players) of
  Nothing ->
    invalidAction $ "Player with id " <> tshow playerId <> " does not exist."
  Just player -> pure player

getPlayers :: Game -> Either GameError [Player]
getPlayers game = pure $ M.elems (game ^. players)

cyclePlayersWithPasses :: Game -> Either GameError Game
cyclePlayersWithPasses game = do
  player <- nextPlayer (game ^. round . currentPlayer)
  let setCurrentPlayer = mempty & round .~ mempty
        { roundCurrentPlayer = Just player
        , roundStep          = 1
        }
      stepRound = mempty { gameStep = 1 }

  (length gokPlayersPassed == length players')
    `impliesError` "All players have passed"

  pure $ game <> setCurrentPlayer <> stepRound
 where
  players' :: [PlayerId]
  players'         = game ^. round . players

  gokPlayersPassed = game ^. round . generosityOfKingsState . playersPassed

  nextPlayer player' = case player' of
    Nothing       -> internalError "Could not find current player."
    Just playerId -> case elemIndex playerId players' of
      Nothing -> internalError "Current player was not found in game."
      Just i  -> if i < length players' - 1
        then
          let nextPlayerId = players' !! succ i
          in  if nextPlayerId `notElem` gokPlayersPassed
                then Right nextPlayerId
                else nextPlayer (Just nextPlayerId)
        else Right (head players')

cyclePlayers :: Game -> Either GameError Game
cyclePlayers game = do
  player <- nextPlayer
  let setCurrentPlayer = mempty & round .~ mempty
        { roundCurrentPlayer = Just player
        , roundStep          = 1
        }
      stepRound = mempty { gameStep = 1 }
  pure $ game <> setCurrentPlayer <> stepRound
 where
  players' :: [PlayerId]
  players'   = game ^. round . players

  nextPlayer = case (game ^. round . currentPlayer) of
    Nothing       -> internalError "Could not find current player."
    Just playerId -> case elemIndex playerId players' of
      Nothing -> internalError "Current player was not found in game."
      Just i  -> if i < length players' - 1
        then Right (players' !! succ i)
        else Right (head players')

setPlayer :: PlayerId -> Player -> Game
setPlayer playerId player =
  mempty { gamePlayers = M.singleton playerId player }

-- TODO: This must be validated - a player can only go up to 40 VR:
-- any actions that push them over that limit are disallowed.
addVictoryRequirement :: Int -> PlayerId -> Game -> Game
addVictoryRequirement pointsToAdd playerId game = setPlayer playerId stepPlayer
 where
  stepPlayer = mempty & victoryRequirement .~ Points
    { pointsStep   = game ^. step
    , pointsPoints = pointsToAdd
    }

addVictoryPoints :: Int -> PlayerId -> Game -> Game
addVictoryPoints pointsToAdd playerId game = setPlayer playerId stepPlayer
 where
  stepPlayer = mempty & victoryPoints .~ Points
    { pointsStep   = game ^. step
    , pointsPoints = pointsToAdd
    }

addCattle :: Int -> PlayerId -> Game
addCattle n playerId = setPlayer playerId (mempty & cattle .~ n)

subtractCattle :: Int -> PlayerId -> Game
subtractCattle n playerId = setPlayer playerId (mempty & cattle .~ (-n))

newRound :: Game -> Either GameError Game
newRound _ = internalError "cannot start a new round yet"

fromMaybeError :: T.Text -> Maybe a -> Either GameError a
fromMaybeError message ma = case ma of
  Nothing -> internalError message
  Just a  -> pure a
