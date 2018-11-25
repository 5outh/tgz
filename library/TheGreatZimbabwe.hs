{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds    #-}

module TheGreatZimbabwe where

import           Prelude                            hiding (round)

import           Control.Lens
import           Data.List
import qualified Data.Map.Strict                    as M
import           Data.Maybe
import           Numeric.Natural
import           Safe
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.Text
import           TheGreatZimbabwe.Types
import           TheGreatZimbabwe.Types.GameCommand
import           TheGreatZimbabwe.Validation

-- TODO: How to handle game commands issued by the game itself? For example,
-- income after each phase should be recorded.

-- Run a game command

runGameCommand :: Game -> GameCommand -> Either GameError Game
runGameCommand game = \case
  -- TODO: When to break out of presetup?
  ChooseEmpire e playerId ->
    handleFinishPreSetup (chooseEmpire e playerId game)
  PlaceStartingMonument location playerId ->
    getPlayerAction (placeStartingMonument location playerId game)

runGameCommands :: Game -> [GameCommand] -> Either GameError Game
runGameCommands game commands = foldl' go (Right game) commands
 where
  go :: Either GameError Game -> GameCommand -> Either GameError Game
  go eGame command = case eGame of
    Left  err   -> Left err
    Right game' -> runGameCommand game' command

-- * Common

cyclePlayers :: Game -> Either GameError Game
cyclePlayers game = do
  player <- nextPlayer
  pure $ game <> (mempty & round .~ mempty { roundCurrentPlayer = Just player })
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

newRound :: Game -> Either GameError Game
newRound _ = internalError "cannot start a new round yet"

-- * Pre-Setup

withPlayer :: PlayerId -> Player -> Game
withPlayer playerId player =
  mempty { gamePlayers = M.singleton playerId player }

chooseEmpire :: Empire -> PlayerId -> Game -> PlayerAction 'PreSetup
chooseEmpire empire' playerId game = PlayerAction $ do
  let chosenEmpires = mapMaybe playerEmpire $ M.elems $ game ^. players
      withEmpire    = mempty { playerEmpire = Just empire' }

  phaseIs PreSetup game
  playerIs playerId game

  (empire' `elem` chosenEmpires)
    `impliesInvalid` "Empire has already been chosen."

  pure $ game <> withPlayer playerId withEmpire

-- TODO: Move this logic into 'newRound' below
--
handleFinishPreSetup :: PlayerAction 'PreSetup -> Either GameError Game
handleFinishPreSetup (PlayerAction act) = do
  game <- act
  let players'    = M.elems (game ^. players)
      firstPlayer = headMay (game ^. round . players)
  -- Set phase = Setup
  -- Set currentPlayer = first player
  cyclePlayers $ if (all isJust $ map playerEmpire players')
    then
      game
      & (round . currentPhase .~ Just Setup)
      . (round . currentPlayer .~ firstPlayer)
    else game

-- * Setup

-- TODO: PlayerId should always match current player id

placeStartingMonument :: Location -> PlayerId -> Game -> PlayerAction 'Setup
placeStartingMonument location playerId game = PlayerAction $ do
  phaseIs Setup game
  not isStartingMonument
    `impliesInvalid` (  "Cannot place monument at non-starting location: "
                     <> tshow location
                     )
  -- TODO must be starting location
  pure $ game <> changeSet
 where
  isStartingMonument = case (game ^. mapLayout) of
    Nothing -> False
    Just (MapLayout layout) ->
      M.lookup location layout == Just (Land StartingArea)

  withStartingMonument = mempty { playerMonuments = M.singleton location 1 }
  changeSet            = withPlayer playerId withStartingMonument
-- * Generosity of Kings Phase

data GenerosityOfKingsAction = Bid Natural | Pass

getPlayer :: PlayerId -> Game -> Either GameError Player
getPlayer playerId game = case M.lookup playerId (game ^. players) of
  Nothing ->
    invalidAction $ "Player with id " <> tshow playerId <> " does not exist."
  Just player -> pure player

bid :: Natural -> PlayerId -> Game -> PlayerAction 'GenerosityOfKings
bid amount playerId game = PlayerAction $ do
  playerIs playerId game

  (game ^. round . currentPhase /= Just GenerosityOfKings)
    `impliesInvalid` "You can only take the bid action in the Generosity of Kings phase"

  (playerId `elem` game ^. playersPassedLens)
    `impliesInvalid` "You cannot bid; you have already passed."

  player <- getPlayer playerId game

  let notEnoughCattle =
        "You do not have enough cattle (need " <> tshow amount <> ")."
  (player ^. cattle < fromIntegral amount) `impliesInvalid` notEnoughCattle

  (amount < minimumBid)
    `impliesInvalid` ("You must bid " <> tshow minimumBid <> " cattle or more.")

  pure
    $  game
    <> withPlayer playerId modifiedPlayer
    <> (mempty & round . generosityOfKingsState .~ modifiedGenerosityOfKings)
 where
  playersPassedLens :: Lens' Game [PlayerId]
  playersPassedLens = round . generosityOfKingsState . playersPassed

  minimumBid :: Natural
  minimumBid = maybe 0 succ (game ^. round . generosityOfKingsState . lastBid)

  modifiedGenerosityOfKings :: GenerosityOfKingsState
  modifiedGenerosityOfKings =
    mempty { generosityOfKingsStateCattlePool = (-amount) }

  modifiedPlayer = mempty { playerCattle = (negate $ fromIntegral amount) }

pass :: PlayerId -> Game -> PlayerAction 'GenerosityOfKings
pass playerId game = PlayerAction $ do
  (playerId `elem` game ^. playersPassedLens)
    `impliesInvalid` "You have already passed."
  pure $ game & playersPassedLens %~ (playerId :)
 where
  playersPassedLens :: Lens' Game [PlayerId]
  playersPassedLens = round . generosityOfKingsState . playersPassed

clearSlate :: Game -> GameEvent 'GenerosityOfKings
clearSlate = undefined

orderPlaques :: Game -> GameEvent 'GenerosityOfKings
orderPlaques = undefined

endGenerosityOfKings :: Game -> GameEvent 'GenerosityOfKings
endGenerosityOfKings = undefined

-- * Religion and Culture Phase

-- Either choose a god or choose a specialist. You may only perform
-- ONE of these two actions in a turn.
-- •  Use a specialist.
-- •  Either (1) build a monument, (2) place craftsmen, or (3) raise
-- monuments. You may only perform only ONE of these three actions in a turn

data ReligionAndCultureCommand1 = ChooseGod God | ChooseSpecialist Specialist
data UseSpecialist = UseSpecialist Specialist
-- NB. f ~ Identity usually, or [] for Obatala
data ReligionAndCultureCommand3 f
  = BuildMonument (f Location)
  | PlaceCraftsmen [(Location, Craftsman)]
  | RaiseMonuments [(Location, [RaiseMonumentCommand])]

data ReligionAndCultureCommand f = ReligionAndCultureCommand
  { religionAndCultureAction1 :: Maybe ReligionAndCultureCommand1
  , religionAndCultureAction2 :: Maybe UseSpecialist
  , religionAndCultureAction3 :: Maybe (ReligionAndCultureCommand3 f)
  }

chooseGod :: God -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
chooseGod = undefined

chooseSpecialist
  :: Specialist -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
chooseSpecialist = undefined

useSpecialist
  :: Specialist -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
useSpecialist = undefined

buildMonument
  :: Location -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
buildMonument = undefined

-- TODO: Craftsmen have shapes, which must be validated. Location is top-left
-- of Craftsman tile
placeCraftsmen
  :: [(Location, Craftsman)]
  -- ^ Note: This can be empty just to trigger a 'raisePrices' command.
  -> PlayerId
  -> Game
  -> PlayerAction 'ReligionAndCulture
placeCraftsmen = undefined

data RaiseMonumentCommand

-- TODO: This can affect the game state in a variety of ways. need some sequence
-- of commands from the player to execute this action. This is the heft of the
-- game logic.
raiseMonuments
  :: [(Location, [RaiseMonumentCommand])]
  -- ^ For each location user wants to raise, a sequence of commands done in
  -- order to raise it.
  -> PlayerId
  -> Game
  -> PlayerAction 'ReligionAndCulture
raiseMonuments = undefined
 where
  -- NB. This is just for reference; all should be contained in
  -- 'RaiseMonumentCommand'
  obtainRitualGoods     = undefined
  useResource           = undefined
  useSecondaryCraftsman = undefined

-- Set the price of a technology card. Mandatory after choosing a new card.
setPrice
  :: TechnologyCard -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
setPrice = undefined

-- Raise prices of a Player's technology cards. Available after taking the
-- placeCraftsmen action.
raisePrices
  :: [(TechnologyCard, Natural)]
  -> PlayerId
  -> Game
  -> PlayerAction 'ReligionAndCulture
raisePrices = undefined

-- * Revenues

-- TODO: Remember to reset Qamata income card if in play.
collectPayments :: Game -> GameEvent 'Revenues
collectPayments = undefined

collectIncome :: Game -> GameEvent 'Revenues
collectIncome = undefined

-- * Let us compare mythologies

letUsCompareMythologies :: Game -> GameEvent 'LetUsCompareMythologies
letUsCompareMythologies = undefined

-- * Game loop

announceWinner :: Monad m => Game -> m ()
announceWinner = undefined

playRound :: Monad m => Game -> m Game
playRound = undefined
