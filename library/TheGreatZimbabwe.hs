{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module TheGreatZimbabwe where

import           Prelude                    hiding (round)

import           Control.Lens
import           Control.Monad
import qualified Data.Map.Strict            as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Validation
import           Numeric.Natural
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.MapLayout
import           TheGreatZimbabwe.Text
import           TheGreatZimbabwe.Types

-- Pre-Setup

chooseEmpire :: Empire -> PlayerId -> Game -> PlayerAction 'PreSetup
chooseEmpire empire' playerId game = PlayerAction $ do
  let chosenEmpires =
        mapMaybe (getAlt . playerEmpire) $ M.elems $ game ^. players
      setEmpire = mempty { playerEmpire = Alt (Just empire') }

  (empire' `elem` chosenEmpires)
    `impliesInvalid` "Empire has already been chosen."

  pure $ game & over (players . at playerId) (fmap (mappend setEmpire))

-- * Setup

placeStartingMonument :: Location -> PlayerId -> Game -> PlayerAction 'Setup
placeStartingMonument = undefined
  -- must not be a monument at the location
  -- must be a starting location

-- * Generosity of Kings Phase

data GenerosityOfKingsAction = Bid Natural | Pass

impliesInvalid :: Bool -> T.Text -> Either GameError ()
predicate `impliesInvalid` err = when predicate $ invalidAction err

getPlayer :: PlayerId -> Game -> Either GameError Player
getPlayer playerId game = case M.lookup playerId (game ^. players) of
  Nothing ->
    invalidAction $ "Player with id " <> tshow playerId <> " does not exist."
  Just player -> pure player

isPlayersTurn :: PlayerId -> Game -> Either GameError ()
isPlayersTurn playerId game = when (game ^. round . currentPlayer /= playerId)
  $ invalidAction "It is not your turn."

-- TODO: How to cycle players in a foolproof way?

bid :: Natural -> PlayerId -> Game -> PlayerAction 'GenerosityOfKings
bid amount playerId game = PlayerAction $ do
  isPlayersTurn playerId game

  (game ^. round . currentPhase /= GenerosityOfKings)
    `impliesInvalid` "You can only take the bid action in the Generosity of Kings phase"

  (playerId `elem` game ^. playersPassedLens)
    `impliesInvalid` "You cannot bid; you have already passed."

  player <- getPlayer playerId game

  let notEnoughCattle =
        "You do not have enough cattle (need " <> tshow amount <> ")."
  (player ^. cattle < fromIntegral amount) `impliesInvalid` notEnoughCattle

  (amount < minimumBid)
    `impliesInvalid` ("You must bid " <> tshow minimumBid <> " cattle or more.")

  pure $ modifyGame game
 where
  playersPassedLens :: Lens' Game [PlayerId]
  playersPassedLens = round . generosityOfKingsState . playersPassed

  minimumBid :: Natural
  minimumBid = succ $ game ^. round . generosityOfKingsState . lastBid

  modifyGenerosityOfKings :: GenerosityOfKingsState -> GenerosityOfKingsState
  modifyGenerosityOfKings gok = gok & cattlePool +~ amount

  modifiedPlayer = mempty { playerCattle = Sum (negate $ fromIntegral amount) }

  modifyGame :: Game -> Game
  modifyGame =
    over (players . at playerId) (fmap (mappend modifiedPlayer))
      . over (round . generosityOfKingsState) modifyGenerosityOfKings

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

data ReligionAndCultureAction1 = ChooseGod God | ChooseSpecialist Specialist
data UseSpecialist = UseSpecialist Specialist
-- NB. f ~ Identity usually, or [] for Obatala
data ReligionAndCultureAction3 f
  = BuildMonument (f Location)
  | PlaceCraftsmen [(Location, Craftsman)]
  | RaiseMonuments [(Location, [RaiseMonumentCommand])]

data ReligionAndCultureAction f = ReligionAndCultureAction
  { religionAndCultureAction1 :: Maybe ReligionAndCultureAction1
  , religionAndCultureAction2 :: Maybe UseSpecialist
  , religionAndCultureAction3 :: Maybe (ReligionAndCultureAction3 f)
  }

chooseGod :: God -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
chooseGod = undefined

chooseSpecialist
  :: Specialist -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
chooseSpecialist = undefined

useSpecialist
  :: Specialist -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
useSpecialist = undefined
 where
  use Shaman       = undefined
  use RainCeremony = undefined
  use Nomads       = undefined
  use (Builder _)  = undefined

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

gameLoop game = do
  if (isJust $ gameWinner game)
    then announceWinner game
    else do
      nextGame <- playRound game
      gameLoop game
