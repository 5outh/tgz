{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module TheGreatZimbabwe where

import           Prelude                        hiding (round)

import           Control.Lens
import           Control.Monad
import           Data.List
import qualified Data.Map.Strict                as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import           Data.Validation
import           Numeric.Natural
import           TheGreatZimbabwe.Database.User (UserId (..))
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.MapLayout
import           TheGreatZimbabwe.Text
import           TheGreatZimbabwe.Types
import           TheGreatZimbabwe.Validation

-- * Common

cyclePlayers :: Game -> Either GameError Game
cyclePlayers game = do
  mPlayer <- nextPlayer
  case mPlayer of
    Just player ->
      pure
        $  game
        <> ( mempty & round .~ mempty { roundCurrentPlayer = Last (Just player)
                                      }
           )
    Nothing -> newRound game
 where
  players' :: [UserId]
  players'   = game ^. round . players

  nextPlayer = case (getLast $ game ^. round . currentPlayer) of
    Nothing       -> internalError "Could not find current player."
    Just playerId -> case elemIndex playerId players' of
      Nothing    -> internalError "Current player was not found in game."
      Just index -> if index < length players' - 1
        then Right (Just $ players' !! index)
        else Right Nothing -- signals end of turn

newRound :: Game -> Either GameError Game
newRound game = internalError "cannot start a new round yet"

-- * Pre-Setup

withPlayer :: UserId -> Player -> Game
withPlayer playerId player =
  mempty { gamePlayers = Merge (M.singleton playerId player) }

chooseEmpire :: Empire -> UserId -> Game -> PlayerAction 'PreSetup
chooseEmpire empire' playerId game = PlayerAction $ do
  let chosenEmpires =
        mapMaybe (getAlt . playerEmpire) $ M.elems $ getMerge $ game ^. players
      withEmpire = mempty { playerEmpire = Alt (Just empire') }

  phaseIs PreSetup game
  (empire' `elem` chosenEmpires)
    `impliesInvalid` "Empire has already been chosen."

  pure $ game <> withPlayer playerId withEmpire

-- * Setup

-- TODO: UserId should always match current player id

placeStartingMonument :: Location -> UserId -> Game -> PlayerAction 'Setup
placeStartingMonument location playerId game = PlayerAction $ do
  phaseIs Setup game
  not isStartingMonument
    `impliesInvalid` (  "Cannot place monument at non-starting location: "
                     <> tshow location
                     )
  -- TODO must be starting location
  pure $ game <> changeSet
 where
  isStartingMonument = case (game ^. mapLayout . to getFirst) of
    Nothing -> False
    Just (MapLayout layout) ->
      M.lookup location layout == Just (Land StartingArea)

  withStartingMonument =
    mempty { playerMonuments = Merge (M.singleton location 1) }
  changeSet = withPlayer playerId withStartingMonument
-- * Generosity of Kings Phase

data GenerosityOfKingsAction = Bid Natural | Pass

getPlayer :: UserId -> Game -> Either GameError Player
getPlayer playerId game =
  case M.lookup playerId (getMerge $ game ^. players) of
    Nothing ->
      invalidAction $ "Player with id " <> tshow playerId <> " does not exist."
    Just player -> pure player

isPlayersTurn :: UserId -> Game -> Either GameError ()
isPlayersTurn playerId game =
  when (game ^. round . currentPlayer . to getLast /= Just playerId)
    $ invalidAction "It is not your turn."

-- TODO: How to cycle players in a foolproof way?

bid :: Natural -> UserId -> Game -> PlayerAction 'GenerosityOfKings
bid amount playerId game = PlayerAction $ do
  isPlayersTurn playerId game

  (game ^. round . currentPhase . to getLast /= Just GenerosityOfKings)
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
  playersPassedLens :: Lens' Game [UserId]
  playersPassedLens = round . generosityOfKingsState . playersPassed

  minimumBid :: Natural
  minimumBid =
    maybe 0 succ $ getLast (game ^. round . generosityOfKingsState . lastBid)

  modifiedGenerosityOfKings :: GenerosityOfKingsState
  modifiedGenerosityOfKings =
    mempty { generosityOfKingsStateCattlePool = Sum (-amount) }

  modifiedPlayer = mempty { playerCattle = Sum (negate $ fromIntegral amount) }

pass :: UserId -> Game -> PlayerAction 'GenerosityOfKings
pass playerId game = PlayerAction $ do
  (playerId `elem` game ^. playersPassedLens)
    `impliesInvalid` "You have already passed."
  pure $ game & playersPassedLens %~ (playerId :)
 where
  playersPassedLens :: Lens' Game [UserId]
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

chooseGod :: God -> UserId -> Game -> PlayerAction 'ReligionAndCulture
chooseGod = undefined

chooseSpecialist
  :: Specialist -> UserId -> Game -> PlayerAction 'ReligionAndCulture
chooseSpecialist = undefined

useSpecialist
  :: Specialist -> UserId -> Game -> PlayerAction 'ReligionAndCulture
useSpecialist = undefined
 where
  use Shaman       = undefined
  use RainCeremony = undefined
  use Nomads       = undefined
  use (Builder _)  = undefined

buildMonument :: Location -> UserId -> Game -> PlayerAction 'ReligionAndCulture
buildMonument = undefined

-- TODO: Craftsmen have shapes, which must be validated. Location is top-left
-- of Craftsman tile
placeCraftsmen
  :: [(Location, Craftsman)]
  -- ^ Note: This can be empty just to trigger a 'raisePrices' command.
  -> UserId
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
  -> UserId
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
  :: TechnologyCard -> UserId -> Game -> PlayerAction 'ReligionAndCulture
setPrice = undefined

-- Raise prices of a Player's technology cards. Available after taking the
-- placeCraftsmen action.
raisePrices
  :: [(TechnologyCard, Natural)]
  -> UserId
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
  if (isJust $ getAlt $ gameWinner game)
    then announceWinner game
    else do
      nextGame <- playRound game
      gameLoop game
