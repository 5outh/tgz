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
import           Debug.Trace
import           Numeric.Natural
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.Text
import           TheGreatZimbabwe.Types
import           TheGreatZimbabwe.Types.GameCommand
import           TheGreatZimbabwe.Validation

-- TODO: How to handle game commands issued by the game itself? For example,
-- income after each phase should be recorded.

-- Run a game command

runGameCommand :: Game -> PlayerId -> GameCommand -> Either GameError Game
runGameCommand game playerId = \case
  ChooseEmpire e -> handleFinishPreSetup (chooseEmpire e playerId game)
  PlaceStartingMonument location ->
    handleFinishSetup (placeStartingMonument location playerId game)
  Bid amount -> handleFinishGenerosityOfKings (bid amount playerId game)
  Pass       -> handleFinishGenerosityOfKings (pass playerId game)

runGameCommands :: Game -> [(PlayerId, GameCommand)] -> Either GameError Game
runGameCommands game commands = foldl' go (Right game) commands
 where
  go
    :: Either GameError Game -> (PlayerId, GameCommand) -> Either GameError Game
  go eGame (playerId, command) = case eGame of
    Left  err   -> Left err
    Right game' -> runGameCommand game' playerId command

-- * Common

cyclePlayersWithPasses :: Game -> Either GameError Game
cyclePlayersWithPasses game = do
  player <- nextPlayer (game ^. round . currentPlayer)
  let setCurrentPlayer =
        mempty & round .~ mempty { roundCurrentPlayer = Just player }
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
  let setCurrentPlayer =
        mempty & round .~ mempty { roundCurrentPlayer = Just player }
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

newRound :: Game -> Either GameError Game
newRound _ = internalError "cannot start a new round yet"

-- * Pre-Setup

setPlayer :: PlayerId -> Player -> Game
setPlayer playerId player =
  mempty { gamePlayers = M.singleton playerId player }

addVictoryRequirement :: Natural -> PlayerId -> Game -> Game
addVictoryRequirement pointsToAdd playerId game = setPlayer playerId stepPlayer
 where
  stepPlayer = mempty & victoryRequirement .~ Points
    { pointsStep   = game ^. step
    , pointsPoints = pointsToAdd
    }

addVictoryPoints :: Natural -> PlayerId -> Game -> Game
addVictoryPoints pointsToAdd playerId game = setPlayer playerId stepPlayer
 where
  stepPlayer = mempty & victoryRequirement .~ Points
    { pointsStep   = game ^. step
    , pointsPoints = pointsToAdd
    }

chooseEmpire :: Empire -> PlayerId -> Game -> PlayerAction 'PreSetup
chooseEmpire empire' playerId game = PlayerAction $ do
  let chosenEmpires = mapMaybe playerEmpire $ M.elems $ game ^. players
      withEmpire    = mempty { playerEmpire = Just empire' }

  playerIs playerId game
  phaseIs PreSetup game

  (empire' `elem` chosenEmpires)
    `impliesInvalid` "Empire has already been chosen."

  pure $ game <> setPlayer playerId withEmpire

handleFinishPreSetup :: PlayerAction 'PreSetup -> Either GameError Game
handleFinishPreSetup (PlayerAction act) = do
  game <- act
  let players' = M.elems (game ^. players)
  cyclePlayers $ if (all isJust $ map playerEmpire players')
    then game & (round . currentPhase .~ Just Setup)
    else game

-- * Setup

handleFinishSetup :: PlayerAction 'Setup -> Either GameError Game
handleFinishSetup (PlayerAction act) = do
  game <- act
  let players' = M.elems (game ^. players)

  cyclePlayers $ if (all (not . M.null) $ map playerMonuments players')
    then setupGenerosityOfKings game
    else game

playerWithShadipinyi :: Game -> Maybe (PlayerId, Player)
playerWithShadipinyi game =
  find ((== Just Shadipinyi) . playerGod . snd) $ M.toList (game ^. players)

setupGenerosityOfKings :: Game -> Game
setupGenerosityOfKings game =
  withPlayerOrder <> setPhase <> resetGenerosityOfKings
 where
  setPhase           = mempty & round . currentPhase .~ Just GenerosityOfKings
  -- current order of players
  currentPlayerOrder = game ^. round . players

  -- players ordered by VR
  playersOrderedByVR =
    sortOn (playerVictoryRequirement . snd) (game ^. players . to M.toList)

  playerPlaques =
    mapMaybe (fmap PlayerPlaque . playerEmpire . snd) playersOrderedByVR

  empirePlaques = if isJust (playerWithShadipinyi game)
    then ShadipinyiPlaque : playerPlaques
    else playerPlaques

  generosityOfKingsState0 = mempty & plaques .~ empirePlaques

  resetGenerosityOfKings =
    mempty & round . generosityOfKingsState .~ generosityOfKingsState0

  withPlayerOrder = game & round . players .~ map fst playersOrderedByVR

-- TODO: Fill out
handleFinishGenerosityOfKings
  :: PlayerAction 'GenerosityOfKings -> Either GameError Game
handleFinishGenerosityOfKings (PlayerAction act) = do
  game <- act

  let gokPlayersPassed = game ^. round . generosityOfKingsState . playersPassed
      players'         = game ^. round . players

  -- Move to next phase if all players have passed
  if length gokPlayersPassed == length players'
    then endGenerosityOfKings game
    else cyclePlayersWithPasses game

endGenerosityOfKings :: Game -> Either GameError Game
endGenerosityOfKings game = do
  shadipinyiMod <- getShadipinyiMod
  divvyCattle <- mconcat <$> traverse getDivvyCattle (M.keys $ game ^. players)

  pure $ mconcat
    [ withClearedGenerosityOfKings
    , withReligionAndCulturePhase
    , shadipinyiMod
    , divvyCattle
    ]
 where
  players' = game ^. players
  -- N.B. This is destructive
  withClearedGenerosityOfKings =
    game & round . generosityOfKingsState .~ mempty

  gok = game ^. round . generosityOfKingsState
  withReligionAndCulturePhase =
    mempty & round . currentPhase .~ Just ReligionAndCulture

  nextRoundPlayerOrder = reverse (gok ^. playersPassed)

  empireMap =
    M.fromList (plaquesWithCattle (gok ^. cattlePool) (gok ^. plaques))

  getShadipinyiMod = case playerWithShadipinyi game of
    Nothing            -> Right mempty
    Just (playerId, _) -> case M.lookup ShadipinyiPlaque empireMap of
      Nothing -> internalError "Shadipinyi plaque not found"
      Just cattleAmount ->
        Right $ setPlayer playerId (mempty & cattle .~ cattleAmount)

  getDivvyCattle playerId = do
    player <- getPlayer playerId game
    case playerEmpire player of
      Nothing     -> internalError "Player not found."
      Just empire -> case M.lookup (PlayerPlaque empire) empireMap of
        Nothing -> internalError "Plaque not found"
        Just cattleAmount ->
          Right $ setPlayer playerId (mempty & cattle .~ cattleAmount)


plaquesWithCattle totalCattle plaques =
  let len       = length plaques

      allGet    = fromIntegral totalCattle `div` len

      remaining = fromIntegral totalCattle `mod` len
      addOneTo n xs = case (n, xs) of
        (0 , _            ) -> xs

        (_ , []           ) -> []

        (n0, (x, m) : rest) -> (x, m + 1) : addOneTo (n0 - 1) rest
  in  addOneTo remaining (zip plaques (repeat allGet))

placeStartingMonument :: Location -> PlayerId -> Game -> PlayerAction 'Setup
placeStartingMonument location playerId game = PlayerAction $ do
  playerIs playerId game
  phaseIs Setup game
  not isStartingMonument
    `impliesInvalid` (  "Cannot place monument at non-starting location: "
                     <> tshow location
                     )
  isTaken
    `impliesInvalid` (  "Cannot place monument at location: "
                     <> tshow location
                     <> " because it has already been taken."
                     )

  pure
    $  game
    <> setPlayer playerId withStartingMonument
    <> addVictoryPoints 1 playerId game
 where
  isStartingMonument = case (game ^. mapLayout) of
    Nothing -> False
    Just (MapLayout layout) ->
      M.lookup location layout == Just (Land StartingArea)
  otherPlayers = filter ((/= playerId) . fst) $ M.toList (game ^. players)
  otherPlayerMonumentLocations =
    concatMap (M.keys . playerMonuments . snd) otherPlayers
  isTaken              = location `elem` otherPlayerMonumentLocations

  withStartingMonument = mempty { playerMonuments = M.singleton location 1 }

-- * Generosity of Kings Phase

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
    <> setPlayer playerId modifiedPlayer
    <> (mempty & round . generosityOfKingsState .~ modifiedGenerosityOfKings)
 where
  playersPassedLens :: Lens' Game [PlayerId]
  playersPassedLens = round . generosityOfKingsState . playersPassed

  minimumBid :: Natural
  minimumBid = maybe 0 succ (game ^. round . generosityOfKingsState . lastBid)

  modifiedGenerosityOfKings :: GenerosityOfKingsState
  modifiedGenerosityOfKings = mempty
    { generosityOfKingsStateCattlePool = amount
    , generosityOfKingsStateLastBid    = Just amount
    }

  modifiedPlayer = mempty { playerCattle = (negate $ fromIntegral amount) }

pass :: PlayerId -> Game -> PlayerAction 'GenerosityOfKings
pass playerId game = PlayerAction $ do
  (playerId `elem` game ^. playersPassedLens)
    `impliesInvalid` "You have already passed."

  pure $ game & playersPassedLens %~ (playerId :)
 where
  playersPassedLens :: Lens' Game [PlayerId]
  playersPassedLens = round . generosityOfKingsState . playersPassed

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
