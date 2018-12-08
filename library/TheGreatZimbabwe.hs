{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds    #-}

module TheGreatZimbabwe where

import           Prelude                             hiding (round)

import           Control.Applicative
import           Control.Lens
import           Data.List
import           Data.List.NonEmpty                  (NonEmpty (..))
import qualified Data.Map.Strict                     as M
import           Data.Maybe
import qualified Data.Set                            as S
import           Data.Traversable
import           Debug.Trace
import           Numeric.Natural
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.Game
import           TheGreatZimbabwe.ReligionAndCulture
import           TheGreatZimbabwe.Text
import           TheGreatZimbabwe.Types
import           TheGreatZimbabwe.Types.GameCommand
import           TheGreatZimbabwe.Validation

-- TODO: How to handle game commands issued by the game itself? For example,
-- income after each phase should be recorded. (null player?)

-- Run a game command

runGameCommand :: Game -> PlayerId -> GameCommand -> Either GameError Game
runGameCommand game playerId = \case
  ChooseEmpire e -> handleFinishPreSetup (chooseEmpire e playerId game)
  PlaceStartingMonument location ->
    handleFinishSetup (placeStartingMonument location playerId game)
  Bid amount -> handleFinishGenerosityOfKings (bid amount playerId game)
  Pass       -> handleFinishGenerosityOfKings (pass playerId game)
  ReligionAndCultureCommand ReligionAndCultureMultiCommand {..} -> do
    game0 <-
      fmap (fromMaybe game)
      $ for religionAndCultureMultiCommandDziva
      $ \prices -> religionAndCultureAction playerId game
          $ setPrices (map (\(SetPrice a b) -> (a, b)) prices)

    game1 <-
      fmap (fromMaybe game0) $ for religionAndCultureMultiCommandAction1 $ \case
        ChooseGod god -> religionAndCultureAction playerId game (chooseGod god)
        ChooseSpecialist specialist ->
          religionAndCultureAction playerId game (chooseSpecialist specialist)

    game2 <-
      fmap (fromMaybe game1)
      $ for religionAndCultureMultiCommandAction2
      $ \action -> religionAndCultureAction playerId game0 $ case action of
          UseShaman       resource location -> useShaman resource location
          UseRainCeremony l1       l2       -> useRainCeremony l1 l2
          UseHerd n                         -> useHerd n
          UseBuilder                        -> useBuilder
          UseNomads                         -> useNomads

    game3 <-
      fmap (fromMaybe game2) $ for religionAndCultureMultiCommandAction3 $ \case
        BuildMonuments (location :| []) ->
          religionAndCultureAction playerId game2 $ buildMonument location
        BuildMonuments (location :| (x : _)) ->
          religionAndCultureAction playerId game2 $ buildMonuments [location, x]
        PlaceCraftsmen placements prices -> do
          newGame <- religionAndCultureAction playerId game2
            $ placeCraftsmen placements
          religionAndCultureAction playerId newGame
            $ raisePrices
                (map (\SetPrice {..} -> (setPricePrice, setPriceCraftsman))
                     prices
                )
        RaiseMonuments commands ->
          religionAndCultureAction playerId game2 $ raiseMonuments commands

    -- TODO: Need 'handleFinishReligionAndCulture'
    if religionAndCultureMultiCommandEnd then cyclePlayers game3 else pure game3

runGameCommands :: Game -> [(PlayerId, GameCommand)] -> Either GameError Game
runGameCommands game commands = foldl' go (Right game) commands
 where
  go
    :: Either GameError Game -> (PlayerId, GameCommand) -> Either GameError Game
  go eGame (playerId, command) = case eGame of
    Left  err   -> Left err
    Right game' -> runGameCommand game' playerId command

-- * Pre-Setup

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
    then game & round . currentPhase .~ Just Setup
    else game

-- * Setup

handleFinishSetup :: PlayerAction 'Setup -> Either GameError Game
handleFinishSetup (PlayerAction act) = do
  game <- act
  let players' = M.elems (game ^. players)

  cyclePlayers $ if (all (not . M.null) $ map playerMonuments players')
    then setupGenerosityOfKings game
    else game

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
  isStartingMonument =
    M.lookup location (mapLayoutMapLayout (game ^. mapLayout))
      == Just (Land StartingArea)
  otherPlayers = filter ((/= playerId) . fst) $ M.toList (game ^. players)
  otherPlayerMonumentLocations =
    concatMap (M.keys . playerMonuments . snd) otherPlayers
  isTaken              = location `elem` otherPlayerMonumentLocations

  withStartingMonument = mempty { playerMonuments = M.singleton location 1 }

-- * Generosity of Kings Phase

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

plaquesWithCattle :: Integral a => a -> [b] -> [(b, Int)]
plaquesWithCattle totalCattle plaques =
  let len       = length plaques

      allGet    = fromIntegral totalCattle `div` len

      remaining = fromIntegral totalCattle `mod` len
      addOneTo n xs = case (n, xs) of
        (0 , _            ) -> xs

        (_ , []           ) -> []

        (n0, (x, m) : rest) -> (x, m + 1) : addOneTo (n0 - 1) rest
  in  addOneTo remaining (zip plaques (repeat allGet))

bid :: Natural -> PlayerId -> Game -> PlayerAction 'GenerosityOfKings
bid amount playerId game = PlayerAction $ do
  playerIs playerId game

  -- TODO: Handle Elegua

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
