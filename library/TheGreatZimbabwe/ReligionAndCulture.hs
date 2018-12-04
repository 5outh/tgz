{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module TheGreatZimbabwe.ReligionAndCulture where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import qualified Data.Map.Strict             as M
import           Data.Maybe
import qualified Data.Set                    as S
import           Data.Text                   (Text)
import           Debug.Trace
import           Elm.Derive
import           GHC.Natural
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.Game
import           TheGreatZimbabwe.Text
import           TheGreatZimbabwe.Types
import           TheGreatZimbabwe.Validation

-- * Religion and Culture Phase

-- Either choose a god or choose a specialist. You may only perform
-- ONE of these two actions in a turn.
-- •  Use a specialist.
-- •  Either (1) build a monument, (2) place craftsmen, or (3) raise
-- monuments. You may only perform only ONE of these three actions in a turn

chooseGod :: God -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
chooseGod god playerId game = PlayerAction $ do
  god
    `S.notMember`    (game ^. gods)
    `impliesInvalid` (tshow god <> " has already been chosen by another player."
                     )
  player <- getPlayer playerId game

  isJust (playerGod player)
    `impliesInvalid` ("You already adore " <> tshow (playerGod player) <> ".")

  -- NB. destructive
  let withoutGod = game & gods %~ S.delete god

  pure
    $  withoutGod
    <> setPlayer playerId (mempty { playerGod = Just god })
    <> addVictoryRequirement (godVR god) playerId mempty

chooseSpecialist
  :: Specialist -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
chooseSpecialist specialist playerId game = PlayerAction $ do
  specialist
    `S.notMember`    (game ^. specialists)
    `impliesInvalid` (  tshow specialist
                     <> " has already been chosen by another player."
                     )
  player <- getPlayer playerId game

  -- NB. destructive
  let withoutSpecialist = game & specialists %~ S.delete specialist

  pure
    $  withoutSpecialist
    <> setPlayer playerId
                 (mempty { playerSpecialists = M.singleton specialist 0 })
    <> addVictoryRequirement (fromIntegral $ specialistVR specialist)
                             playerId
                             mempty

-- TODO: Resource tiles are limited.
useShaman
  :: Resource
  -> Location
  -> PlayerId
  -> Game
  -> PlayerAction 'ReligionAndCulture
useShaman resource location playerId game = undefined
 where
  updateLayout :: Game
  updateLayout = mempty & mapLayout .~ MapLayout
    (M.singleton location (Land (Resource resource)))

useRainCeremony
  :: Location
  -> Location
  -> PlayerId
  -> Game
  -> PlayerAction 'ReligionAndCulture
useRainCeremony = undefined

useHerd :: Natural -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
useHerd = undefined

useBuilder :: PlayerId -> Game -> PlayerAction 'ReligionAndCulture
useBuilder = undefined

useNomads :: PlayerId -> Game -> PlayerAction 'ReligionAndCulture
useNomads = undefined

executeIfEmpty
  :: (Either GameError Game)
  -> Text
  -> Location
  -> PlayerId
  -> Game
  -> Either GameError Game
executeIfEmpty executeAction message location playerId game = do
  playerIs playerId game
  phaseIs ReligionAndCulture game

  let mLocated = locatedAt location game
  let invalidMessage msg =
        invalidAction $ message <> " " <> pprintLocation location <> ": " <> msg

  -- woo! what a world
  case mLocated of
    Nothing      -> invalidMessage "it's off the board!"

    Just located -> case located of
      LocatedPlayerMonument player _ ->
        invalidMessage
          $  showPlayerUsername player
          <> " has already built a monument there!"
      LocatedPlayerCraftsman player _ ->
        invalidMessage
          $  showPlayerUsername player
          <> " has already built a craftsman there!"
      LocatedSquare square -> case square of
        Water     -> invalidMessage "can't build on water!"
        Land land -> case land of
          StartingArea -> invalidMessage "can't build on starting area!"
          Resource resource ->
            invalidMessage
              $  "can't build on resource tile: "
              <> tshow resource
              <> "!"
          BlankLand -> do
            player <- getPlayer playerId game
            if NomadsActive `S.member` (player ^. activations)
              then executeAction
              else case playerMonumentAround location game of
                Just (player, _) ->
                  invalidMessage
                    $ showPlayerUsername player
                    <> " has already built a monument too close to this location!"
                Nothing -> executeAction

buildMonument
  :: Location -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
buildMonument location playerId game = PlayerAction $ do
  let executeAction =
        pure
          $  game
          <> setPlayer playerId
                       (mempty { playerMonuments = M.singleton location 1 })
          <> addVictoryPoints 1 playerId game
  let message = "Can't build a new monument"
  executeIfEmpty executeAction message location playerId game

data Located
  = LocatedSquare Square
  | LocatedPlayerMonument Player Natural
  | LocatedPlayerCraftsman Player (Rotated Craftsman)

-- TODO: Technically there can be multiples here, do we care?
playerMonumentAround :: Location -> Game -> Maybe (Player, Natural)
playerMonumentAround location game = asum
  $ map (`lookupMonument` game) neighborLocations
 where
  neighborLocations =
    [ locationLeft location
    , locationRight location
    , locationAbove location
    , locationBelow location
    , locationLeft (locationAbove location)
    , locationRight (locationAbove location)
    , locationLeft (locationBelow location)
    , locationRight (locationBelow location)
    ]

locatedAt :: Location -> Game -> Maybe Located
locatedAt location game =
  let layout     = mapLayoutMapLayout $ (gameMapLayout game)
      players'   = M.elems (game ^. players)
      mSquare    = M.lookup location layout
      mMonument  = lookupMonument location game
      mCraftsman = lookupCraftsman location game
  in  asum
        [ uncurry LocatedPlayerMonument <$> mMonument
        , uncurry LocatedPlayerCraftsman <$> mCraftsman
        , LocatedSquare <$> mSquare
        ]

lookupMonument :: Location -> Game -> Maybe (Player, Natural)
lookupMonument location game =
  let players' = M.elems (game ^. players)
  in  listToMaybe . flip mapMaybe players' $ \player@(Player {..}) -> do
        monumentHeight <- M.lookup location playerMonuments
        pure (player, monumentHeight)


-- Lookup if a craftsman was built on the board, and who it belongs to if so.
--
-- a b
-- d *
--
-- Cases:
--  - Craftsman exists *at* location. cool, return it.
--- - Craftsman with dimensions (2, 2) built in a, b, or d
--  - Unrotated craftsman with dimensions (1, 2) built at b
--  - Rotated craftsman built at d
--
--
-- TODO: This implementation is really inefficient; we could cache the craftsman
-- at all four locations. But whatever for now.
--
lookupCraftsman :: Location -> Game -> Maybe (Player, Rotated Craftsman)
lookupCraftsman location game = asum
  (  map (lookupPlayerCraftsman location Nothing)   players'
  <> map (lookupPlayerCraftsman left (Just (2, 2))) players'
  <> map (lookupPlayerCraftsman top (Just (2, 2)))  players'
  <> map (lookupPlayerCraftsman top (Just (1, 2)))  players'
  <> map (lookupPlayerCraftsman left (Just (2, 1))) players'
  )
 where
  players' = M.elems (game ^. players)
  top      = locationAbove location
  left     = locationLeft location
  topLeft  = locationLeft (locationAbove location)

  lookupPlayerCraftsman
    :: Location
    -> Maybe (Natural, Natural)
    -> Player
    -> Maybe (Player, Rotated Craftsman)
  lookupPlayerCraftsman loc mDimension player@(Player {..}) = do
    craftsman <- M.lookup loc playerCraftsmen
    case mDimension of
      Nothing        -> pure (player, craftsman)
      Just dimension -> do
        guard (rotatedDimensions craftsman == dimension)
        pure (player, craftsman)


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
  = UseHub Location
  -- ^ Use a hub at some location, utilizing the resource at another location
  | UseCraftsman Location Location
  -- ^ Use a craftsman at some location, utilizing the resource at another location
  deriving (Show)

deriveBoth defaultOptions ''RaiseMonumentCommand

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
