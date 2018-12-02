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

  pure $ withoutGod <> setPlayer playerId (mempty { playerGod = Just god })

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

  pure $ withoutSpecialist <> setPlayer
    playerId
    (mempty { playerSpecialists = S.singleton specialist })

-- TODO: This is complicated
useSpecialist
  :: Specialist -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
useSpecialist specialist = case specialist of
  Shaman       -> useShaman
  RainCeremony -> useRainCeremony
  -- N.B. This number has two uses: to track the number of cattle on the card
  -- total, and the number a player is placing on the card during this action (max 2).
  Herd    n    -> useHerd n
  Builder _    -> useBuilder
  Nomads       -> useNomads

useShaman :: PlayerId -> Game -> PlayerAction 'ReligionAndCulture
useShaman = undefined

useRainCeremony :: PlayerId -> Game -> PlayerAction 'ReligionAndCulture
useRainCeremony = undefined

useHerd :: Natural -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
useHerd = undefined

useBuilder :: PlayerId -> Game -> PlayerAction 'ReligionAndCulture
useBuilder = undefined

useNomads :: PlayerId -> Game -> PlayerAction 'ReligionAndCulture
useNomads = undefined

buildMonument
  :: Location -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
buildMonument location playerId game = PlayerAction $ do
  --locationIsNotBlank
    --`impliesInvalid` ("Location " <> pprintLocation location <> " is not blank."
                     --)
  pure undefined

data Located
  = LocatedSquare Square
  | LocatedPlayerMonument Player Natural
  | LocatedPlayerCraftsman Player (Rotated Craftsman)

locatedAt :: Location -> Game -> Maybe Located
locatedAt location game =
  let layout = fromMaybe mempty $ mapLayoutMapLayout <$> (gameMapLayout game)
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
