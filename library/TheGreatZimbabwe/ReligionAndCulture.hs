{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module TheGreatZimbabwe.ReligionAndCulture where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import qualified Data.List                   as L
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
    `S.notMember` (game ^. gods)
    `impliesInvalid` (tshow god
                     <> " has already been chosen by another player or is unavailable in this game."
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
useShaman resource location playerId game =
  PlayerAction
    $ executeIfEmpty "Cannot build resource" location playerId game
    $ do
        playerIs playerId game
        phaseIs ReligionAndCulture game
        playerHasCattle 2 playerId game
        playerHasSpecialist Shaman playerId game

        case (M.lookup resource (game ^. resourceTiles)) of
          Nothing ->
            invalidAction "There are no resource tiles of that type left."
          Just n ->
            (n <= 0)
              `impliesInvalid` "There are no resource tiles of that type left."

        pure $ game <> updates
 where
  updateLayout :: Game
  updateLayout = mempty & mapLayout .~ MapLayout
    (M.singleton location (Land (Resource resource)))

  adjustResources :: Game
  adjustResources = mempty & resourceTiles .~ M.singleton resource (-1)

  updates =
    mconcat [updateLayout, paySpecialist Shaman 2 playerId, adjustResources]

paySpecialist :: Specialist -> Int -> PlayerId -> Game
paySpecialist specialist n playerId =
  mconcat
    $ [ setPlayer playerId (mempty & specialists .~ M.singleton specialist n)
      , setPlayer playerId (mempty & cattle -~ n)
      ]

useRainCeremony
  :: Location
  -> Location
  -> PlayerId
  -> Game
  -> PlayerAction 'ReligionAndCulture
useRainCeremony loc1 loc2 playerId game = PlayerAction $ do
  playerIs playerId game
  phaseIs ReligionAndCulture game
  playerHasSpecialist RainCeremony playerId game
  playerHasCattle 3 playerId game

  (game ^. waterTiles <= 0)
    `impliesInvalid` "There are no water tiles remaining."


  let convertLocationToWater :: Location -> Either GameError Game
      convertLocationToWater loc = executeIfEmpty
        "Cannot convert to water"
        loc
        playerId
        game
        (pure $ mempty & mapLayout .~ MapLayout (M.singleton loc Water))

  not (locationsAreAdjacent loc1 loc2)
    `impliesInvalid` "Water must be placed in adjacent locations when using rain ceremony."

  convert1 <- convertLocationToWater loc1
  convert2 <- convertLocationToWater loc2

  pure $ mconcat
    [ game
    , convert1
    , convert2
    , paySpecialist RainCeremony 2 playerId
    , mempty & waterTiles .~ (-1)
    ]

  -- Locations must be adjacent and both empty

locationsAreAdjacent :: Location -> Location -> Bool
locationsAreAdjacent (Location x y) loc2 = loc2 `elem` possibilities
 where
  possibilities =
    [ Location (pred x) y
    , Location (succ x) y
    , Location x        (pred y)
    , Location x        (succ y)
    ]

-- use herd N times (2 cattle on card, gain 1 per n)
useHerd :: Natural -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
useHerd n playerId game = PlayerAction $ do
  playerIs playerId game
  phaseIs ReligionAndCulture game
  playerHasCattle (n * 2) playerId game

  pure $ game <> mconcat (replicate (fromIntegral n) herd1)
  where herd1 = mconcat [paySpecialist Herd 2 playerId, addCattle 1 playerId]

useBuilder :: PlayerId -> Game -> PlayerAction 'ReligionAndCulture
useBuilder playerId game = PlayerAction $ do
  playerIs playerId game
  phaseIs ReligionAndCulture game
  playerHasCattle 2 playerId game
  playerHasSpecialist Builder playerId game

  pure $ mconcat
    [ game
    , setPlayer playerId (mempty & activations .~ S.singleton BuilderActive)
    ]

useNomads :: PlayerId -> Game -> PlayerAction 'ReligionAndCulture
useNomads playerId game = PlayerAction $ do
  playerIs playerId game
  phaseIs ReligionAndCulture game
  playerHasCattle 2 playerId game
  playerHasSpecialist Nomads playerId game

  pure $ mconcat
    [ game
    , setPlayer playerId (mempty & activations .~ S.singleton NomadsActive)
    ]

executeIfEmpty
  :: Text
  -> Location
  -> PlayerId
  -> Game
  -> Either GameError a
  -> Either GameError a
executeIfEmpty message location playerId game executeAction = do
  let mLocated = locatedAt location game
  let invalidMessage msg =
        invalidAction $ message <> " " <> pprintLocation location <> ": " <> msg

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
  executeIfEmpty message location playerId game executeAction

data Located
  = LocatedSquare Square
  | LocatedPlayerMonument Player Natural
  | LocatedPlayerCraftsman Player (Rotated Craftsman)

-- TODO: Technically there can be multiples here, but I don't really care yet?
playerMonumentAround :: Location -> Game -> Maybe (Player, Natural)
playerMonumentAround location game = asum
  $ map (`lookupMonument` game) (locationNeighbors8 location)
 where

locationNeighbors8 :: Location -> [Location]
locationNeighbors8 location =
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

-- TODO: This triggers a player taking a technology card if possible
-- TODO: Craftsmen have shapes, which must be validated. Location is top-left
-- of Craftsman tile
placeCraftsmen
  :: [(Location, Rotated Craftsman)]
  -- ^ Note: This can be empty just to trigger a 'raisePrices' command.
  -> PlayerId
  -> Game
  -> PlayerAction 'ReligionAndCulture
placeCraftsmen placements playerId game = PlayerAction $ do
  undefined

placeCraftsman
  :: Location -> Rotated Craftsman -> PlayerId -> Game -> Either GameError Game
placeCraftsman location rCraftsman playerId game = do
  let craftsman = getRotated rCraftsman
      locations = coveredLocations location rCraftsman
      isEmpty loc =
        executeIfEmpty "Cannot place craftsman" loc playerId game (pure ())

  playerHasCard <- playerHasCardOfType craftsman playerId game
  newGame       <- if playerHasCard
    then pure game
    else takeCheapestTechnologyCard craftsman playerId game

  -- Validate that all four locations are empty
  traverse_ isEmpty locations

  -- TODO: Validate that the craftsman is close enough to a resource of
  -- the required type *that is also* not in range of another craftsman of the
  -- same type. ooh baby, this sounds fun

  let updates =
        [ setPlayer playerId
                    (mempty & craftsmen .~ M.singleton location rCraftsman)
        ]

  -- Initially set the price of the craftsman to 1.
  -- Players can raise prices if they want using the 'raise-prices' command
  undefined

coveredLocations :: Location -> Rotated Craftsman -> [Location]
coveredLocations Location {..} rCraftsman =
  let (w, h) = rotatedDimensions rCraftsman
      xs     = case w of
        1 -> [locationX]
        2 -> [locationX, succ locationX]
        _ -> error "Dimensions are too large"
      ys = case h of
        1 -> [locationY]
        2 -> [locationY, succ locationY]
        _ -> error "Dimensions are too large"
  in  Location <$> xs <*> ys

-- TODO: only do this if player doesn't already have a technology card
takeCheapestTechnologyCard
  :: Craftsman -> PlayerId -> Game -> Either GameError Game
takeCheapestTechnologyCard craftsman playerId game = do
  case uncons =<< M.lookup craftsman (game ^. craftsmen) of
    Nothing -> invalidAction
      "There are no available technology cards of that type to take."
    Just (card, cards) -> do
      player <- getPlayer playerId game
      playerHasCattle (technologyCardCost card) playerId game

      let
        updates =
          [ subtractCattle (fromIntegral $ technologyCardCost card) playerId
          , addVictoryPoints (fromIntegral $ technologyCardVictoryPoints card)
                             playerId
                             game
          , addVictoryRequirement
            (fromIntegral $ technologyCardVictoryRequirement card)
            playerId
            game
          ]

      -- NB. This insert looks problematic, but 'cards' is the _rest_ of the
      -- cards after popping the first element, so we're actually deleting
      -- the card that just got taken here.
      pure $ (game & craftsmen %~ M.insert craftsman cards) <> mconcat updates

playerHasCardOfType :: Craftsman -> PlayerId -> Game -> Either GameError Bool
playerHasCardOfType craftsman playerId game = do
  player <- getPlayer playerId game
  pure $ isJust $ find
    (\playerCard -> technologyCardCraftsmanType playerCard == craftsman)
    (M.keys $ player ^. technologyCards)

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

-- DEPRECATED, but exploratory and cool

--reachableWithoutHubsInNSteps
  -- :: Natural -> Location -> MapLayout -> S.Set Location
--reachableWithoutHubsInNSteps = undefined

-- single step
--reachableWithoutHubs
  -- :: S.Set Location -> Location -> MapLayout -> S.Set Location
--reachableWithoutHubs visitedLocations location layout = S.singleton location
  -- <> mconcat (map (`fillWater` layout) neighbors)
 --where
  --neighbors =
    --filter (`S.notMember` visitedLocations) (locationNeighbors8 location)

  --fillWater :: Location -> MapLayout -> S.Set Location
  --fillWater loc (MapLayout layout) = go S.empty (S.singleton loc) [loc]
   --where
    --go visited waters (loc : stack) = case M.lookup loc layout of
      --Just Water ->
        --let
          --immediateNeighbors =
            --locationNeighbors4 loc L.\\ (S.toList (visited <> visitedLocations))
          --neighborSquares = mapMaybe
            --(\loc -> (loc, ) <$> M.lookup loc layout)
            --immediateNeighbors
          --neighborWaters = filter (\(_, s) -> s == Water) neighborSquares
        --in
          --go (S.insert loc visited)
             --(S.fromList (map fst neighborWaters) `S.union` waters)
             --(map fst neighborWaters ++ stack)
      --Nothing -> go visited waters stack
    --go _ waters [] = waters
