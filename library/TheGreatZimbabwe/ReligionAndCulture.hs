{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
module TheGreatZimbabwe.ReligionAndCulture where

import           Prelude                     hiding (round)

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.Function               (on)
import qualified Data.List                   as L
import qualified Data.Map.Strict             as M
import           Data.Maybe
import qualified Data.Set                    as S
import           Data.Text                   (Text)
import           Data.Traversable            (for)
import           Debug.Trace
import           Elm.Derive
import           GHC.Natural
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.Game
import           TheGreatZimbabwe.MapLayout
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

  issueRefundIfGu <- if god == Gu
    then refundTechnologyCards playerId game
    else pure mempty

  pure
    $  withoutGod
    <> setPlayer playerId (mempty { playerGod = Just god })
    <> issueRefundIfGu

refundTechnologyCards :: PlayerId -> Game -> Either GameError Game
refundTechnologyCards playerId game = do
  player <- getPlayer playerId game
  let currentTechnologyCards = M.keys (player ^. technologyCards)
      totalVR = fromIntegral . sum $ map technologyCardVictoryRequirement
                                         currentTechnologyCards
      refundedVR = totalVR

  pure $ addVictoryRequirement refundedVR playerId game

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

buildMonuments
  :: [Location] -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
buildMonuments locations playerId game = PlayerAction $ do
  player <- getPlayer playerId game
  (player ^. god /= Just Obatala)
    `impliesInvalid` "Only the player adoring Obatala can perform this action."
  (null locations) `impliesInvalid` "You must build at least one monument."
  (length locations > 2)
    `impliesInvalid` "You cannot build more than two monuments."

  foldM
    (\game0 location -> getPlayerAction (buildMonument location playerId game0))
    game
    locations

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
playerMonumentAround location game =
  asum $ map (`lookupMonument` game) (locationNeighbors8 location)

locatedAt :: Location -> Game -> Maybe Located
locatedAt location game =
  let layout     = mapLayoutMapLayout $ (gameMapLayout game)
      players'   = M.elems (game ^. players)
      mSquare    = M.lookup location layout
      mMonument  = lookupMonument location game
      mCraftsman = lookupCraftsman location game
  in  asum
        [ uncurry LocatedPlayerMonument <$> mMonument
        , uncurry LocatedPlayerCraftsman
          <$> (fmap (\(a, b, c) -> (b, c)) mCraftsman)
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
lookupCraftsman
  :: Location -> Game -> Maybe (Location, Player, Rotated Craftsman)
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
    -> Maybe (Location, Player, Rotated Craftsman)
  lookupPlayerCraftsman loc mDimension player@(Player {..}) = do
    craftsman <- M.lookup loc playerCraftsmen
    case mDimension of
      Nothing        -> pure (loc, player, craftsman)
      Just dimension -> do
        guard (rotatedDimensions craftsman == dimension)
        pure (loc, player, craftsman)

placeCraftsmen
  :: [(Location, Rotated Craftsman)]
  -- ^ Note: This can be empty just to trigger a 'raisePrices' command.
  -> PlayerId
  -> Game
  -> PlayerAction 'ReligionAndCulture
placeCraftsmen placements playerId game0 = PlayerAction $ do
  playerIs playerId game0
  phaseIs ReligionAndCulture game0

  go placements game0
 where
  go :: [(Location, Rotated Craftsman)] -> Game -> Either GameError Game
  go []                              game1 = pure game1
  go ((location, rCraftsman) : rest) game1 = do
    game2 <- placeCraftsman location rCraftsman playerId game1
    go rest game2

-- TODO: Craftsmen are limited (add them to the supply)
placeCraftsman
  :: Location -> Rotated Craftsman -> PlayerId -> Game -> Either GameError Game
placeCraftsman location rCraftsman playerId game0 = do
  let craftsman          = getRotated rCraftsman
      craftsmanLocations = coveredLocations location rCraftsman
      isEmpty loc =
        executeIfEmpty "Cannot place craftsman" loc playerId game0 (pure ())

  case M.lookup craftsman (game0 ^. craftsmanTiles) of
    Nothing ->
      internalError $ "Missing craftsman tile for type: " <> tshow craftsman
    Just n ->
      (n <= 0)
        `impliesInvalid` "There are no craftsman tiles of this type left."

  playerHasCard <- playerHasCardOfType craftsman playerId game0
  game          <- if playerHasCard
    then pure game0
    else takeCheapestTechnologyCard craftsman playerId game0

  -- Validate that all four locations are empty
  traverse_ isEmpty craftsmanLocations

  -- Validate that the craftsman is close enough to a resource of
  -- the required type *that is also* not in range of another craftsman of the
  -- same type.

  -- 1. get coordinates of other craftsmen of the same type
  badCoordinates <- findCraftsmanLocations craftsman game

  -- 2. get coordinates of required resources
  let MapLayout layout            = game ^. mapLayout
      requiredResourceCoordinates = M.keys
        $ M.filter (== Land (Resource (craftsmanResource craftsman))) layout

  -- 3. find all spaces 3 steps away from each required resource location.
  let mapGraph              = constructMapGraph (game ^. mapLayout)
      reachableLocationSets = flip map requiredResourceCoordinates
        $ \location -> mapConnectionsN 3 mapGraph (S.singleton location)

  -- 4. check, for each resource location:
  -- (a) no coordinates of craftsmen of the same type (badCoordinates) are in set
  -- (b) at least one coordinate of coveredLocations is in the set
  let
    validPlacement = or $ flip map reachableLocationSets $ \locationSet ->
      S.null (locationSet `S.intersection` badCoordinates)
        && not
             (S.null
               (locationSet `S.intersection` (S.fromList craftsmanLocations))
             )

  not validPlacement
    `impliesInvalid` "Craftsman is not close enough to an unclaimed, required resource."

  -- Validate secondary craftsman placement if applicable
  for (craftsmanAssociatedCraftsman craftsman) $ \associatedCraftsman -> do
    associatedCraftsmanLocations <- findCraftsmanLocations associatedCraftsman
                                                           game

    S.null associatedCraftsmanLocations `impliesInvalid` mconcat
      [ "None of the associated primary craftsmen ("
      , tshow associatedCraftsman
      , ") have been built yet."
      ]

    reachableLocations <- reachableLocationsUsingOneHub
      mapGraph
      (S.fromList craftsmanLocations)
      game
    S.null (reachableLocations `S.intersection` associatedCraftsmanLocations)
      `impliesInvalid` "Secondary craftsmen must be placed within range of primary craftsman (using hubs)."

  let updates =
        [ setPlayer playerId
                    (mempty & craftsmen .~ M.singleton location rCraftsman)
        , mempty & craftsmanTiles .~ M.singleton craftsman (-1)
        ]

  -- Initially set the price of the craftsman to 1.
  -- Players can raise prices if they want using the 'raise-prices' command
  pure $ game <> (traceShowId $ mconcat updates)

reachableLocationsUsingOneHub
  :: MapGraph -> S.Set Location -> Game -> Either GameError (S.Set Location)
reachableLocationsUsingOneHub mapGraph startingLocations game = do
  allPlayers <- getPlayers game
  let
    reachableLocations = mapConnectionsN 3 mapGraph startingLocations
    allHubLocations =
      S.fromList . M.keys . mconcat $ map playerMonuments allPlayers
    reachableHubLocations = allHubLocations `S.intersection` reachableLocations
    reachableByOneHub     = mapConnectionsN 3 mapGraph startingLocations

  pure $ reachableByOneHub <> reachableLocations

findCraftsmanLocations :: Craftsman -> Game -> Either GameError (S.Set Location)
findCraftsmanLocations craftsman game = do
  players <- getPlayers game
  let allCraftsmenOnBoard = M.unions (map playerCraftsmen players)
      craftsmenOfSameType = M.filter
        (\otherCraftsman -> getRotated otherCraftsman == craftsman)
        allCraftsmenOnBoard
  pure $ S.fromList . concatMap (uncurry coveredLocations) $ M.toList
    craftsmenOfSameType

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

takeCheapestTechnologyCard
  :: Craftsman -> PlayerId -> Game -> Either GameError Game
takeCheapestTechnologyCard craftsman playerId game = do
  case uncons =<< M.lookup craftsman (game ^. technologyCards) of
    Nothing -> invalidAction
      "There are no available technology cards of that type to take."
    Just (card, cards) -> do
      player <- getPlayer playerId game
      playerHasCattle (technologyCardCost card) playerId game

      let cardVR = if player ^. god == Just Gu
            then 1
            else fromIntegral (technologyCardVictoryRequirement card)
          updates =
            [ subtractCattle (fromIntegral $ technologyCardCost card) playerId
            , addVictoryPoints
              (fromIntegral $ technologyCardVictoryPoints card)
              playerId
              game
            , addVictoryRequirement cardVR playerId game
            , setPlayer
              playerId
              (mempty & technologyCards .~ M.singleton
                card
                (mempty { technologyCardStatePrice = 1 })
              )
            ]

      -- NB. This insert looks problematic, but 'cards' is the _rest_ of the
      -- cards after popping the first element, so we're actually deleting
      -- the card that just got taken here.
      pure
        $  (game & technologyCards %~ M.insert craftsman cards)
        <> mconcat updates

playerHasCardOfType :: Craftsman -> PlayerId -> Game -> Either GameError Bool
playerHasCardOfType craftsman playerId game = do
  player <- getPlayer playerId game
  pure $ isJust $ find
    (\playerCard -> technologyCardCraftsmanType playerCard == craftsman)
    (M.keys $ player ^. technologyCards)

setPrices
  :: [(Int, Craftsman)] -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
setPrices prices playerId game = PlayerAction $ do
  playerIs playerId game
  phaseIs ReligionAndCulture game

  foldM (\game0 (price, craftsman) -> setPrice craftsman price playerId game0)
        game
        prices

raisePrices
  :: [(Int, Craftsman)] -> PlayerId -> Game -> PlayerAction 'ReligionAndCulture
raisePrices prices playerId game = PlayerAction $ do
  playerIs playerId game
  phaseIs ReligionAndCulture game

  foldM
    (\game0 (price, craftsman) -> raisePrice craftsman price playerId game0)
    game
    prices

-- Raise the price of a technology card. Only possible at beginning of turn
-- as Dziva.
setPrice :: Craftsman -> Int -> PlayerId -> Game -> Either GameError Game
setPrice craftsman newPrice playerId game = do
  (newPrice > 3 || newPrice < 1)
    `impliesInvalid` "Price must be between 1 and 3."

  player <- getPlayer playerId game
  (playerGod player /= Just Dziva)
    `impliesInvalid` "You must adore Dziva to perform the set-price action."

  let availableTechnologyCards = M.toList (player ^. technologyCards)
      mTechnologyCard          = find
        ((== craftsman) . technologyCardCraftsmanType . fst)
        availableTechnologyCards

  case mTechnologyCard of
    Nothing -> invalidAction $ mconcat
      [ "Cannot raise price of a "
      , tshow craftsman
      , ": you do not have the necessary Technology Card."
      ]

    Just (card, cardState) -> do
      pure $ game <> setPlayer
        playerId
        (mempty & technologyCards .~ M.singleton card
                                                 (mempty & price .~ newPrice)
        )

-- Raise the price of a technology card. Only possible after placing craftsmen.
raisePrice :: Craftsman -> Int -> PlayerId -> Game -> Either GameError Game
raisePrice craftsman newPrice playerId game = do
  (newPrice > 3 || newPrice < 1)
    `impliesInvalid` "Price must be between 1 and 3."

  player <- getPlayer playerId game

  let availableTechnologyCards = M.toList (player ^. technologyCards)
      mTechnologyCard          = find
        ((== craftsman) . technologyCardCraftsmanType . fst)
        availableTechnologyCards

  case mTechnologyCard of
    Nothing -> invalidAction $ mconcat
      [ "Cannot raise price of a "
      , tshow craftsman
      , ": you do not have the necessary Technology Card."
      ]

    Just (card, TechnologyCardState {..}) -> do
      (fromIntegral newPrice <= technologyCardStatePrice)
        `impliesInvalid` "Price must increase."
      pure $ game <> setPlayer
        playerId
        (mempty & technologyCards .~ M.singleton
          card
          (mempty & price .~ fromIntegral newPrice)
        )

data RaiseMonumentCommand
  = UseHub Location
  -- ^ Use a hub at some location, utilizing the resource at another location
  | UseCraftsman Location Location
  -- ^ Use a craftsman at some location, utilizing the resource at another location
  deriving (Show)

deriveBoth defaultOptions ''RaiseMonumentCommand

data RaiseMonumentState = RaiseMonumentState
  { raiseMonumentStateCurrentLocation :: Maybe Location
  -- ^ The player's "current location"
  , raiseMonumentStateRitualGoodHeld  :: Maybe Craftsman
  -- ^ You either have nothing yet, or a ritual good from a craftsman (primary or secondary).
  , raiseMonumentStateResourcesUsed   :: S.Set Resource
  -- ^ Track the types of Resource already used *to raise a monument* this turn.
  -- NB. the bolded statement is important because you can use resources to pay
  -- primary craftsmen on the way to a secondary. that doesn't count against you
  -- here.
  }

makeLensesWith camelCaseFields 'RaiseMonumentState

instance Semigroup RaiseMonumentState where
  a <> b = RaiseMonumentState
    { raiseMonumentStateCurrentLocation =
        lastMay (raiseMonumentStateCurrentLocation a) (raiseMonumentStateCurrentLocation b)
    , raiseMonumentStateRitualGoodHeld = lastMay (raiseMonumentStateRitualGoodHeld a)
        (raiseMonumentStateRitualGoodHeld b)
    , raiseMonumentStateResourcesUsed = on (<>) raiseMonumentStateResourcesUsed a b
    }
   where
    lastMay a b = case (a, b) of
          (Nothing, Nothing) -> Nothing
          (Just a, Nothing)  -> Just a
          (Nothing, Just b)  -> Just b
          (Just a, Just b)   -> Just b

instance Monoid RaiseMonumentState where
  mempty = RaiseMonumentState Nothing Nothing S.empty

raiseMonumentForLocation location playerId mapGraph game commands = do
  player <- getPlayer playerId game
  let mMonumentHeight = M.lookup location (playerMonuments player)
  case mMonumentHeight of
    Nothing ->
      invalidAction
        $  "You must build a monument at "
        <> pprintLocation location
        <> " before raising it."
    Just currentHeight -> do
      let desiredHeight = (fromIntegral currentHeight + 1)
      -- If this completes, it worked.
      (game0, commands0) <- go desiredHeight game commands

      let heightVP = \case
            1 -> 1
            2 -> 2
            3 -> 4
            4 -> 6
            5 -> 8
          updates =
            [ setPlayer playerId (mempty & monuments .~ M.singleton location 1)
            , addVictoryPoints (heightVP desiredHeight) playerId game0
            ]

      -- if there are additional commands, just do it again
      case commands0 of
        []                -> pure (game0 <> mconcat updates)
        remainingCommands -> raiseMonumentForLocation location
                                                      playerId
                                                      mapGraph
                                                      game0
                                                      remainingCommands
 where
  -- nb. there is a case where we raise a monument more than once, so retain the remaining commands
  go
    :: Int
    -> Game
    -> [RaiseMonumentCommand]
    -> Either GameError (Game, [RaiseMonumentCommand])
  go 0      game0 commands0 = pure (game0, commands0)
  go height game0 commands0 = do
    (game1, additionalCommands) <- raiseMonument1
      playerId
      mapGraph
      (mempty { raiseMonumentStateCurrentLocation = Just location })
      game0
      commands0
    go (height - 1) game1 commands

-- Fetch one level of ritual goods.
raiseMonument1 playerId mapGraph state@(RaiseMonumentState {..}) game = \case
  -- using a craftsman can trigger a location reset, i.e. end this.
  (UseCraftsman craftsmanLocation resourceLocation : commands) -> do
    player <- getPlayer playerId game
    let transportationRange = if player ^. god == Just Eshu then 6 else 3
        -- TODO: do we kick off from the location the user chose, or allow
        -- branching off from all covered locations? former is easier.
        --
        -- The craftsman we're trying to use - one of the covered locations must
        -- be within range.
        mCraftsmanInUse     = lookupCraftsman craftsmanLocation game

    case mCraftsmanInUse of
      Nothing -> invalidAction
        (  "There is no craftsman at location "
        <> pprintLocation craftsmanLocation
        <> "."
        )
      Just (hookLocation, owner, rotatedCraftsman) -> do
        let craftsmanLocations =
              S.fromList $ coveredLocations hookLocation rotatedCraftsman
            reachableFromCraftsman =
              mapConnectionsN transportationRange mapGraph (craftsmanLocations)
            theCraftsman = getRotated rotatedCraftsman
            theResource  = craftsmanResource theCraftsman
            theGod       = playerGod player

        (technologyCard, technologyCardState) <-
          fromMaybeError "Technology Card for Craftsman not found in owner"
          $ find
              (\(card, _) -> technologyCardCraftsmanType card == theCraftsman)
          $ M.toList (owner ^. technologyCards)

        let costToUseCraftsman = technologyCardStatePrice technologyCardState

        -- can the player reach the craftsman?
        currentLocation <- fromMaybeError
          "Current location should not be empty"
          raiseMonumentStateCurrentLocation

        (currentLocation `S.notMember` reachableFromCraftsman)
          `impliesInvalid` (  "You cannot reach the craftsman at "
                           <> pprintLocation craftsmanLocation
                           <> "."
                           )

        -- can the player reach the required resource, starting at the craftsman?

        (resourceLocation `S.notMember` reachableFromCraftsman)
          `impliesInvalid` (  "You cannot reach the required resource at "
                           <> pprintLocation resourceLocation
                           <> " (starting from the "
                           <> tshow theCraftsman
                           <> ")."
                           )

        -- can the player validly use this craftsman this turn (have they used before, and are not tsui-goab?)
        (          theResource
          `S.member` raiseMonumentStateResourcesUsed
          &&         theGod
          /=         Just TsuiGoab
          )
          `impliesInvalid` (  "You have already used "
                           <> tshow theResource
                           <> " to raise this monument this turn."
                           )

        -- does the player have enough money to pay for the ritual good?
        playerHasCattle (fromIntegral costToUseCraftsman) playerId game

        -- has the resource already been used (a used marker is there, and not Atete/used twice?)?
        let usedMarkers' = game ^. round . usedMarkers
            threshold    = if player ^. god == Just Atete then 2 else 1

        (fromMaybe 0 (M.lookup resourceLocation usedMarkers') >= threshold)
          `impliesError` "Cannot use that resource: it has already been used this round."

        -- if all are fine, add a used marker to resource and update state

        ownerId <-
          fromMaybeError "Cannot find owner"
          $   playerInfoPlayerId
          <$> (owner ^. info)

        let
          gameUpdates =
            [ mempty
              &  round
              .  usedMarkers
              .~ M.singleton resourceLocation 1
            -- Add cost to use craftsman to tech card
            , setPlayer
              ownerId
              (mempty & technologyCards .~ M.singleton
                technologyCard
                (mempty & cattle .~ costToUseCraftsman)
              )
            -- spend cattle
            , subtractCattle costToUseCraftsman playerId
            ]
          stateUpdates =
            [ mempty
              { raiseMonumentStateCurrentLocation = Just craftsmanLocation
              }
            , mempty & ritualGoodHeld .~ Just theCraftsman
            , mempty & resourcesUsed .~ S.singleton theResource
            ]

        let associatedPrimaryCraftsman =
              craftsmanAssociatedCraftsman theCraftsman
        doesSecondaryCraftsmanExist <- secondaryCraftsmanExists theCraftsman
                                                                game

        -- is it finished:
        --  - primary craftsman => secondary does not exist on board yet
        --  - secondary craftsman => holding primary ritual good
        if (  not doesSecondaryCraftsmanExist
           && isNothing associatedPrimaryCraftsman
           )
        then
          case commands of
            [] -> pure (game <> mconcat gameUpdates, [])
            _  -> pure (game <> mconcat gameUpdates, commands)
        else
          do
            -- if this *is* the secondary craftsman, validate that the primary
            -- ritual good is held
            case associatedPrimaryCraftsman of
              Just primary -> do
                (raiseMonumentStateRitualGoodHeld /= Just primary)
                  `impliesInvalid` "You have not obtained the necessary resources to pay this secondary craftsman."
                pure (game <> mconcat gameUpdates, commands)
                -- not primary, shouldn't be on the board if the secondary craftsman doesn't exist
              Nothing ->
                internalError
                  "Searching for primary craftsman, but secondary craftsman doesn't exist"

            raiseMonument1 playerId
                           mapGraph
                           (state <> mconcat stateUpdates)
                           (game <> mconcat gameUpdates)
                           commands

  (UseHub hubLocation : commands) -> do
    player <- getPlayer playerId game
    playerHasCattle 1 playerId game

    currentLocation <- fromMaybeError "Current location should not be empty"
                                      raiseMonumentStateCurrentLocation

    let transportationRange = if player ^. god == Just Eshu then 6 else 3
        reachable           = mapConnectionsN transportationRange
                                              mapGraph
                                              (S.singleton currentLocation)

    (hubLocation `S.notMember` reachable)
      `impliesInvalid` "You are not within range of that hub."

    payHub <- payForHub playerId game

    let newGame = game <> payHub
        newState =
          state { raiseMonumentStateCurrentLocation = Just hubLocation }

    raiseMonument1 playerId mapGraph newState newGame commands

  [] -> invalidAction "Ran out of commands without raising a monument."

-- pay qamata for hub instead of common stock if she exists
payForHub :: PlayerId -> Game -> Either GameError Game
payForHub playerId game = do
  players <- getPlayers game
  let mPlayerWithQamata = find
        (\p -> case playerGod p of
          Just (Qamata _) -> True
          _               -> False
        )
        players
  case mPlayerWithQamata of
    Nothing -> pure $ subtractCattle 1 playerId
    Just player ->
      let
        -- this is safe, from above
          Just (Qamata qamataCattle) = playerGod player
      in  case playerInfo player of
            Nothing -> internalError "Cannot find playerId for player"
            Just PlayerInfo {..} -> pure $ mconcat
              [ setPlayer playerId $ mempty & god .~ Just
                (Qamata (succ qamataCattle))
              , subtractCattle 1 playerId
              ]


-- | Returns true if the secondary craftsman exists for the given craftsman
--
-- Returns 'False' if a secondary craftsman is passed in.
--
secondaryCraftsmanExists :: Craftsman -> Game -> Either GameError Bool
secondaryCraftsmanExists craftsman game = do
  players    <- getPlayers game
  mLocations <-
    for (craftsmanSecondaryCraftsman craftsman) $ \associatedCraftsman -> do
      findCraftsmanLocations associatedCraftsman game
  pure $ S.null (fromMaybe S.empty mLocations)

raiseMonuments
  :: [(Location, [RaiseMonumentCommand])]
  -- ^ For each location user wants to raise, a sequence of commands done in
  -- order to raise it.
  -> PlayerId
  -> Game
  -> PlayerAction 'ReligionAndCulture
raiseMonuments raiseMonumentCommands playerId game = PlayerAction $ do
  player <- getPlayer playerId game

  let monumentHeightAt location = M.lookup location (player ^. monuments)
      commandCounts   = map (over _2 (const 1)) raiseMonumentCommands
      commandCountMap = M.fromListWith (+) commandCounts

  foldM
    (\game0 (location, commands) ->
      raiseMonumentForLocation location playerId mapGraph game0 commands
    )
    game
    raiseMonumentCommands
 where
  mkState location =
    mempty { raiseMonumentStateCurrentLocation = Just location }
  mapGraph = constructMapGraph (game ^. mapLayout)
