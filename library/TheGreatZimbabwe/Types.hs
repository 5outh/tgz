{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TheGreatZimbabwe.Types where

import           Data.Tuple                               ( swap )
import           Control.Applicative
import           Control.Lens
import           Data.Aeson                        hiding ( defaultOptions )
import           Data.Function                            ( on )
import qualified Data.Map.Strict               as M
import           Data.Monoid
import           Data.Maybe
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           GHC.Generics                      hiding ( to )
import           Numeric.Natural
import           Prelude                           hiding ( round )
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.Aeson
import           TheGreatZimbabwe.Text
import           TheGreatZimbabwe.Orphans                 ( )
import           Elm.Derive

newtype PlayerId = PlayerId { getPlayerId :: Integer }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSONKey, ToJSONKey)

deriveBoth (unPrefix "get") ''PlayerId

data Location = Location
  { locationX :: !Natural
  , locationY :: !Char
  } deriving (Show, Eq, Ord, Generic)

locationAbove :: Location -> Location
locationAbove loc = loc { locationY = pred (locationY loc) }

locationBelow :: Location -> Location
locationBelow loc = loc { locationY = succ (locationY loc) }

locationLeft :: Location -> Location
locationLeft loc = loc { locationX = pred (locationX loc) }

locationRight :: Location -> Location
locationRight loc = loc { locationX = succ (locationX loc) }

instance ToJSONKey Location where
  toJSONKey = ToJSONKeyValue toJSON toEncoding

instance FromJSONKey Location where
  fromJSONKey = FromJSONKeyValue parseJSON

deriveBoth (unPrefix "location") ''Location
makeLensesWith camelCaseFields ''Location

pprintLocation :: Location -> T.Text
pprintLocation (Location x y) = T.pack [y] <> tshow x

newtype Username = Username { usernameUsername :: T.Text }
  deriving (Generic, Show)

deriveBoth (unPrefix "username") ''Username
makeLensesWith camelCaseFields ''Username

data PlayerInfo = PlayerInfo
  { playerInfoUsername :: Username
  -- ^ A Player's unique username
  , playerInfoEmail    :: T.Text
  -- ^ A Player's unique email address
  } deriving (Generic, Show)

deriveBoth (unPrefix "playerInfo") ''PlayerInfo
makeLensesWith camelCaseFields ''PlayerInfo

data Empire
  = Kilwa -- Red
  | Mutapa -- Yellow
  | Zulu -- Green
  | Lozi -- Black
  | Mapungubwe -- White
  deriving (Show, Eq, Ord, Generic)

deriveBoth (unPrefix "empire") ''Empire

data EmpirePlaque = PlayerPlaque Empire | ShadipinyiPlaque
  deriving (Show, Eq, Ord)

deriveBoth defaultOptions ''EmpirePlaque

data Resource = Clay | Wood | Ivory | Diamonds
  deriving (Show, Eq, Ord, Generic)

instance FromJSONKey Resource where
  fromJSONKey = FromJSONKeyValue parseJSON

instance ToJSONKey Resource where
  toJSONKey = ToJSONKeyValue toJSON toEncoding

deriveBoth defaultOptions ''Resource

data Land
  = StartingArea
  | Resource Resource
  | BlankLand
    deriving (Show, Eq, Generic)

deriveBoth defaultOptions ''Land

data Square =
    Water
  | Land Land
    deriving (Show, Eq, Generic)

deriveBoth defaultOptions ''Square

newtype MapLayout = MapLayout { mapLayoutMapLayout :: M.Map Location Square }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

deriveBoth (unPrefix "mapLayout") ''MapLayout
makeLensesWith camelCaseFields ''MapLayout

data Craftsman
  -- * Primary
  = Potter
  | IvoryCarver
  | WoodCarver
  | DiamondCutter
  -- * Secondary
  | VesselMaker
  | ThroneMaker
  | Sculptor
    deriving (Eq, Ord, Show)

instance FromJSONKey Craftsman where
  fromJSONKey = FromJSONKeyValue parseJSON

instance ToJSONKey Craftsman where
  toJSONKey = ToJSONKeyValue toJSON toEncoding

deriveBoth defaultOptions ''Craftsman

-- | Rotated 90 degrees (or not)
data Rotated a = Rotated a | UnRotated a
  deriving (Show, Eq, Ord, Generic)

instance ToJSON a => ToJSON (Rotated a) where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (Rotated a) where
  parseJSON = genericParseJSON defaultOptions

-- | Resource a craftsman requires
craftsmanResource :: Craftsman -> Resource
craftsmanResource = \case
  Potter        -> Clay
  IvoryCarver   -> Ivory
  WoodCarver    -> Wood
  DiamondCutter -> Diamonds

  VesselMaker   -> Clay
  ThroneMaker   -> Wood
  Sculptor      -> Wood

-- | Associated Craftsman for tier 2 Craftsmen
craftsmanAssociatedCraftsman :: Craftsman -> Maybe Craftsman
craftsmanAssociatedCraftsman = \case
  VesselMaker -> Just Potter
  ThroneMaker -> Just IvoryCarver
  Sculptor    -> Just WoodCarver
  _           -> Nothing

-- | Dimensions of placed Craftsmen. Width x Height.
craftsmanDimensions :: Craftsman -> (Natural, Natural)
craftsmanDimensions = \case
  Potter        -> (1, 2)
  IvoryCarver   -> (2, 2)
  WoodCarver    -> (1, 2)
  DiamondCutter -> (2, 2)
  -- * Secondary
  VesselMaker   -> (2, 2)
  ThroneMaker   -> (1, 2)
  Sculptor      -> (2, 2)

rotatedDimensions :: Rotated Craftsman -> (Natural, Natural)
rotatedDimensions (UnRotated c) = craftsmanDimensions c
rotatedDimensions (Rotated   c) = swap (craftsmanDimensions c)

data PrimaryOrSecondary = Primary | Secondary

deriveBoth defaultOptions ''PrimaryOrSecondary

data TechnologyCard = TechnologyCard
  { technologyCardName               :: T.Text
  , technologyCardCraftsmanType      :: Craftsman
  , technologyCardVictoryRequirement :: Natural
  , technologyCardVictoryPoints      :: Natural
  , technologyCardCost               :: Natural
  } deriving (Eq, Ord, Generic, Show)

instance FromJSONKey TechnologyCard where
  fromJSONKey = FromJSONKeyValue parseJSON

instance ToJSONKey TechnologyCard where
  toJSONKey = ToJSONKeyValue toJSON toEncoding

deriveBoth (unPrefix "technologyCard") ''TechnologyCard
makeLensesWith camelCaseFields ''TechnologyCard

-- The 'Natural' is the number of cattle paid to the card
data Specialist
  = Shaman
  -- ^ May place one resource for 2 cattle (additional action type)
  | RainCeremony
  -- ^ May place one water tile for 3 cattle
  | Herd
  -- ^ May pay 2 cattle to this card to gain 1 cattle from the common stock.
  -- Use at most 3 times per turn.
  | Builder
  -- ^ Pay 2 cattle to activate this turn.
  -- If active, you pay the first two cattle of each newly placed craftsman to
  -- this card.
  | Nomads
  -- ^ Pay 2 cattle to ignore zoning restrictions when building a new monument
    deriving (Eq, Ord, Generic, Show)

instance FromJSONKey Specialist where
  fromJSONKey = FromJSONKeyValue parseJSON

instance ToJSONKey Specialist where
  toJSONKey = ToJSONKeyValue toJSON toEncoding

deriveBoth defaultOptions ''Specialist

specialistVR :: Specialist -> Natural
specialistVR = \case
  Shaman       -> 3
  Nomads       -> 1
  RainCeremony -> 1
  Builder      -> 2
  Herd         -> 6

allSpecialists :: S.Set Specialist
allSpecialists = S.fromList [Shaman, RainCeremony, Herd, Builder, Nomads]

data Activation = BuilderActive | NomadsActive | None
  deriving (Show, Eq, Ord)

deriveBoth defaultOptions ''Activation

data God
  = Shadipinyi
  -- ^ First plaque in Generosity of Kings
  | Elegua
  -- ^ Use up to 3 cattle from common stock on first bid in Generosity of Kings
  | Dziva
  -- ^ Can raise/lower prices at beginning of their turn
  | Eshu
  -- ^ Raise transportation range to 6
  | Gu
  -- ^ Makes all tech cards cost 1 (even retroactively)
  | Obatala
  -- ^ May place 2 monuments instead of 1 when placing monuments
  | Atete
  -- ^ May use each resource a second time
  | TsuiGoab
  -- ^ Does not need to use different resources to raise monumens
  | Anansi
  -- ^ Only pay 1 cattle per ritual good, regardless of price
  | Qamata Natural
  -- ^ Hub payment goes to Qamata card instead of common stock
  | Engai
  -- ^ Receives 2 additional cattle in each revenue phase
  | Xango
  -- ^ Victory Requirement - 2
    deriving (Generic, Show, Eq, Ord)

deriveBoth defaultOptions ''God

allGods :: [God]
allGods =
  [ Anansi
  , Atete
  , Dziva
  , Elegua
  , Engai
  , Eshu
  , Gu
  , Obatala
  , Qamata 0
  , Shadipinyi
  , TsuiGoab
  , Xango
  ]

-- Victory Requirement for a god
godVR :: God -> Int
godVR = \case
  Anansi     -> 5
  Atete      -> 5
  Dziva      -> 2
  Elegua     -> 4
  Engai      -> 5
  Eshu       -> 4
  Gu         -> 4
  Obatala    -> 7
  Qamata _   -> 2
  Shadipinyi -> 4
  TsuiGoab   -> 3
  Xango      -> (-2)

data Points = Points
  { pointsPoints :: Int
  , pointsStep :: Natural
  } deriving (Show, Eq, Ord)

deriveBoth (unPrefix "points") ''Points

instance Semigroup Points where
  p1 <> p2 = Points (pointsPoints p1 + pointsPoints p2) (pointsStep p2)

instance Monoid Points where
  mempty = Points 0 0

data Player = Player
  { playerInfo               :: Maybe PlayerInfo
  -- ^ Info about the human player
  , playerVictoryRequirement :: Points
  -- ^ Player's current victory requirement
  , playerVictoryPoints      :: Points
  -- ^ Player's current victory points
  , playerEmpire             :: Maybe Empire
  -- ^ The Empire the Player belongs to. Players have to choose this at the
  -- beginning of the game, so it's initially empty.
  , playerCattle             :: Int
  -- ^ Number of cattle that a Player currently has. Start amount: 3
  , playerMonuments          :: M.Map Location Natural
  -- ^ Locations of player-owned monuments on the map, along with monument height
  , playerCraftsmen          :: M.Map Location (Rotated Craftsman)
  -- ^ Locations of player-owned craftsmen on the map
  , playerTechnologyCards    :: M.Map TechnologyCard Int
  -- ^ Player-owned technology cards, along with the price other players must
  -- pay to use them
  , playerSpecialists        :: M.Map Specialist Int
  -- ^ Player-owned specialist cards
  , playerGod                :: Maybe God
  -- ^ God a player adores
  , playerActivations :: S.Set Activation
  -- ^ Activations a player has used this round.
  } deriving (Generic, Show)

instance Semigroup Player where
  p1 <> p2 = Player
    { playerInfo = on (<|>) playerInfo p1 p2
    , playerVictoryRequirement = on (<>) playerVictoryRequirement p1 p2
    , playerVictoryPoints = on (<>) playerVictoryPoints p1 p2
    , playerEmpire = on (<|>) playerEmpire p1 p2
    , playerCattle = on (+) playerCattle p1 p2
    , playerMonuments = on (M.unionWith (+)) playerMonuments p1 p2
    , playerCraftsmen = on (<>) playerCraftsmen p1 p2
    , playerTechnologyCards = on (M.unionWith (+)) playerTechnologyCards p1 p2
    , playerSpecialists = on (M.unionWith (+)) playerSpecialists p1 p2
    , playerGod = on (<|>) playerGod p1 p2
    , playerActivations = playerActivations p2
    }

instance Monoid Player where
  mempty = Player
    { playerInfo = Nothing
    , playerVictoryRequirement = mempty
    , playerVictoryPoints = mempty
    , playerEmpire = Nothing
    , playerCattle = 0
    , playerMonuments = mempty
    , playerCraftsmen = mempty
    , playerTechnologyCards = mempty
    , playerSpecialists = mempty
    , playerGod = Nothing
    , playerActivations = mempty
    }

deriveBoth (unPrefix "player") ''Player
makeLensesWith camelCaseFields ''Player

showPlayerUsername :: Player -> T.Text
showPlayerUsername player = fromMaybe
  "another player"
  (usernameUsername . playerInfoUsername <$> playerInfo player)

-- Note: UsedTwice only occurs when Atete is in play.
data UsedMarker = NotUsed | Used | UsedTwice
  deriving (Generic, Show)

deriveBoth defaultOptions ''UsedMarker

data GenerosityOfKingsState = GenerosityOfKingsState
  { generosityOfKingsStatePlaques       :: [EmpirePlaque]
  -- ^ Ordered plaques (static)
  , generosityOfKingsStateCattlePool    :: Natural
  -- ^ Total number of cattle that have been bid (this along with plaques
  -- is enough to reconstruct the view)
  , generosityOfKingsStateLastBid       :: Maybe Natural
  -- ^ The last bid made; next bid must be higher (or pass)
  , generosityOfKingsStatePlayersPassed :: [PlayerId]
  -- ^ Which players have passed, in what order?
  } deriving (Generic, Show)

instance Semigroup GenerosityOfKingsState where
  g1 <> g2 = GenerosityOfKingsState
    { generosityOfKingsStatePlaques = on (<>) generosityOfKingsStatePlaques g1 g2
    , generosityOfKingsStateCattlePool = on (+) generosityOfKingsStateCattlePool g1 g2
    , generosityOfKingsStateLastBid = on secondMay generosityOfKingsStateLastBid g1 g2
    , generosityOfKingsStatePlayersPassed = on (<>) generosityOfKingsStatePlayersPassed g1 g2
    }
   where
    secondMay Nothing Nothing = Nothing
    secondMay Nothing (Just n) = Just n
    secondMay (Just n) Nothing = Just n
    secondMay (Just n) (Just m) = Just m

instance Monoid GenerosityOfKingsState where
  mempty = GenerosityOfKingsState mempty 0 Nothing mempty

deriveBoth (unPrefix "generosityOfKingsState") ''GenerosityOfKingsState
makeLensesWith camelCaseFields ''GenerosityOfKingsState

data Phase
  = PreSetup
  | Setup
  | GenerosityOfKings
  | ReligionAndCulture
  | Revenues
  | LetUsCompareMythologies
    deriving (Show, Eq, Generic)

deriveBoth defaultOptions ''Phase

appendLast :: Maybe a -> Maybe a -> Maybe a
appendLast a b = if isJust b then b else a

data Round = Round
  { roundPlayers                :: [PlayerId]
  -- ^ Ordered list representing the order of play, determined by
  -- (a) VR in the generosity of kings phase, and
  -- (b) by the generosity of kings phase in other phases.
  , roundCurrentPlayer          :: Maybe PlayerId
  -- ^ The current player of the round
  , roundUsedMarkers            :: M.Map Location UsedMarker
  -- ^ Used marker locations on the map
  , roundGenerosityOfKingsState :: GenerosityOfKingsState
  -- ^ State used in the Generosity of kings phase
  , roundCurrentPhase           :: Maybe Phase
  -- Current phase of the round
  } deriving (Generic, Show)

instance Semigroup Round where
  r1 <> r2 = Round
    { roundPlayers = on (<>) roundPlayers r1 r2
    , roundCurrentPlayer = on appendLast roundCurrentPlayer r1 r2
    , roundUsedMarkers = on (<>) roundUsedMarkers r1 r2
    , roundGenerosityOfKingsState = on (<>) roundGenerosityOfKingsState r1 r2
    , roundCurrentPhase = on appendLast roundCurrentPhase r1 r2
    }

instance Monoid Round where
  mempty = Round mempty Nothing mempty mempty Nothing

deriveBoth (unPrefix "round") ''Round
makeLensesWith camelCaseFields ''Round

-- Note: unlimited cattle stock

data Game = Game
  { gamePlayers   :: M.Map PlayerId Player
  -- ^ Players of the game, in unordered format.
  , gameRound     :: Round
  -- ^ Current Round state
  , gameMapLayout :: MapLayout
  -- ^ Layout of the Map
  , gameCraftsmen :: M.Map Craftsman [TechnologyCard]
  -- ^ Remaining Craftsmen of each type
  , gameGods :: S.Set God
  -- ^ Remaining Gods
  , gameSpecialists :: S.Set Specialist
  -- ^ Remaining Specialists
  , gameWinner    :: Maybe PlayerId
  -- ^ PlayerId of the winner of the game, if any.
  , gameStep :: Natural
  -- ^ Current step of the game (cycles whenever current player cycles)
  , gameResourceTiles :: M.Map Resource Int
  -- ^ Remaining resource tiles in the game
  , gameWaterTiles :: Int
  -- ^ Remaining water tiles
  } deriving (Generic, Show)

instance Semigroup Game where
  g1 <> g2 = Game
    { gamePlayers = on (M.unionWith (<>)) gamePlayers g1 g2
    , gameRound = on (<>) gameRound g1 g2
    , gameMapLayout = on (<>) gameMapLayout g1 g2
    , gameCraftsmen = on (M.unionWith (<>)) gameCraftsmen g1 g2
    , gameGods = on (<>) gameGods g1 g2
    , gameSpecialists = on (<>) gameSpecialists g1 g2
    , gameWinner = on (<|>) gameWinner g1 g2
    , gameStep = on (+) gameStep g1 g2
    , gameResourceTiles = on (M.unionWith (+)) gameResourceTiles g1 g2
    , gameWaterTiles = on (+) gameWaterTiles g1 g2
    }

instance Monoid Game where
  mempty = Game mempty mempty mempty mempty mempty mempty Nothing 0 mempty 0

deriveBoth (unPrefix "game") ''Game
makeLensesWith camelCaseFields ''Game

-- * Pure functions from game to game

data PlayerAction (phase :: Phase) = PlayerAction { getPlayerAction :: Either GameError Game }
data GameEvent (phase :: Phase) = GameEvent { getGameEvent :: Either GameError Game }
