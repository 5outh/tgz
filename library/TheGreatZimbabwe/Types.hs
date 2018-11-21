{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE StandaloneDeriving              #-}
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

import           Control.Lens
import           Control.Lens.TH
import           Data.Aeson
import qualified Data.Aeson             as Aeson
import           Data.Function          (on)
import           Data.List
import qualified Data.Map.Strict        as M
import           Data.Maybe             (isJust)
import           Data.Monoid
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Data.Validation
import           Database.Persist       (Entity (..))
import           GHC.Generics
import           Numeric.Natural
import           Prelude                hiding (round)
import           TheGreatZimbabwe.Error

-- Orphans :X
deriving newtype instance FromJSON a => FromJSON (Alt Maybe a)
deriving newtype instance ToJSON a => ToJSON (Alt Maybe a)
deriving newtype instance FromJSON a => FromJSON (Sum a)
deriving newtype instance ToJSON a => ToJSON (Sum a)

newtype Merge k v = Merge { getMerge :: M.Map k v }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

type instance Index (Merge k v) = k
type instance IxValue (Merge k v) = v
instance Ord k => Ixed (Merge k v) where
  ix k f m = Merge <$> ix k f (getMerge m)
  {-# INLINE ix #-}

instance Ord k => At (Merge k v) where
  at k f m = Merge <$> at k f (getMerge m)
  {-# INLINE at #-}

instance (Ord k, Semigroup v) => Semigroup (Merge k v) where
  Merge xs <> Merge ys = Merge $ M.unionWith (<>) xs ys

instance (Ord k, Semigroup v) => Monoid (Merge k v) where
  mempty = Merge (M.empty)
  mappend = (<>)

newtype PlayerId = PlayerId { getPlayerId :: Integer }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey)

data Location = Location
  { locationX :: !Natural
  , locationY :: !Char
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON Location where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance ToJSONKey Location where
  toJSONKey = ToJSONKeyValue toJSON toEncoding

instance FromJSON Location where
  parseJSON = genericParseJSON defaultOptions

instance FromJSONKey Location where
  fromJSONKey = FromJSONKeyValue parseJSON

makeLensesWith camelCaseFields ''Location

newtype Username = Username { usernameUsername :: T.Text }
  deriving newtype (ToJSON, FromJSON)

makeLensesWith camelCaseFields ''Username

data PlayerInfo = PlayerInfo
  { playerInfoUsername :: Username
  -- ^ A Player's unique username
  , playerInfoEmail    :: T.Text
  -- ^ A Player's unique email address
  } deriving (Generic)

instance FromJSON PlayerInfo where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON PlayerInfo where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

makeLensesWith camelCaseFields ''PlayerInfo

data Empire
  = Kilwa -- Red
  | Mutapa -- Yellow
  | Zulu -- Green
  | Lozi -- Black
  | Mapungubwe -- White
  deriving (Show, Eq, Generic)

instance ToJSON Empire where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Empire where
  parseJSON = genericParseJSON defaultOptions

data Resource = Clay | Wood | Ivory | Diamonds
  deriving (Show, Eq, Generic)

instance ToJSON Resource where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Resource where
  parseJSON = genericParseJSON defaultOptions

data Land
  = StartingArea
  | Resource Resource
  | BlankLand
    deriving (Show, Eq, Generic)

instance ToJSON Land where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Land where
  parseJSON = genericParseJSON defaultOptions

data Square =
    Water
  | Land Land
    deriving (Show, Eq, Generic)

instance ToJSON Square where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Square where
  parseJSON = genericParseJSON defaultOptions

newtype MapLayout = MapLayout { mapLayoutMapLayout :: M.Map Location Square }
  deriving stock (Show, Eq)
  deriving newtype (ToJSON, FromJSON)

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
    deriving (Eq, Ord, Generic)

instance FromJSONKey Craftsman where
  fromJSONKey = FromJSONKeyValue parseJSON

instance ToJSONKey Craftsman where
  toJSONKey = ToJSONKeyValue toJSON toEncoding

instance FromJSON Craftsman where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Craftsman where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

data PrimaryOrSecondary = Primary | Secondary

data TechnologyCard = TechnologyCard
  { technologyCardName               :: T.Text
  , technologyCardCraftsmanType      :: Craftsman
  , technologyCardVictoryRequirement :: Natural
  , technologyCardVictoryPoints      :: Natural
  , technologyCardCost               :: Natural
  } deriving (Eq, Ord, Generic)

instance FromJSON TechnologyCard where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON TechnologyCard where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSONKey TechnologyCard where
  fromJSONKey = FromJSONKeyValue parseJSON

instance ToJSONKey TechnologyCard where
  toJSONKey = ToJSONKeyValue toJSON toEncoding

makeLensesWith camelCaseFields ''TechnologyCard

data Specialist
  = Shaman
  -- ^ May place one resource for 2 cattle (additional action type)
  | RainCeremony
  -- ^ May place one water tile for 3 cattle
  | Herd Natural
  -- ^ May pay 2 cattle to this card to gain 1 cattle from the common stock.
  -- Use at most 3 times per turn.
  | Builder Natural
  -- ^ Pay 2 cattle to activate this turn.
  -- If active, you pay the first two cattle of each newly placed craftsman to
  -- this card.
  | Nomads
  -- ^ Pay 2 cattle to ignore zoning restrictions when building a new monument
    deriving (Eq, Ord, Generic)

instance FromJSON Specialist where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Specialist where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

specialistVR :: Specialist -> Natural
specialistVR = \case
  Shaman       -> 3
  Nomads       -> 1
  RainCeremony -> 1
  Builder _    -> 2
  Herd    _    -> 6

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
    deriving (Generic)

instance FromJSON God where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON God where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

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

-- Little idea: this could be encoded as a Monoid and state changes could be
-- encoded as mempty{ change }. could the whole game be done that way?
data Player = Player
  { playerInfo               :: Alt Maybe PlayerInfo
  -- ^ Info about the human player
  , playerVictoryRequirement :: Sum Int
  -- ^ Player's current victory requirement
  , playerVictoryPoints      :: Sum Int
  -- ^ Player's current victory points
  , playerEmpire             :: Alt Maybe Empire
  -- ^ The Empire the Player belongs to. Players have to choose this at the
  -- beginning of the game, so it's initially empty.
  , playerCattle             :: Sum Int
  -- ^ Number of cattle that a Player currently has. Start amount: 3
  , playerMonuments          :: Merge Location (Sum Natural)
  -- ^ Locations of player-owned monuments on the map, along with monument height
  , playerCraftsmen          :: M.Map Location Craftsman
  -- ^ Locations of player-owned craftsmen on the map (each individual owned square)
  , playerTechnologyCards    :: Merge TechnologyCard (Sum Int)
  -- ^ Player-owned technology cards, along with the price other players must
  -- pay to use them
  , playerSpecialists        :: S.Set Specialist
  -- ^ Player-owned specialist cards
  , playerGod                :: Alt Maybe God
  -- ^ God a player adores
  } deriving (Generic)

instance FromJSON Player where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Player where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance Semigroup Player where
  p1 <> p2 = Player
    { playerInfo = on (<>) playerInfo p1 p2
    , playerVictoryRequirement = on (<>) playerVictoryRequirement p1 p2
    , playerVictoryPoints = on (<>) playerVictoryPoints p1 p2
    , playerEmpire = on (<>) playerEmpire p1 p2
    , playerCattle = on (<>) playerCattle p1 p2
    , playerMonuments = on (<>) playerMonuments p1 p2
    , playerCraftsmen = on (<>) playerCraftsmen p1 p2
    , playerTechnologyCards = on (<>) playerTechnologyCards p1 p2
    , playerSpecialists = on (<>) playerSpecialists p1 p2
    , playerGod = on (<>) playerGod p1 p2
    }

instance Monoid Player where
  mempty = Player mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

makeLensesWith camelCaseFields ''Player

-- Note: UsedTwice only occurs when Atete is in play.
data UsedMarker = NotUsed | Used | UsedTwice
  deriving (Generic)

instance ToJSON UsedMarker where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON UsedMarker where
  parseJSON = genericParseJSON defaultOptions

data GenerosityOfKingsState = GenerosityOfKingsState
  { generosityOfKingsStatePlaques       :: [Empire]
  -- ^ Ordered plaques (static)
  , generosityOfKingsStateCattlePool    :: Sum Natural
  -- ^ Total number of cattle that have been bid (this along with plaques
  -- is enough to reconstruct the view)
  , generosityOfKingsStateLastBid       :: Last Natural
  -- ^ The last bid made; next bid must be higher (or pass)
  , generosityOfKingsStatePlayersPassed :: [PlayerId]
  -- ^ Which players have passed, in what order?
  } deriving (Generic)

instance ToJSON GenerosityOfKingsState where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON GenerosityOfKingsState where
  parseJSON = genericParseJSON defaultOptions

instance Semigroup GenerosityOfKingsState where
  g1 <> g2 = GenerosityOfKingsState
    { generosityOfKingsStatePlaques = on (<>) generosityOfKingsStatePlaques g1 g2
    , generosityOfKingsStateCattlePool = on (<>) generosityOfKingsStateCattlePool g1 g2
    , generosityOfKingsStateLastBid = on (<>) generosityOfKingsStateLastBid g1 g2
    , generosityOfKingsStatePlayersPassed = on (<>) generosityOfKingsStatePlayersPassed g1 g2
    }

instance Monoid GenerosityOfKingsState where
  mempty = GenerosityOfKingsState mempty mempty mempty mempty

makeLensesWith camelCaseFields ''GenerosityOfKingsState

data Phase
  = PreSetup
  | Setup
  | GenerosityOfKings
  | ReligionAndCulture
  | Revenues
  | LetUsCompareMythologies
    deriving (Show, Eq, Generic)

instance ToJSON Phase where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Phase where
  parseJSON = genericParseJSON defaultOptions

data Round = Round
  { roundPlayers                :: [PlayerId]
  -- ^ Ordered list representing the order of play, determined by
  -- (a) VR in the generosity of kings phase, and
  -- (b) by the generosity of kings phase in other phases.
  , roundCurrentPlayer          :: Last PlayerId
  -- ^ The current player of the round
  , roundUsedMarkers            :: Merge Location (Last UsedMarker)
  -- ^ Used marker locations on the map
  , roundGenerosityOfKingsState :: GenerosityOfKingsState
  -- ^ State used in the Generosity of kings phase
  , roundCurrentPhase           :: Last Phase
  -- Current phase of the round
  } deriving (Generic)

instance FromJSON Round where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Round where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance Semigroup Round where
  r1 <> r2 = Round
    { roundPlayers = on (<>) roundPlayers r1 r2
    , roundCurrentPlayer = on (<>) roundCurrentPlayer r1 r2
    , roundUsedMarkers = on (<>) roundUsedMarkers r1 r2
    , roundGenerosityOfKingsState = on (<>) roundGenerosityOfKingsState r1 r2
    , roundCurrentPhase = on (<>) roundCurrentPhase r1 r2
    }

instance Monoid Round where
  mempty = Round mempty mempty mempty mempty mempty

makeLensesWith camelCaseFields ''Round

-- Note: unlimited cattle stock

data Game = Game
  { gamePlayers   :: Merge PlayerId Player
  -- ^ Players of the game, in unordered format.
  , gameRound     :: Round
  -- ^ Current Round state
  , gameMapLayout :: First MapLayout
  -- ^ Layout of the Map
  , gameCraftsmen :: Merge Craftsman (S.Set TechnologyCard)
  -- ^ Remaining Craftsmen of each type
  , gameWinner    :: Alt Maybe PlayerId
  } deriving (Generic)

instance ToJSON Game where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Game where
  parseJSON = withObject "Game" $ \o -> Game
    <$> o .: "gamePlayers"
    <*> o .: "gameRound"
    <*> o .: "gameMapLayout"
    <*> o .: "gameCraftsmen"
    <*> (Alt <$> o .: "gameWinner")

instance Semigroup Game where
  g1 <> g2 = Game
    { gamePlayers = on (<>) gamePlayers g1 g2
    , gameRound = on (<>) gameRound g1 g2
    , gameMapLayout = on (<>) gameMapLayout g1 g2
    , gameCraftsmen = on (<>) gameCraftsmen g1 g2
    , gameWinner = on (<>) gameWinner g1 g2
    }

instance Monoid Game where
  mempty = Game mempty mempty mempty mempty mempty

makeLensesWith camelCaseFields ''Game

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

-- * Pure functions from game to game

data PlayerAction (phase :: Phase) = PlayerAction { getPlayerAction :: Either GameError Game }
data GameEvent (phase :: Phase) = GameEvent { getGameEvent :: Either GameError Game }
