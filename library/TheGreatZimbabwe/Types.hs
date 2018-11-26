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
import           TheGreatZimbabwe.Orphans                 ( )
import           Elm.Derive

newtype PlayerId = PlayerId { getPlayerId :: Integer }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSONKey, ToJSONKey)

deriveBoth (unPrefix "get") ''PlayerId

data Location = Location
  { locationX :: !Natural
  , locationY :: !String
  } deriving (Show, Eq, Ord, Generic)

instance ToJSONKey Location where
  toJSONKey = ToJSONKeyValue toJSON toEncoding

instance FromJSONKey Location where
  fromJSONKey = FromJSONKeyValue parseJSON

deriveBoth (unPrefix "location") ''Location
makeLensesWith camelCaseFields ''Location

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
  deriving (Show, Eq, Generic)

deriveBoth (unPrefix "empire") ''Empire

data Resource = Clay | Wood | Ivory | Diamonds
  deriving (Show, Eq, Generic)

deriveBoth (unPrefix "resource") ''Resource

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
    deriving (Eq, Ord, Generic, Show)

instance FromJSONKey Craftsman where
  fromJSONKey = FromJSONKeyValue parseJSON

instance ToJSONKey Craftsman where
  toJSONKey = ToJSONKeyValue toJSON toEncoding

deriveBoth defaultOptions ''Craftsman

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
    deriving (Eq, Ord, Generic, Show)

deriveBoth defaultOptions ''Specialist

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
    deriving (Generic, Show)

deriveBoth defaultOptions ''God

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
  { playerInfo               :: Maybe PlayerInfo
  -- ^ Info about the human player
  , playerVictoryRequirement :: Int
  -- ^ Player's current victory requirement
  , playerVictoryPoints      :: Int
  -- ^ Player's current victory points
  , playerEmpire             :: Maybe Empire
  -- ^ The Empire the Player belongs to. Players have to choose this at the
  -- beginning of the game, so it's initially empty.
  , playerCattle             :: Int
  -- ^ Number of cattle that a Player currently has. Start amount: 3
  , playerMonuments          :: M.Map Location Natural
  -- ^ Locations of player-owned monuments on the map, along with monument height
  , playerCraftsmen          :: M.Map Location Craftsman
  -- ^ Locations of player-owned craftsmen on the map (each individual owned square)
  , playerTechnologyCards    :: M.Map TechnologyCard Int
  -- ^ Player-owned technology cards, along with the price other players must
  -- pay to use them
  , playerSpecialists        :: S.Set Specialist
  -- ^ Player-owned specialist cards
  , playerGod                :: Maybe God
  -- ^ God a player adores
  } deriving (Generic, Show)

enrichPlayerVP :: Player -> Player
enrichPlayerVP player@(Player {..}) =
  player & victoryPoints +~ fromIntegral extraVictoryPoints
 where
  extraVictoryPoints = sum [monumentVP, technologyCardVP]
  monumentVP         = sum $ map getHeightVP (M.elems playerMonuments)
  technologyCardVP =
    sum $ map technologyCardVictoryPoints (M.keys playerTechnologyCards)
  getHeightVP = \case
    1 -> 1
    2 -> 3
    3 -> 7
    4 -> 13
    5 -> 21
    _ -> error "Height invalid!"

instance Semigroup Player where
  p1 <> p2 = Player
    { playerInfo = on (<|>) playerInfo p1 p2
    --- ^ TODO: This may not be needed
    , playerVictoryRequirement = on (+) playerVictoryRequirement p1 p2
    , playerVictoryPoints = on (+) playerVictoryPoints p1 p2
    , playerEmpire = on (<|>) playerEmpire p1 p2
    , playerCattle = on (+) playerCattle p1 p2
    , playerMonuments = on (M.unionWith (+)) playerMonuments p1 p2
    , playerCraftsmen = on (<>) playerCraftsmen p1 p2
    , playerTechnologyCards = on (M.unionWith (+)) playerTechnologyCards p1 p2
    , playerSpecialists = on (<>) playerSpecialists p1 p2
    , playerGod = on (<|>) playerGod p1 p2
    }

instance Monoid Player where
  mempty = Player
    { playerInfo = Nothing
    , playerVictoryRequirement = 0
    , playerVictoryPoints = 0
    , playerEmpire = Nothing
    , playerCattle = 0
    , playerMonuments = mempty
    , playerCraftsmen = mempty
    , playerTechnologyCards = mempty
    , playerSpecialists = mempty
    , playerGod = Nothing
    }

deriveBoth (unPrefix "player") ''Player
makeLensesWith camelCaseFields ''Player

-- Note: UsedTwice only occurs when Atete is in play.
data UsedMarker = NotUsed | Used | UsedTwice
  deriving (Generic, Show)

deriveBoth defaultOptions ''UsedMarker

data GenerosityOfKingsState = GenerosityOfKingsState
  { generosityOfKingsStatePlaques       :: [Empire]
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
    , generosityOfKingsStateLastBid = on plusMay generosityOfKingsStateLastBid g1 g2
    , generosityOfKingsStatePlayersPassed = on (<>) generosityOfKingsStatePlayersPassed g1 g2
    }
   where
    plusMay Nothing Nothing = Nothing
    plusMay Nothing (Just n) = Just n
    plusMay (Just n) Nothing = Just n
    plusMay (Just n) (Just m) = Just $ m + n

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
  , gameMapLayout :: Maybe MapLayout
  -- ^ Layout of the Map
  , gameCraftsmen :: M.Map Craftsman (S.Set TechnologyCard)
  -- ^ Remaining Craftsmen of each type
  , gameWinner    :: Maybe PlayerId
  } deriving (Generic, Show)

instance Semigroup Game where
  g1 <> g2 = Game
    { gamePlayers = on (M.unionWith (<>)) gamePlayers g1 g2
    , gameRound = on (<>) gameRound g1 g2
    , gameMapLayout = on appendLast gameMapLayout g1 g2
    , gameCraftsmen = on (M.unionWith (<>)) gameCraftsmen g1 g2
    , gameWinner = on (<|>) gameWinner g1 g2
    }

instance Monoid Game where
  mempty = Game mempty mempty Nothing mempty Nothing

deriveBoth (unPrefix "game") ''Game
makeLensesWith camelCaseFields ''Game

-- TODO: I don't love this function, but it does the job for now.
enrichPlayersVP :: Game -> Game
enrichPlayersVP game = game & players %~ fmap enrichPlayerVP

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
