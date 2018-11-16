{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module TheGreatZimbabwe.Types where

import           Control.Lens
import           Control.Lens.TH
import qualified Data.Map.Strict as M
import           Data.Maybe      (isJust)
import qualified Data.Set        as S
import qualified Data.Text       as T
import           Data.Validation
import           Numeric.Natural

data Location = Location
  { locationX :: !Natural
  , locationY :: !Char
  } deriving (Show, Eq, Ord)

makeLensesWith camelCaseFields ''Location

newtype Username = Username { usernameUsername :: T.Text }

makeLensesWith camelCaseFields ''Username

data PlayerInfo = PlayerInfo
  { playerInfoUsername :: Username
  -- ^ A Player's unique username
  , playerInfoEmail    :: T.Text
  -- ^ A Player's unique email address
  }

makeLensesWith camelCaseFields ''PlayerInfo

data Empire
  = Kilwa -- Red
  | Mutapa -- Yellow
  | Zulu -- Green
  | Lozi -- Black
  | Mapungubwe -- White
  deriving (Show, Eq)

data Resource = Clay | Wood | Ivory | Diamonds
  deriving (Show, Eq)

data Land
  = StartingArea
  | Resource Resource
  | BlankLand
    deriving (Show, Eq)

data Square =
    Water
  | Land Land
    deriving (Show, Eq)

newtype MapLayout = MapLayout { mapLayoutMapLayout :: M.Map Location Square }
  deriving (Show, Eq)

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
    deriving (Eq, Ord)

data PrimaryOrSecondary = Primary | Secondary

data TechnologyCard = TechnologyCard
  { technologyCardName               :: T.Text
  , technologyCardCraftsmanType      :: Craftsman
  , technologyCardVictoryRequirement :: Natural
  , technologyCardVictoryPoints      :: Natural
  , technologyCardCost               :: Natural
  }

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

-- Player owns their locations on the board. Map is just the layout.

newtype PlayerId = PlayerId { playerIdPlayerId :: Natural } deriving (Show, Eq, Ord)

makeLensesWith camelCaseFields ''PlayerId

data Player = Player
  { playerInfo               :: PlayerInfo
  -- ^ Info about the human player
  , playerVictoryRequirement :: Natural
  -- ^ Player's current victory requirement
  , playerVictoryPoints      :: Natural
  -- ^ Player's current victory points
  , playerEmpire             :: Empire
  -- ^ The Empire the Player belongs to.
  , playerCattle             :: Natural
  -- ^ Number of cattle that a Player currently has. Start amount: 3
  , playerMonuments          :: M.Map Location Natural
  -- ^ Locations of player-owned monuments on the map, along with monument height
  , playerCraftsmen          :: S.Set Location
  -- ^ Locations of player-owned craftsmen on the map
  , playerTechnologyCards    :: M.Map TechnologyCard Natural
  -- ^ Player-owned technology cards, along with the price other players must pay to use them
  , playerSpecialists        :: S.Set Specialist
  -- ^ Player-owned specialist cards
  , playerGod                :: Maybe God
  -- ^ God a player adores
  }

makeLensesWith camelCaseFields ''Player

-- Note: UsedTwice only occurs when Atete is in play.
data UsedMarker = NotUsed | Used | UsedTwice

data GenerosityOfKingsState = GenerosityOfKingsState
  { generosityOfKingsStatePlaques       :: [Empire]
  -- ^ Ordered plaques
  , generosityOfKingsStateCattlePool    :: Natural
  -- ^ Total number of cattle that have been bid (this along with plaques
  -- is enough to reconstruct the view)
  , generosityOfKingsStateLastBid       :: Natural
  -- ^ The last bid made; next bid must be higher (or pass)
  , generosityOfKingsStatePlayersPassed :: [PlayerId]
  -- ^ Which players have passed, in what order?
  }

makeLensesWith camelCaseFields ''GenerosityOfKingsState

data Phase
  = Setup
  | GenerosityOfKings
  | ReligionAndCulture
  | Revenues
  | LetUsCompareMythologies
    deriving (Eq)

data Round = Round
  { roundPlayers                :: [PlayerId]
  -- ^ Ordered list representing the order of play, determined by
  -- (a) VR in the generosity of kings phase, and
  -- (b) by the generosity of kings phase in other phases.
  , roundCurrentPlayer          :: PlayerId
  -- ^ The current player of the round
  , roundUsedMarkers            :: M.Map Location UsedMarker
  -- ^ Used marker locations on the map
  , roundGenerosityOfKingsState :: GenerosityOfKingsState
  -- ^ State used in the Generosity of kings phase
  , roundCurrentPhase           :: Phase
  }

makeLensesWith camelCaseFields ''Round

-- Note: unlimited cattle stock

data Game = Game
  { gamePlayers   :: M.Map PlayerId Player
  -- ^ Players of the game, in unordered format.
  , gameRound     :: Round
  -- ^ Current Round state
  , gameMapLayout :: MapLayout
  -- ^ Layout of the MapLayout
  , gameCraftsmen :: M.Map Craftsman [TechnologyCard]
  -- ^ Remaining Craftsmen of each type
  , gameWinner    :: Maybe PlayerId
  }

makeLensesWith camelCaseFields ''Game

newGameCraftsmen :: M.Map Craftsman [TechnologyCard]
newGameCraftsmen = M.fromList
  [ Potter .: potters
  , IvoryCarver .: ivoryCarvers
  , WoodCarver .: woodCarvers
  , DiamondCutter .: diamondCutters
  ]
 where
  (.:) = (,)
  -- VR/VP/Cost
  potters =
    [ TechnologyCard "Potter1" Potter 3 1 2
    , TechnologyCard "Potter2" Potter 4 1 2
    ]

  ivoryCarvers =
    [ TechnologyCard "IvoryCarver1" IvoryCarver 2 1 2
    , TechnologyCard "IvoryCarver2" IvoryCarver 3 1 2
    ]

  woodCarvers =
    [ TechnologyCard "WoodCarver1" WoodCarver 3 1 2
    , TechnologyCard "WoodCarver2" WoodCarver 4 1 2
    ]

  diamondCutters =
    [ TechnologyCard "DiamondCutter1" DiamondCutter 1 3 10
    , TechnologyCard "DiamondCutter1" DiamondCutter 2 3 10
    ]

  vesselMakers =
    [ TechnologyCard "VesselMaker1" VesselMaker 3 2 4
    , TechnologyCard "VesselMaker2" VesselMaker 4 2 4
    ]
  throneMakers =
    [ TechnologyCard "ThroneMaker1" ThroneMaker 3 2 4
    , TechnologyCard "ThroneMaker1" ThroneMaker 4 2 4
    ]
  sculptors =
    [ TechnologyCard "Sculptor1" Sculptor 3 2 4
    , TechnologyCard "Sculptor1" Sculptor 4 2 4
    ]

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
