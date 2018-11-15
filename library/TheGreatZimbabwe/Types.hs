{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
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
  { locationX :: Natural
  , locationY :: Char
  } deriving (Show, Eq)

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

-- TODO: Finish
data Empire = Kilwa | Mutapa | Zulu
  deriving (Show, Eq)

data Resource = Clay | Wood | Ivory | Diamonds

data Land
  = StartingArea
  | Resource Resource
  | BlankLand

data Square =
    Water
  | Land Land

newtype MapLayout = MapLayout { mapLayoutMapLayout :: M.Map Location Square }

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

data TechnologyCard = TechnologyCard
  { technologyCardName               :: T.Text
  , technologyCardCraftsmanType      :: Craftsman
  , technologyCardVictoryRequirement :: Natural
  , technologyCardVictoryPoints      :: Natural
  , technologyCardCost               :: Natural
  }

makeLensesWith camelCaseFields ''TechnologyCard

data Specialist = Shaman | RainCeremony | Herd | Builder | Nomads

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
  { generosityOfKingsStatePlaques       :: [(Empire, Natural)]
  -- ^ Ordered plaques and associated cattle amount
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

data Round = Round
  { roundPlayers                :: [PlayerId]
  -- ^ Ordered list representing the order of play, determined by
  -- the generosity of kings.
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
