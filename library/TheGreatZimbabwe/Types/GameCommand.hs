{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module TheGreatZimbabwe.Types.GameCommand where

import           Data.List.NonEmpty                  (NonEmpty)
import           Elm.Derive
import           GHC.Generics
import           GHC.Natural
import           TheGreatZimbabwe.Aeson
import           TheGreatZimbabwe.ReligionAndCulture (RaiseMonumentCommand (..))
import           TheGreatZimbabwe.Types

data ReligionAndCultureCommand1 = ChooseGod God | ChooseSpecialist Specialist
  deriving (Show, Eq)

deriveBoth defaultOptions ''ReligionAndCultureCommand1

data UseSpecialist
  = UseShaman Resource Location
  -- ^ Place a resource tile at some location
  | UseRainCeremony Location Location
  -- ^ Place 2 contiguous water tiles.
  | UseHerd Natural
  -- ^ Use a herd n times
  | UseBuilder
  | UseNomads
  deriving (Show,Eq)

deriveBoth defaultOptions ''UseSpecialist

data SetPrice = SetPrice
  { setPricePrice     :: Int
  , setPriceCraftsman :: Craftsman
  } deriving (Show,Eq)

deriveBoth (unPrefix "setPrice") ''SetPrice

data ReligionAndCultureCommand3
  = BuildMonuments (NonEmpty Location)
  | PlaceCraftsmen [(Location, Rotated Craftsman)] [SetPrice]
  | RaiseMonuments [(Location, [RaiseMonumentCommand])]
  deriving (Show,Eq)

deriveBoth defaultOptions ''ReligionAndCultureCommand3

data ReligionAndCultureMultiCommand = ReligionAndCultureMultiCommand
  { religionAndCultureMultiCommandDziva   :: Maybe [SetPrice]
  -- ^ Dziva can raise or lower prices at the beginning of their turn. If this
  -- is set and player is *not* Dziva, that is invalid
  , religionAndCultureMultiCommandAction1 :: Maybe ReligionAndCultureCommand1
  , religionAndCultureMultiCommandAction2 :: Maybe UseSpecialist
  , religionAndCultureMultiCommandAction3 :: Maybe ReligionAndCultureCommand3
  } deriving (Show, Eq)

deriveBoth (unPrefix "religionAndCultureMultiCommand") ''ReligionAndCultureMultiCommand

data GameCommand
  = ChooseEmpire Empire
  -- ^ @choose-empire mapungubwe@
  | PlaceStartingMonument Location
  -- ^ place-starting-monument a4
  | Bid Natural
  -- ^ bid 4
  | Pass
  -- ^ pass
  | ReligionAndCultureCommand ReligionAndCultureMultiCommand
  -- ^
  -- set dziva prices:
  -- - set-price-dziva potter 3
  -- one of these 3:
  --  command 1, one of:
  --  - choose-god anansi
  --  - choose-specialist herd. herd 2 -- MUST be used on this turn
  --  command 2, one of:
  --  - shaman ivory a7
  --  - rain-ceremony a9
  --  - herd 3
  --  - builder
  --  - nomads
  --  command 3, one of:
  --  1. build-monument a3 (+ more allowed if obatala)
  --  2. place-craftsman a4 (rotate potter). set-price potter 3 -- must appear together
  --     -- can also set other prices:
  --     set-price diamond-cutter 1
  --  3. raise a9
  --     hub a1
  --     use-craftsman a3 b1 -- use the craftsman at a3 using resource at b1
  --     -- and so on
  --
  --  Note: when specialists are chosen they must also be used that turn
  --
  deriving (Generic, Show, Eq)

deriveBoth defaultOptions ''GameCommand
