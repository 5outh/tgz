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
  deriving (Show)

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
  deriving (Show)

deriveBoth defaultOptions ''UseSpecialist

data SetPrice = SetPrice
  { setPricePrice     :: Int
  , setPriceCraftsman :: Craftsman
  } deriving (Show)

deriveBoth (unPrefix "setPrice") ''SetPrice

data ReligionAndCultureCommand3
  = BuildMonuments (NonEmpty Location)
  | PlaceCraftsmen [(Location, Rotated Craftsman)] [SetPrice]
  | RaiseMonuments [(Location, [RaiseMonumentCommand])]
  deriving (Show)

deriveBoth defaultOptions ''ReligionAndCultureCommand3

data ReligionAndCultureMultiCommand = ReligionAndCultureMultiCommand
  { religionAndCultureMultiCommandDziva   :: Maybe [SetPrice]
  -- ^ Dziva can raise or lower prices at the beginning of their turn. If this
  -- is set and player is *not* Dziva, that is invalid
  , religionAndCultureMultiCommandAction1 :: Maybe ReligionAndCultureCommand1
  , religionAndCultureMultiCommandAction2 :: Maybe UseSpecialist
  , religionAndCultureMultiCommandAction3 :: Maybe ReligionAndCultureCommand3
  , religionAndCultureMultiCommandEnd     :: Bool
  } deriving (Show)

deriveBoth (unPrefix "religionAndCultureMultiCommand") ''ReligionAndCultureMultiCommand

data GameCommand
  = ChooseEmpire Empire
  | PlaceStartingMonument Location
  | Bid Natural
  | Pass
  | ReligionAndCultureCommand ReligionAndCultureMultiCommand
  -- ^ Required to end a religion and culture command. Does nothing otherwise.
  deriving (Generic, Show)

deriveBoth defaultOptions ''GameCommand
