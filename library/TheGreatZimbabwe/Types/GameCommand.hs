{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module TheGreatZimbabwe.Types.GameCommand where

import           Elm.Derive
import           GHC.Generics
import           GHC.Natural
import           TheGreatZimbabwe.Types

data GameCommand
  = ChooseEmpire Empire
  | PlaceStartingMonument Location
  | Bid Natural
  | Pass
  deriving (Generic, Show)

deriveBoth defaultOptions ''GameCommand
