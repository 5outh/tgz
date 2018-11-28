{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module TheGreatZimbabwe.Types.GameCommand where

import           Elm.Derive
import           GHC.Generics
import           TheGreatZimbabwe.Types

data GameCommand
  = ChooseEmpire Empire
  | PlaceStartingMonument Location
  deriving (Generic, Show)

deriveBoth defaultOptions ''GameCommand
