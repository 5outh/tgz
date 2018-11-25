{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module TheGreatZimbabwe.Types.GameCommand where

import           Elm.Derive
import           GHC.Generics
import           TheGreatZimbabwe.Types

data GameCommand
  = ChooseEmpire Empire PlayerId
  | PlaceStartingMonument Location PlayerId
  deriving (Generic, Show)

deriveBoth defaultOptions ''GameCommand
