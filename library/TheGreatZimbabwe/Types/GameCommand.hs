{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module TheGreatZimbabwe.Types.GameCommand where

import           Data.Aeson             hiding (defaultOptions)
import           Elm.Derive
import           GHC.Generics
import           TheGreatZimbabwe.Types

data GameCommand
  = ChooseEmpire Empire PlayerId
  | PlaceStartingMonument Location PlayerId
  deriving Generic

deriveBoth defaultOptions ''GameCommand
