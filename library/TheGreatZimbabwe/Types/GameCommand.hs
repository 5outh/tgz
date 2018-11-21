{-# LANGUAGE DeriveGeneric #-}
module TheGreatZimbabwe.Types.GameCommand where

import           Data.Aeson
import           GHC.Generics
import           TheGreatZimbabwe.Types

data GameCommand
  = ChooseEmpire Empire PlayerId
  | PlaceStartingMonument Location PlayerId
  deriving Generic

instance ToJSON GameCommand where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON GameCommand where
  parseJSON = genericParseJSON defaultOptions
