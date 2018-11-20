{-# LANGUAGE DeriveGeneric #-}
module TheGreatZimbabwe.Types.GameCommand where

import           Data.Aeson
import qualified Data.Aeson                     as Aeson
import           GHC.Generics
import           TheGreatZimbabwe.Database.User (UserId (..))
-- TODO: This heirarchy is weird, should fix it
import           TheGreatZimbabwe.Types

data GameCommand
  = ChooseEmpire Empire UserId
  | PlaceStartingMonument Location UserId
  deriving Generic

instance ToJSON GameCommand where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON GameCommand where
  parseJSON = genericParseJSON defaultOptions
