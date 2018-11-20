{-# LANGUAGE DeriveGeneric #-}
module TheGreatZimbabwe.Error where

import           Data.Aeson
import qualified Data.Aeson   as Aeson
import qualified Data.Text    as T
import           GHC.Generics

data GameError
  = InvalidAction T.Text
  | InternalError T.Text
  deriving (Show, Eq, Generic)

invalidAction :: T.Text -> Either GameError a
invalidAction = Left . InvalidAction

internalError :: T.Text -> Either GameError a
internalError = Left . InternalError

instance ToJSON GameError where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON GameError where
  parseJSON = genericParseJSON defaultOptions
