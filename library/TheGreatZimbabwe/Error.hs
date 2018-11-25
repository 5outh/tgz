{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module TheGreatZimbabwe.Error where

import qualified Data.Text    as T
import           Elm.Derive
import           GHC.Generics

data GameError
  = InvalidAction T.Text
  | InternalError T.Text
  deriving (Show, Eq, Generic)

deriveBoth defaultOptions ''GameError

invalidAction :: T.Text -> Either GameError a
invalidAction = Left . InvalidAction

internalError :: T.Text -> Either GameError a
internalError = Left . InternalError

