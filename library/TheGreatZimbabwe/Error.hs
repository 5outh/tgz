module TheGreatZimbabwe.Error where

import qualified Data.Text as T

data GameError
  = InvalidAction T.Text
  | InternalError T.Text
  deriving (Show, Eq)

invalidAction :: T.Text -> Either GameError a
invalidAction = Left . InvalidAction

internalError :: T.Text -> Either GameError a
internalError = Left . InternalError
