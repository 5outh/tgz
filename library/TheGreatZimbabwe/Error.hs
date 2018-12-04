{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module TheGreatZimbabwe.Error where

import           Control.Monad
import qualified Data.Text     as T
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

impliesInvalid :: Bool -> T.Text -> Either GameError ()
predicate `impliesInvalid` err = when predicate $ invalidAction err

impliesError :: Bool -> T.Text -> Either GameError ()
predicate `impliesError` err = when predicate $ internalError err
