module TheGreatZimbabwe.Aeson where

import           Data.Aeson hiding (defaultOptions)
import           Elm.Derive

unPrefix :: String -> Options
unPrefix str =
  defaultOptions { fieldLabelModifier = camelTo2 '_' . drop (length str) }
