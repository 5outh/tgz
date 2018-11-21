{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TheGreatZimbabwe.Orphans where

import           Data.Aeson
import           Data.Monoid

deriving newtype instance FromJSON a => FromJSON (Alt Maybe a)
deriving newtype instance ToJSON a => ToJSON (Alt Maybe a)
deriving newtype instance FromJSON a => FromJSON (Sum a)
deriving newtype instance ToJSON a => ToJSON (Sum a)
