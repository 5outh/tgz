{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module TheGreatZimbabwe.Database.JSONB where

import           Data.Aeson
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding   as TE
import           Data.Time            ()
import           Database.Persist
import           Database.Persist.Sql
import           GHC.Generics

newtype JSONB a = JSONB { unJSONB :: a }
    deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (JSONB a) where
  parseJSON a = JSONB <$> parseJSON a

instance ToJSON a => ToJSON (JSONB a) where
  toJSON (JSONB a) = toJSON a
  toEncoding (JSONB a) = toEncoding a

instance (ToJSON a, FromJSON a) => PersistField (JSONB a) where
  toPersistValue = PersistText . TE.decodeUtf8 . BSL.toStrict . Aeson.encode
  fromPersistValue (PersistByteString v)
    = case Aeson.decode (BSL.fromStrict v) of
        Nothing    -> Left "Could not decode JSONB value"
        Just value -> Right value
  fromPersistValue _ = Left "Underlying format must be bytestring"

instance (ToJSON a, FromJSON a) => PersistFieldSql (JSONB a) where
   sqlType _ = SqlOther "JSONB"
