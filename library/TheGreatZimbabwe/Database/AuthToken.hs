{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module TheGreatZimbabwe.Database.AuthToken where

import           Data.ByteString
import qualified Data.ByteString.Char8          as B
import           Data.SecureMem
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Time                      (UTCTime)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Elm.Derive
import           TheGreatZimbabwe.Database.User (UserId)
import           TheGreatZimbabwe.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  AuthToken sql=auth_tokens
    userId UserId
    token Text
    issuedAt UTCTime
    expiresAt UTCTime
    UniqueToken token
|]
