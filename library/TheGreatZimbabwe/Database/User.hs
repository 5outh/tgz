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

module TheGreatZimbabwe.Database.User where

import           Crypto.KDF.BCrypt           (hashPassword, validatePassword)
import           Data.ByteString
import qualified Data.ByteString.Char8       as B
import           Data.SecureMem
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Elm.Derive
import           TheGreatZimbabwe.Aeson
import           TheGreatZimbabwe.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User sql=users
    email     Text
    username  Text
    password  ByteString
    UniqueEmail email
    UniqueUsername username
    deriving Show
|]

toPlayerId :: UserId -> PlayerId
toPlayerId = PlayerId . fromIntegral . fromSqlKey

fromPlayerId :: PlayerId -> UserId
fromPlayerId (PlayerId playerId) = toSqlKey (fromIntegral playerId)

toPlayerInfoWithId :: Entity User -> (PlayerId, PlayerInfo)
toPlayerInfoWithId (Entity playerId (User {..})) =
  ( PlayerId . fromIntegral $ fromSqlKey playerId
  , PlayerInfo (Username userUsername)
               userEmail
               (PlayerId . fromIntegral $ fromSqlKey playerId)
  )

data UserView = UserView
  { _id       :: Int
  , _username :: Text
  , _email    :: Text
  }

toView :: Entity User -> UserView
toView (Entity userId (User {..})) = UserView {..}
 where
  _id       = fromIntegral $ fromSqlKey userId
  _username = userUsername
  _email    = userEmail

deriveBoth (unPrefix "_") ''UserView

newUser :: Text -> Text -> Text -> IO User
newUser email username password = do
  hashedPassword <- hashPassword 12 (B.pack $ T.unpack password)
  pure $ User
    { userEmail    = email
    , userUsername = username
    , userPassword = hashedPassword
    }
