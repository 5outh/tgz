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

import           Data.Text                   (Text)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Elm.Derive
import           TheGreatZimbabwe.Aeson
import           TheGreatZimbabwe.Types

-- TODO add password

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User sql=users
    username  Text
    email     Text
    UniqueEmail email
    UniqueUsername username
|]

toPlayerId :: UserId -> PlayerId
toPlayerId = PlayerId . fromIntegral . fromSqlKey

fromPlayerId :: PlayerId -> UserId
fromPlayerId (PlayerId playerId) = toSqlKey (fromIntegral playerId)

toPlayerInfoWithId :: Entity User -> (PlayerId, PlayerInfo)
toPlayerInfoWithId (Entity playerId (User {..})) =
  ( PlayerId . fromIntegral $ fromSqlKey playerId
  , PlayerInfo (Username userUsername) userEmail
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
