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

import           Control.Monad.IO.Class         (liftIO)
import           Data.Text                      (Text)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           TheGreatZimbabwe.Database.Game
import           TheGreatZimbabwe.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User json
    username  Text
    email     Text
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
