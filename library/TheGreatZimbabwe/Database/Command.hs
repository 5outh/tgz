{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module TheGreatZimbabwe.Database.Command where

import           Control.Monad.IO.Class             (liftIO)
import           Data.Text                          (Text)
import           Data.Time                          (UTCTime)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           TheGreatZimbabwe.Database.Game
import           TheGreatZimbabwe.Database.JSONB
import           TheGreatZimbabwe.Database.User
import           TheGreatZimbabwe.Types.GameCommand

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Command json sql=commands
    gameId       GameId
    userId       UserId
    createdAt    UTCTime
    command      (JSONB GameCommand) -- TODO want JSONB
|]
