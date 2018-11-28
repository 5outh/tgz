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

import           Control.Arrow                      ((&&&))
import           Control.Monad.IO.Class
import           Data.Time                          (UTCTime)
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

fetchGameCommandsForGame
  :: MonadIO m => GameId -> SqlPersistT m [(UserId, GameCommand)]
fetchGameCommandsForGame gameId = do
  commands <- selectList [CommandGameId ==. gameId] [Asc CommandCreatedAt]
  let commandUserId'     = commandUserId . entityVal
      commandGameCommand = unJSONB . commandCommand . entityVal
  pure $ map (commandUserId' &&& commandGameCommand) commands

insertGameCommand
  :: MonadIO m
  => GameId
  -> UserId
  -> UTCTime
  -> GameCommand
  -> SqlPersistT m CommandId
insertGameCommand gameId userId now cmd =
  insert $ Command gameId userId now (JSONB cmd)
