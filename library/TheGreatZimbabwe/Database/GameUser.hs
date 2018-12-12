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

module TheGreatZimbabwe.Database.GameUser where

import           Database.Persist.Postgresql
import           Database.Persist.TH
import           TheGreatZimbabwe.Database.Game (GameId)
import           TheGreatZimbabwe.Database.User (UserId)
import qualified TheGreatZimbabwe.Types         as Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  GameUser sql=game_users
    gameId GameId
    userId UserId
|]
