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

module TheGreatZimbabwe.Database.Game where

import           Control.Monad.IO.Class          (liftIO)
import           Data.Text                       (Text)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           TheGreatZimbabwe.Database.JSONB
import qualified TheGreatZimbabwe.Types          as Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Game json
    name         Text
    initialState (JSONB Types.Game)
|]
