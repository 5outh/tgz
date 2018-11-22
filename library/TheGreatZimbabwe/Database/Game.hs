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

module TheGreatZimbabwe.Database.Game
  ( Game(..)
  , GameId
  , migrateAll
  , GameView
  , toView
  )
where

import           Data.Text                       (Text)
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Elm.Derive
import           TheGreatZimbabwe.Aeson
import           TheGreatZimbabwe.Database.JSONB
import qualified TheGreatZimbabwe.Types          as Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Game sql=games
    name         Text
    initialState (JSONB Types.Game)
|]

data GameView = GameView
  { _id           :: GameId
  , _name         :: Text
  , _initialState :: Types.Game
  }

toView :: Entity Game -> GameView
toView (Entity _id (Game _name (JSONB _initialState))) = GameView {..}

deriveBoth (unPrefix "_") ''GameView
