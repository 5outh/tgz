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
  , fromGameState
  )
where

import           Data.Text                       (Text)
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Elm.Derive
import           TheGreatZimbabwe.Aeson
import           TheGreatZimbabwe.Database.JSONB
import           TheGreatZimbabwe.Database.User  (UserId)
import qualified TheGreatZimbabwe.Types          as Types

-- TODO: If we just store an initial seed, we aren't at all beholden to
-- the database representation of the game *at all*. The game command structure
-- still matters, but the game structure can be modified willy-nilly. Might be
-- worth that switch!
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Game sql=games
    name         Text
    adminId      UserId
    initialState (JSONB Types.Game)
|]

data GameView = GameView
  { _id    :: Int
  , _name  :: Text
  , _state :: Types.Game
  }

toView :: Entity Game -> GameView
toView (Entity gameId (Game _name _ (JSONB _state))) = GameView {..}
  where _id = fromIntegral $ fromSqlKey gameId

deriveBoth (unPrefix "_") ''GameView

-- For use for preview
fromGameState :: Entity Game -> Types.Game -> GameView
fromGameState (Entity gameId (Game _name _adminId _)) _state = GameView {..}
  where _id = fromIntegral $ fromSqlKey gameId
