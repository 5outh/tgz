{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module TheGreatZimbabwe.Api where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Monoid                       (mconcat)
import           Data.Pool
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import           Database.Persist.Postgresql
import qualified Database.Persist.Postgresql       as P
import           Database.Persist.Sql
import           Network.HTTP.Types.Status
import           Network.Wai                       (Response)
import qualified TheGreatZimbabwe.Database.Command as Command
import           TheGreatZimbabwe.Database.Game
import qualified TheGreatZimbabwe.Database.Game    as Game
import           TheGreatZimbabwe.Database.JSONB
import qualified TheGreatZimbabwe.Database.User    as User
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.NewGame
import           TheGreatZimbabwe.Types
import           Web.Scotty
import qualified Web.Scotty                        as Scotty

devString :: ConnectionString
devString = "host=localhost dbname=tgz_dev user=bendotk port=5432"

main :: IO ()
main = do
  runStdoutLoggingT $ withPostgresqlPool devString 2 $ \pool -> do
    runDB pool $ do
      runMigration User.migrateAll
      runMigration Command.migrateAll
      runMigration Game.migrateAll

    liftIO $ scotty 3000 $ do
      -- TODO: Create a new user

      -- create a new game
      post "/new-game" $ do
        -- TODO: validate this is at least 2 people
        userIds :: [User.UserId] <- map (toSqlKey . fromIntegral)
          <$> param @[Int] "userIds"
        name  <- param "name"
        users <- runDB pool $ selectList [User.UserId <-. userIds] []
        let playerInfos = map User.toPlayerInfoWithId users
        eGameData <- getGameEvent <$> liftIO (newGame playerInfos)
        case eGameData of
          Left gameError -> case gameError of
            InvalidAction err -> do
              status forbidden403 *> json gameError
            InternalError err ->
              status internalServerError500 *> json gameError
          Right gameData -> do
            mSavedGameData <- runDB pool $ do
              key <- insert (Game.Game name (JSONB gameData))
              P.getEntity key
            case mSavedGameData of
              Nothing            -> status internalServerError500 *> json ()
              Just savedGameData -> json savedGameData

-- TODO: need runSqlPersistMPool?
runDB
  :: (MonadIO m, MonadBaseControl IO m)
  => ConnectionPool
  -> SqlPersistT m a
  -> m a
runDB pool act = withResource pool (runReaderT act)
