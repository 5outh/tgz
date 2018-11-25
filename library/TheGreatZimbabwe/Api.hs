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
import           Data.Pool
import           Data.Text                          (Text)
import           Data.Time.Clock                    (getCurrentTime)
import           Database.Persist.Postgresql
import qualified Database.Persist.Postgresql        as P
import           GHC.Int                            (Int64)
import           Network.HTTP.Types.Status
import           TheGreatZimbabwe
import qualified TheGreatZimbabwe.Database.Command  as Command
import qualified TheGreatZimbabwe.Database.Game     as Game
import           TheGreatZimbabwe.Database.JSONB
import qualified TheGreatZimbabwe.Database.User     as User
import qualified TheGreatZimbabwe.Database.User     as User
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.NewGame
import           TheGreatZimbabwe.Types
import           TheGreatZimbabwe.Types.GameCommand
import           Web.Scotty
import qualified Web.Scotty                         as Scotty

-- TODO: Environment Variables
devString :: ConnectionString
devString = "host=localhost dbname=tgz_dev user=bendotk port=5432"

api :: IO ()
api = do
  runStdoutLoggingT $ withPostgresqlPool devString 10 $ \pool -> do
    runDB pool $ do
      runMigration User.migrateAll
      runMigration Command.migrateAll
      runMigration Game.migrateAll

    liftIO $ scotty 8000 (routes pool)

routes :: ConnectionPool -> ScottyM ()
routes pool = do
  Scotty.get "/game/:id" $ do
    addHeader "Access-Control-Allow-Origin" "*"
    gameId :: Game.GameId <- toSqlKey <$> param @Int64 "id"
    mGame <- runDB pool $ selectFirst [persistIdField ==. gameId] []
    case mGame of
      Nothing   -> status notFound404 *> json ()
      Just game -> status ok200 *> json (Game.toView game)

  Scotty.post "/game/:gameId/player/:username/command" $ do
    gameId <- toSqlKey . fromIntegral <$> param @Int "gameId"
    mGame  <- runDB pool $ selectFirst [persistIdField ==. gameId] []
    case mGame of
      Nothing   -> status notFound404 *> json ()
      Just game -> do
        -- TODO: mkUsername
        username <- param @Text "username"
        mUser    <- runDB pool $ getBy (User.UniqueUsername username)
        case mUser of
          Nothing   -> status notFound404 *> json ()
          Just user -> do
            preview  <- flag "preview"
            command  <- jsonData @GameCommand
            commands <- runDB pool
              $ Command.fetchGameCommandsForGame (entityKey game)
            now <- liftIO getCurrentTime

            let eGameState = runGameCommands
                  (unJSONB $ Game.gameInitialState (entityVal game))
                  (commands ++ [command])

            liftIO $ print preview

            case eGameState of
              Left  gameError -> json gameError
              Right gameState -> do
                when (not preview)
                  $ void
                  $ runDB pool
                  $ Command.insertGameCommand (entityKey game)
                                              (entityKey user)
                                              now
                                              command
                status ok200
                json (Game.fromGameState game gameState)

  Scotty.post "/new-game" $ do
    addHeader "Access-Control-Allow-Origin" "*"
    userIds :: [User.UserId] <- map (toSqlKey . fromIntegral)
      <$> param @[Int] "userIds"
    gameName <- param "name"
    users    <- runDB pool $ selectList [User.UserId <-. userIds] []
    let playerInfos = map User.toPlayerInfoWithId users
    eGameData <- getGameEvent <$> liftIO (newGame playerInfos)
    case eGameData of
      Left gameError -> case gameError of
        InvalidAction _ -> do
          status forbidden403 *> json gameError
        InternalError _ -> status internalServerError500 *> json gameError
      Right gameData -> do
        mSavedGameData <- runDB pool $ do
          key <- insert (Game.Game gameName (JSONB gameData))
          P.getEntity key
        case mSavedGameData of
          Nothing            -> status internalServerError500 *> json ()
          Just savedGameData -> json (Game.toView savedGameData)

flag paramName = do
  ps <- params
  pure $ paramName `elem` map fst ps

runDB
  :: (MonadIO m, MonadBaseControl IO m)
  => ConnectionPool
  -> SqlPersistT m a
  -> m a
runDB pool act = withResource pool (runReaderT act)
