{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module TheGreatZimbabwe.Api where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Bifunctor                     (first)
import           Data.Pool
import           Data.Text                          (Text)
import qualified Data.Text.Lazy                     as TL
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
  Scotty.get "/user/:username" $ do
    addHeader "Access-Control-Allow-Origin" "*"
    usernameParam <- param @Text "username"
    mUser         <- runDB pool $ getBy (User.UniqueUsername usernameParam)
    case mUser of
      Nothing   -> status notFound404 *> json ()
      Just user -> json $ User.toView user

  Scotty.get "/game/:id" $ do
    addHeader "Access-Control-Allow-Origin" "*"
    gameId :: Game.GameId <- toSqlKey <$> param @Int64 "id"
    mGame                 <- runDB pool $ fetchFullGameWith [] gameId
    case mGame of
      Nothing                    -> status notFound404 *> json ()
      Just (Left  err          ) -> status forbidden403 *> json err
      Just (Right (_, gameView)) -> status ok200 *> json gameView

  Scotty.options "/game/:gameId/player/:username/command" $ do
    addHeader "Access-Control-Allow-Origin"  "*"
    addHeader "Access-Control-Allow-Headers" "Content-Type"
    status ok200 *> text ""

  Scotty.post "/game/:gameId/player/:username/command" $ do
    addHeader "Access-Control-Allow-Origin" "*"
    gameId        <- toSqlKey . fromIntegral <$> param @Int "gameId"
    -- TODO: mkUsername
    usernameParam <- param @Text "username"
    mUser         <- runDB pool $ getBy (User.UniqueUsername usernameParam)
    case mUser of
      Nothing                     -> status notFound404 *> json ()
      Just user@(Entity userId _) -> do
        preview <- flag "preview"
        command <- jsonData @GameCommand
        now     <- liftIO getCurrentTime

        mGame   <- runDB pool
          $ fetchFullGameWith [(User.toPlayerId userId, command)] gameId

        case mGame of
          Nothing                       -> status notFound404 *> json ()
          Just (Left  err             ) -> status forbidden403 *> json err
          Just (Right (game, gameView)) -> do
            -- TODO:
            -- validatePlayerOwnsCommand c
            -- TODO: Don't allow commit of partial religion and culture action
            -- (require 'end') (this can probably be noted in the textarea too)

            let
              saveCommand = void $ runDB pool $ Command.insertGameCommand
                (entityKey game)
                (entityKey user)
                now
                command
              shouldSave = not preview && case command of
                  -- Religion and culture commands must explicitly be ended.
                ReligionAndCultureCommand ReligionAndCultureMultiCommand {..}
                  -> religionAndCultureMultiCommandEnd
                _ -> True


            when shouldSave saveCommand
            status ok200 *> json gameView

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

flag :: TL.Text -> ActionM Bool
flag paramName = do
  ps <- params
  pure $ paramName `elem` map fst ps

-- | Fetch a Game, along with its GameView.
--
-- Returns 'Nothing' if the game does not exist.
-- Returns 'Right NewGameView' if the game exists and the command sequence is fine.
-- Returns 'Left error' if the game exists, but running the command sequence
-- produces an error.
--
fetchFullGameWith
  :: MonadIO m
  => [(PlayerId, GameCommand)]
  -> Game.GameId
  -> SqlPersistT
       m
       (Maybe (Either GameError (Entity Game.Game, Game.GameView)))
fetchFullGameWith cmds gameId = do
  mGame <- selectFirst [persistIdField ==. gameId] []
  case mGame of
    Nothing   -> pure Nothing
    Just game -> do
      commands <- Command.fetchGameCommandsForGame (entityKey game)
      let formattedCommands = map (first User.toPlayerId) commands

      let eGameState = runGameCommands
            (unJSONB $ Game.gameInitialState (entityVal game))
            (formattedCommands ++ cmds)

      pure $ Just $ case eGameState of
        Left  gameError -> Left gameError
        Right gameState -> Right (game, Game.fromGameState game gameState)

runDB
  :: (MonadIO m, MonadBaseControl IO m)
  => ConnectionPool
  -> SqlPersistT m a
  -> m a
runDB pool act = withResource pool (runReaderT act)
