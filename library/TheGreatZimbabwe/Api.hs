{-# LANGUAGE DeriveGeneric         #-}
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
import           Crypto.KDF.BCrypt                   (hashPassword,
                                                      validatePassword)
import           Data.Aeson                          (FromJSON (..),
                                                      genericParseJSON, object,
                                                      (.=))
import           Data.Bifunctor                      (first)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Char8               as B
import           Data.Pool
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Lazy                      as TL
import           Data.Time.Clock                     (getCurrentTime)
import           Database.Persist.Postgresql
import qualified Database.Persist.Postgresql         as P
import           Debug.Trace
import           GHC.Generics
import           GHC.Int                             (Int64)
import           Network.HTTP.Types.Status
import           Network.Wai                         (pathInfo, requestMethod)
import           Network.Wai.Middleware.HttpAuth
import           TheGreatZimbabwe
import           TheGreatZimbabwe.Aeson
import           TheGreatZimbabwe.Api.Auth
import qualified TheGreatZimbabwe.Database.AuthToken as AuthToken
import qualified TheGreatZimbabwe.Database.Command   as Command
import qualified TheGreatZimbabwe.Database.Game      as Game
import qualified TheGreatZimbabwe.Database.GameUser  as GameUser
import           TheGreatZimbabwe.Database.JSONB
import qualified TheGreatZimbabwe.Database.User      as User
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.NewGame
import           TheGreatZimbabwe.Types
import           TheGreatZimbabwe.Types.GameCommand
import           Web.Scotty                          hiding (get, post)
import qualified Web.Scotty                          as Scotty

data Signup = Signup
  { signupUsername :: Text
  , signupPassword :: Text
  , signupEmail    :: Text
  } deriving (Generic)

instance FromJSON Signup where
  parseJSON = genericParseJSON (unPrefix "signup")

-- TODO: Environment Variables
devString :: ConnectionString
devString = "host=localhost dbname=tgz_dev user=bendotk port=5432"

authSettings :: AuthSettings
authSettings = "TGZ"
  { authIsProtected = \req -> do
                        let isWhitelisted =
                              pathInfo req
                                == ["login"]
                                || pathInfo req
                                == ["signup"]
                                || requestMethod req
                                == "OPTIONS"

                        pure (not isWhitelisted)
  }

api :: IO ()
api = do
  --connectionString :: ConnectionString <- getEnv "PG_CONN_STR"
  runStdoutLoggingT $ withPostgresqlPool devString 10 $ \pool -> do
    runDB pool $ do
      runMigration User.migrateAll
      runMigration Command.migrateAll
      runMigration Game.migrateAll
      runMigration GameUser.migrateAll
      runMigration AuthToken.migrateAll

    liftIO $ scotty 8000 $ do
      middleware $ basicAuth (authorizeUser pool) authSettings

      routes pool

authorizeUser :: ConnectionPool -> ByteString -> ByteString -> IO Bool
authorizeUser pool u p = do
  mUser <- runDB pool $ getBy (User.UniqueUsername (T.pack $ B.unpack u))
  pure $ case mUser of
    Nothing              -> False
    Just (Entity _ user) -> validatePassword p (User.userPassword user)

data AuthError = UserNotFound | UserCredentialsInvalid
  deriving (Show, Eq)

-- re-checks the user
getAuthorizedUser pool = do
  mAuthHeader <- header "Authorization"
  case mAuthHeader of
    Just text -> case extractBasicAuth (B.pack (TL.unpack text)) of
      Nothing                   -> raise "Authorzation header is malformed"
      Just (username, password) -> do
        mUser <- runDB pool
          $ getBy (User.UniqueUsername (T.pack (B.unpack username)))
        case mUser of
          Nothing -> raise "User is missing"
          Just user ->
            if validatePassword password (User.userPassword (entityVal user))
              then pure user
              else raise "User credentials incorrect"

    Nothing -> raise "Authorization header missing"

routes :: ConnectionPool -> ScottyM ()
routes pool = do
  httpPost "/signup" (postSignup pool)
  -- This just checks if a credentials are valid.
  httpPost "/login"  (postLogin pool)
  httpGet "/games/:id" $ getGame pool
  httpPost "/games/:gameId/players/:username/commands"
    $ postPlayerGameCommand pool
  httpPost "/games" (postGame pool)
  httpGet "/users/:username" (getUser pool)
  httpGet "/users/:id/games" (getUserGames pool)

getUser pool = do
  userEntity@(Entity userId user) <- getAuthorizedUser pool
  usernameParam :: Text           <- param @Text "username"

  if usernameParam /= (User.userUsername user)
    then status forbidden403 *> json ()
    else do
      user <- getAuthorizedUser pool
      status ok200 *> json (User.toView userEntity)

getUserGames pool = do
  Entity userId user         <- getAuthorizedUser pool
  userIdParam :: User.UserId <- toSqlKey <$> param @Int64 "id"
  if userIdParam /= userId
    then status forbidden403 *> json ()
    else do
      games <- runDB pool $ do
        gameUsers <- selectList [GameUser.GameUserUserId ==. userIdParam] []
        selectList
          [ persistIdField
              <-. map (GameUser.gameUserGameId . entityVal) gameUsers
          ]
          []
        -- for now, don't worry about hydrating the games. just returm them.
      status ok200 *> json (map Game.toView games)

postSignup pool = do
  Signup {..} <- jsonData @Signup
  user0       <- runDB pool $ getBy (User.UniqueUsername signupUsername)
  case user0 of
    Just _  -> status badRequest400 *> json ()
    Nothing -> do
      user <- liftIO $ User.newUser signupEmail signupUsername signupPassword
      runDB pool $ insert user
      mToken <- runDB pool $ login $ Credentials signupUsername signupPassword
      case mToken of
        -- This 500s because a failure to login here should be impossible.
        Nothing -> status internalServerError500 *> json ()
        Just (Entity _ AuthToken.AuthToken {..}) ->
          status ok200 *> json (object ["token" .= authTokenToken])

postLogin pool = do
  credentials@Credentials {..} <- jsonData @Credentials
  mToken                       <- runDB pool $ login credentials
  case mToken of
    Nothing -> status notFound404 *> json ()
    Just (Entity _ AuthToken.AuthToken {..}) ->
      status ok200 *> json (object ["token" .= authTokenToken])

getGame pool = do
  gameId :: Game.GameId <- toSqlKey <$> param @Int64 "id"
  mGame                 <- runDB pool $ fetchFullGameWith [] gameId
  case mGame of
    Nothing                    -> status notFound404 *> json ()
    Just (Left  err          ) -> status forbidden403 *> json err
    Just (Right (_, gameView)) -> status ok200 *> json gameView

postPlayerGameCommand pool = do
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
          let saveCommand = void $ runDB pool $ Command.insertGameCommand
                (entityKey game)
                (entityKey user)
                now
                command
              shouldSave = not preview

          when shouldSave saveCommand
          status ok200 *> json gameView

data PostGame = PostGame
  { postGameUsernames :: [Text]
  } deriving (Generic)

instance FromJSON PostGame where
  parseJSON = genericParseJSON (unPrefix "postGame")

postGame pool = do
  PostGame {..} <- jsonData @PostGame

  admin         <- getAuthorizedUser pool

  gameName      <- param "name"
  users <- runDB pool $ selectList [User.UserUsername <-. postGameUsernames] []

  let playerInfos = map User.toPlayerInfoWithId users
  eGameData <- getGameEvent <$> liftIO (newGame playerInfos)
  case eGameData of
    Left gameError -> case gameError of
      InvalidAction _ -> do
        status forbidden403 *> json gameError
      InternalError _ -> status internalServerError500 *> json gameError
    Right gameData -> do
      mSavedGameData <- runDB pool $ do
        key <- insert (Game.Game gameName (entityKey admin) (JSONB gameData))
        P.getEntity key
      case mSavedGameData of
        Nothing            -> status internalServerError500 *> json ()
        Just savedGameData -> do
          -- Associate each user with the game in question
          void
            $ runDB pool
            $ insertMany
            $ flip map (map entityKey users)
            $ \userId -> GameUser.GameUser (entityKey savedGameData) userId
          json (Game.toView savedGameData)

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

withCors act = do
  addHeader "Access-Control-Allow-Origin"  "*"
  addHeader "Access-Control-Allow-Headers" "Content-Type"
  addHeader "Access-Control-Allow-Headers" "Authorization"
  act

corsOptions name = Scotty.options name $ withCors (status ok200 *> text "")

httpGet name act = do
  corsOptions name
  Scotty.get name $ withCors act

httpPost name act = do
  corsOptions name
  Scotty.post name $ withCors act

runDB
  :: (MonadIO m, MonadBaseControl IO m)
  => ConnectionPool
  -> SqlPersistT m a
  -> m a
runDB pool act = withResource pool (runReaderT act)
