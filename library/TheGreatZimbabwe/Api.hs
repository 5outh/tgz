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
import           Data.Monoid                     (mconcat)
import           Data.Pool
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as TL
import           Database.Persist.Postgresql
import qualified Database.Persist.Postgresql     as P
import           Network.HTTP.Types.Status
import           Network.Wai                     (Response)
import qualified TheGreatZimbabwe.Database.Game  as DB
import           TheGreatZimbabwe.Database.JSONB
import           TheGreatZimbabwe.Database.User
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.NewGame
import           TheGreatZimbabwe.Types
import           Web.Scotty
import qualified Web.Scotty                      as Scotty

devString :: ConnectionString
devString = ""

main :: IO ()
main = do
  runStdoutLoggingT $ withPostgresqlPool devString 2 $ \pool -> do
    -- TODO: obviously no logging will happen here because we're just lifting.
    -- We probably want that to not be the case.
    liftIO $ scotty 3000 $ do
      -- create a new game
      post "/new-game" $ do
        userIds :: [UserId] <- map (toSqlKey . fromIntegral)
          <$> param @[Int] "userIds"
        name  <- param "name"
        users <- runDB pool $ selectList [UserId <-. userIds] []
        let playerInfos = map toPlayerInfoWithId users
        eGameData <- getGameEvent <$> liftIO (newGame playerInfos)
        case eGameData of
          Left gameError -> case gameError of
            InvalidAction err -> do
              status forbidden403 *> json gameError
            InternalError err ->
              status internalServerError500 *> json gameError
          Right gameData -> do
            mSavedGameData <- runDB pool $ do
              key <- insert (DB.Game name (JSONB gameData))
              P.getEntity key
            case mSavedGameData of
              Nothing            -> status internalServerError500 *> json ()
              Just savedGameData -> json savedGameData

runDB
  :: (MonadIO m, MonadBaseControl IO m)
  => ConnectionPool
  -> SqlPersistT m a
  -> m a
runDB pool act = withResource pool (runReaderT act)
