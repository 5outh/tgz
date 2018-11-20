{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TheGreatZimbabwe.Api where

import           Web.Scotty
import qualified Web.Scotty                     as Scotty

import           Control.Monad.IO.Class         (liftIO)
import           Data.Monoid                    (mconcat)
import           Database.Persist.Postgresql
import qualified Database.Persist.Postgresql    as P
import           Network.HTTP.Types.Status
import           TheGreatZimbabwe.Database.User
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.NewGame
import           TheGreatZimbabwe.Types

main :: IO ()
main = scotty 3000 $ do
  -- create a new game
  post "/new-game" $ do
    userIds :: [UserId] <- jsonData
    -- TODO: pass a connection pool here
    users               <- runDB $ selectList [UserId <-. userIds]
    let playerInfos = map toPlayerInfoWithId users
    eGameData <- getGameEvent <$> liftIO (newGame playerInfos)
    case eGameData of
      Left gameError -> case gameError of
        InvalidAction err -> do
          status forbidden403 *> json gameError
        InternalError err -> status internalServerError500 *> json gameError
      Right gameData -> do
        mSavedGameData <- runDB $ insert gameData >>= P.get
        case mSavedGameData of
          Nothing            -> raise "Game was not created"
          Just savedGameData -> json savedGameData

  -- here for testing, for now
  Scotty.get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
