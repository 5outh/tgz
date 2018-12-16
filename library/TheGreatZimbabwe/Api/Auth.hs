{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module TheGreatZimbabwe.Api.Auth where

import           Control.Monad.IO.Class
import           Crypto.KDF.BCrypt                   (hashPassword,
                                                      validatePassword)
import           Data.Aeson                          (FromJSON (..),
                                                      genericParseJSON)
import qualified Data.ByteString.Char8               as B
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Time.Clock
import           Data.UUID
import           Database.Persist
import           Database.Persist.Postgresql
import           GHC.Generics
import           System.Random
import           TheGreatZimbabwe.Aeson
import           TheGreatZimbabwe.Database.AuthToken
import           TheGreatZimbabwe.Database.User

data Credentials = Credentials
  { credentialsUsername :: Text
  , credentialsPassword :: Text
  } deriving (Generic)

instance FromJSON Credentials where
  parseJSON = genericParseJSON (unPrefix "credentials")

-- Login a user and return a new auth token. Deletes any existing auth tokens.
login :: MonadIO m => Credentials -> SqlPersistT m (Maybe (Entity AuthToken))
login Credentials {..} = do
  user <- getBy (UniqueUsername credentialsUsername)
  case user of
    Nothing -> pure Nothing
    Just (Entity userId user) ->
      if validatePassword (B.pack $ T.unpack credentialsPassword)
                          (userPassword user)
        then do
          uuid <- liftIO randomIO
          now  <- liftIO getCurrentTime
          let sevenDays = 7 * 24 * 60 * 60
              token     = AuthToken
                { authTokenUserId    = userId
                , authTokenToken     = toText uuid
                , authTokenIssuedAt  = now
                , authTokenExpiresAt = addUTCTime sevenDays now
                }
          deleteWhere ([AuthTokenUserId ==. userId])
          Just <$> insertEntity token
        else pure Nothing
