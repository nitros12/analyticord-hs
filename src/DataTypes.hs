{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module DataTypes where

import           Data.Aeson

import           GHC.Generics

import           Data.Text    (Text)


data ApiErr = ApiErr { error       :: Text -- ^ Name of the error
                     , description :: Text -- ^ Description of the error
                     } deriving (Generic, Show)

data LoginResponse = LoginResponse { lName    :: Text -- ^ Name of the bot
                                   , lBotID   :: Text -- ^ ID of the bot
                                   , lOwnerID :: Text -- ^ ID of the owner
                                   } deriving Show

data SendResponse = SendResponse { sStatus     :: Text -- ^ Status of the action, should be 'OK'
                                 , sResponseID :: Text -- ^ ID of the response
                                 } deriving Show

data BotResponse = BotResponse { rApikey  :: Text -- ^ API key of the bot
                               , rBotID   :: Text -- ^ ID of the bot
                               , rName    :: Text -- ^ Name of the bot
                               , rOwnerID :: Text -- ^ ID of the owner
                               } deriving Show

data DataResponse = DataResponse { dData       :: Text -- ^ Response data, it is up to you to decode this
                                 , dEventType  :: Text -- ^ Event type
                                 , dResponseID :: Text -- ^ ID of the response
                                 , dOwnerId    :: Text -- ^ ID of the bot that owns the response
                                 , dTime       :: Text -- ^ Time of the response, as a integer UNIX timestamp
                                 } deriving Show

instance FromJSON ApiErr

instance FromJSON LoginResponse where
  parseJSON = withObject "LoginResponse" $ \v -> LoginResponse
    <$> v .: "name"
    <*> v .: "id"
    <*> v .: "owner"

instance FromJSON SendResponse where
  parseJSON = withObject "SendResponse" $ \v -> SendResponse
    <$> v .: "status"
    <*> v .: "ID"

instance FromJSON BotResponse where
  parseJSON = withObject "BotResponse" $ \v -> BotResponse
    <$> v .: "apikey"
    <*> v .: "id"
    <*> v .: "name"
    <*> v .: "owner"

instance FromJSON DataResponse where
  parseJSON = withObject "DataResponse" $ \v -> DataResponse
    <$> v .: "data"
    <*> v .: "eventType"
    <*> v .: "id"
    <*> v .: "owner"
    <*> v .: "time"
