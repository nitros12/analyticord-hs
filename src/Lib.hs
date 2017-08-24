{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib where


import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)

import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B

import           Data.Text                 (Text)
import           Data.Text.Encoding        (encodeUtf8)

import           Data.Int                  (Int64)

import           Data.Aeson                (FromJSON, eitherDecode)

import           DataTypes

type ApiEffect a = IO (Either ApiErr a)


data ApiKey = ApiKey { bot  :: ByteString
                     , user :: Maybe ByteString
                     } deriving Show
get =  (++) "GET "
post = (++) "POST "

(//) :: String -> String -> String
a // b =  a ++ "/" ++ b

baseAddress = "https://analyticord.solutions"

loginAddress   = get  $ baseAddress // "api" // "botLogin"
sendAddress    = post $ baseAddress // "api" // "submit"
getAddress     = get  $ baseAddress // "api" // "getData"
botinfoAddress = get  $ baseAddress // "api" // "botinfo"
botlistAddress = get  $ baseAddress // "api" // "botlist"

-- |Build an api key from the bot key, and maybe the user key
makeKey :: ByteString -> Maybe ByteString -> ApiKey
makeKey bot user = ApiKey (B.append "bot " bot) (B.append "user " <$> user)

-- |Make a request to the API
makeRequest :: FromJSON a => String -- ^ The url to make the request to, in the format "<request type> <url>"
            -> [(ByteString, Maybe ByteString)] -- ^ Array of k:v pairs for request parameters
            -> ByteString -- ^ Auth string to use
            -> ApiEffect a -- ^ Returned API response, either an error or your response
makeRequest url params auth = do
  manager <- newManager tlsManagerSettings

  baseRequest <- parseRequest url
  let req = setQueryString params $ baseRequest { requestHeaders = [("Authorization", auth)]}
  res <- httpLbs req manager

  let resp = responseBody res
  let status = statusCode $ responseStatus res
  pure $ case status of
    200 -> case eitherDecode resp of
      Right a  -> Right a
      Left err -> Prelude.error ("Unable to decode:" ++ err)
    otherwise -> case eitherDecode resp of
      Right a  -> Left a
      Left err -> Prelude.error ("Unable to decode:" ++ err)

doLogin :: ApiKey -> ApiEffect LoginResponse
doLogin = (makeRequest loginAddress []) . bot

doSend :: ApiKey -> ByteString -> ByteString -> ApiEffect SendResponse
doSend key etype edata = makeRequest sendAddress [(etype, Just edata)] $ bot key

doGet :: ApiKey -> [(ByteString, Maybe ByteString)] -> ApiEffect [DataResponse]
doGet (ApiKey _ (Just key)) params = makeRequest getAddress params key
doGet (ApiKey _ Nothing) _ = Prelude.error "ApiKey must be given a user key to call user endpoints"

doBotList :: ApiKey -> ApiEffect [BotResponse]
doBotList (ApiKey _ (Just key)) = makeRequest botlistAddress [] key
doBotList (ApiKey _ Nothing) = Prelude.error "ApiKey must be given a user key to call user endpoints"

doBotInfo :: ApiKey -> Text -> ApiEffect [BotResponse]
doBotInfo (ApiKey _ (Just key)) id = makeRequest botinfoAddress [("id", Just . encodeUtf8 $ id)] key
doBotInfo (ApiKey _ Nothing) _ = Prelude.error "ApiKey must be given a user key to call user endpoints"
