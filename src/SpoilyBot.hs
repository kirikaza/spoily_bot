{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module SpoilyBot
    ( startApp
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, append, pack)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant ((:>), Get, PlainText, Proxy(..), Server, serve)
import Web.Telegram.API.Bot (GetMeResponse(..), Token(..), getMe, user_first_name, user_result)

type API = "start" :> Get '[PlainText] Text

startApp :: Int -> String -> IO ()
startApp port telegramToken = run port $ app telegramToken

app :: String -> Application
app telegramToken = serve api $ server telegramToken

api :: Proxy API
api = Proxy

server :: String -> Server API
server telegramToken = liftIO $ start telegramToken

start :: String -> IO Text
start telegramToken = do
  let token = Token . pack $ "bot" ++ telegramToken
  manager <- newManager tlsManagerSettings
  res <- getMe token manager
  case res of
    Left e -> do
      return . pack . show $ e
    Right GetMeResponse { user_result = u } -> do
      return $ user_first_name u
