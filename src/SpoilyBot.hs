{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module SpoilyBot
    ( startApp
    ) where

import SpoilyBot.Config
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, append, pack)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant ((:>), Get, PlainText, Proxy(..), Server, serve)
import Web.Telegram.API.Bot (GetMeResponse(..), Token(..), getMe, user_first_name, user_result)

type API = "start" :> Get '[PlainText] Text

startApp :: Config -> IO ()
startApp (Config port telegramToken) = run port $ app telegramToken

app :: Token -> Application
app telegramToken = serve api $ server telegramToken

api :: Proxy API
api = Proxy

server :: Token -> Server API
server telegramToken = liftIO $ start telegramToken

start :: Token -> IO Text
start telegramToken = do
  manager <- newManager tlsManagerSettings
  res <- getMe telegramToken manager
  case res of
    Left e -> do
      return . pack . show $ e
    Right GetMeResponse { user_result = u } -> do
      return $ user_first_name u
