{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module SpoilyBot
    ( startApp
    ) where

import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type API = "start" :> Get '[PlainText] Text

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return start

start :: Text
start = pack ""
