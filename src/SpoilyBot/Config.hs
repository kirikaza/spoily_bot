module SpoilyBot.Config
  ( Config(..)
  , Port
  , Token(..)
  , loadConfig
  , parseConfig
  )
where

import Network.Wai.Handler.Warp (Port)
import Web.Telegram.API.Bot (Token(..))

import qualified Data.Text as Text
import qualified System.Environment as Env
import qualified Text.Read as Read

data Config = Config
  { port :: Port
  , telegramToken :: Token
  }

instance Show Config where
  show (Config port (Token telegramToken)) = concat
    [ "Config {port = "
    , show port
    ,  ", telegramToken = "
    , show . mask $ telegramToken
    ]
    where
      mask secret = Text.concat
        [ Text.take 3 secret
        , Text.pack "..." 
        , Text.takeEnd 3 secret
        ]

loadConfig :: IO (Either String Config)
loadConfig = do
  portEnvar <- Env.lookupEnv portEnvarName
  telegramTokenEnvar <- Env.lookupEnv telegramTokenEnvarName
  return $ case (portEnvar, telegramTokenEnvar) of
    (Nothing, _) -> Left $ "specify port by environment variable " ++ portEnvarName
    (_, Nothing) -> Left $ "specify Telegram token by environment variable " ++ telegramTokenEnvarName
    (Just portStr, Just telegramToken) -> parseConfig portStr telegramToken
 

parseConfig :: String -> String -> Either String Config
parseConfig portStr telegramToken = case Read.readMaybe portStr of
  Nothing -> Left $ "value `" ++ portStr ++ "' is not a valid port"
  Just port -> Right $ Config port (Token . Text.pack $ telegramToken)

portEnvarName :: String
portEnvarName = "SPOILY_PORT"

telegramTokenEnvarName :: String
telegramTokenEnvarName = "SPOILY_TELEGRAM_TOKEN"
