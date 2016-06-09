module Main where

import Data.Either (either)
import Data.Maybe (maybe)
import System.Environment (lookupEnv)
import System.Exit (die)
import Text.Read (readMaybe)
import SpoilyBot (startApp)


main :: IO ()
main = do
  portEnvar <- lookupEnv portEnvarName
  let errorOrPort = decidePort portEnvar
  telegramTokenEnvar <- lookupEnv telegramTokenEnvarName
  case telegramTokenEnvar of
    Nothing -> die "Telegram token is not set"
    Just telegramToken -> either
      die
      (\port -> startApp port telegramToken)
      errorOrPort

decidePort :: Maybe String -> Either String Int
decidePort envar = maybe (Right defaultPort) parsePort envar

parsePort :: String -> Either String Int
parsePort portStr = maybe (left portStr) Right (readMaybe portStr)
  where
    left portStr = Left $ portStr ++ " is not valid port"

defaultPort :: Int
defaultPort = 8080

portEnvarName :: String
portEnvarName = "SPOILY_PORT"

telegramTokenEnvarName :: String
telegramTokenEnvarName = "SPOILY_TELEGRAM_TOKEN"
