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
  let port = decidePort portEnvar
  either die startApp port

decidePort :: Maybe String -> Either String Int
decidePort envar = maybe (Right defaultPort) parsePort envar

parsePort :: String -> Either String Int
parsePort portStr = maybe (left portStr) Right (readMaybe portStr)
  where
    left portStr = Left $ portStr ++ " is not valid port"

portEnvarName :: String
portEnvarName = "SPOILY_PORT"

defaultPort :: Int
defaultPort = 8080
