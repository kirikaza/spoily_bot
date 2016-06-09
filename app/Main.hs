module Main where

import SpoilyBot.Config (loadConfig)
import SpoilyBot (startApp)

import Data.Either (either)
import System.Exit (die)

main :: IO ()
main = do
  config <- loadConfig
  either die startApp config
