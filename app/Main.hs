{-# LANGUAGE OverloadedStrings #-}

module Main where

import NMA

import Options.Applicative (Parser, option, maybeReader, strOption, long, help, fullDesc,
                            showDefault, value, execParser, info, progDesc, helper, (<**>))
import Data.Semigroup ((<>))
import System.Exit (die)
import qualified Data.Text as T

options :: Parser Notification
options = Notification
  <$> option (maybeReader $ pure.pure.T.pack) (long "apikey" <> help "NMA API Key")
  <*> strOption (long "devkey" <> value "" <> help "NMA Dev Key")
  <*> strOption (long "app" <> showDefault <> value "haskell" <> help "Application")
  <*> strOption (long "desc" <> value "" <> help "Description")
  <*> strOption (long "event" <> value "" <> help "Event")
  <*> option (maybeReader $ pure.pure.read) (long "priority" <> help "Priority")
  <*> strOption (long "url" <> value "" <> help "URL")
  <*> strOption (long "contentType" <> value "" <> help "Content Type")

go :: Notification -> IO ()
go note =
  do
    res <- notify note
    case res of
      Left x -> (die.show) x
      Right _ -> pure ()

main :: IO ()
main = go =<< execParser opts
  where opts = info (options <**> helper)
               ( fullDesc <> progDesc "NotifyMyAndroid")

