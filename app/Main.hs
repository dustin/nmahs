{-# LANGUAGE OverloadedStrings #-}

module Main where

import NMA

import Options.Applicative
import Data.Semigroup ((<>))
import System.Exit (die, exitSuccess)
import qualified Data.Text as T

data Options = Options { optAPIKey       :: T.Text
                       , optDeveloperKey :: T.Text
                       , optApplication  :: T.Text
                       , optDescription  :: T.Text
                       , optEvent        :: T.Text
                       , optPriority     :: PriorityLevel
                       , optURL          :: T.Text
                       , optContentType  :: T.Text
                       }

options :: Parser Options
options = Options
  <$> strOption (long "apikey" <> help "NMA API Key")
  <*> strOption (long "devkey" <> value "" <> help "NMA Dev Key")
  <*> strOption (long "app" <> showDefault <> value "haskell" <> help "Application")
  <*> strOption (long "desc" <> value "" <> help "Description")
  <*> strOption (long "event" <> value "" <> help "Event")
  <*> option auto (long "priority" <> showDefault <> value Moderate <> help "Priority")
  <*> strOption (long "url" <> value "" <> help "URL")
  <*> strOption (long "contentType" <> value "" <> help "Content Type")

go :: Options -> IO ()
go opts =
  do
    let nma = NMA [optAPIKey opts] (optDeveloperKey opts)
    let note = Notification{
          application=optApplication opts,
          description=optDescription opts,
          event=optEvent opts,
          priority=optPriority opts,
          url=optURL opts,
          contentType=optContentType opts
          }
    res <- notify nma note
    case res of
      Left x -> (die.show) x
      Right _ -> pure ()

main :: IO ()
main = go =<< execParser opts
  where opts = info (options <**> helper)
               ( fullDesc <> progDesc "NotifyMyAndroid")
