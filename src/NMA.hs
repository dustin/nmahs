{-|
Module      : NMA
Description : An interface to NotifyMyAndroid.
Copyright   : (c) Dustin Sallings, 2018
License     : MIT
Maintainer  : dustin@spy.net
Stability   : experimental

This is an interface to NotifyMyAndroid - http://notifymyandroid.com/
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module NMA (
  -- * NotifyMyAndroid API Functions.
  notify
  , verify
  -- * Request Structures
  , Notification(..)
  , notification
  , apiKey, developerKey, application, description, event, priority, url, contentType
  , PriorityLevel(..)
  -- * Response
  , Response(..)
  , msg, code, remaining, timeLeft
  -- * for testing
  , parseResponse
  ) where

import SymEither

import Control.Lens (makeLenses, (<>~), (^.), (%%~))
import Control.Monad (guard)
import Data.Default (Default, def)
import Data.Monoid (Last, getLast)
import Data.Semigroup (Semigroup)
import Generics.Deriving.Base (Generic)
import Generics.Deriving.Monoid (memptydefault, mappenddefault)
import Data.Semigroup ((<>))
import Network.Wreq (post, FormParam(..), responseBody, responseStatus, statusCode)
import Text.Read (readEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Xeno.SAX as X

-- | Priority levels for a notification.
data PriorityLevel = VeryLow | Moderate | Normal | High | Emergency
  deriving(Eq, Bounded, Enum, Show, Read)

{-|
Notification to be delivered to an android device.

Notifications are monoids, allowing you to construct them in two
different parts of your application and combine them naturally.

e.g., you may set up the apiKey(s) and developerKey in initialization,
and then add notification details as you go.

@
λ> nma = def & apiKey .~ ["somekey"] & developerKey .~ "devkey" & application .~ "haskell"
λ> note = def & event .~ "Some Event" & description .~ "things are broken" & priority .~ pure Emergency
λ> nma <> note
Notification {_apiKey = ["somekey"], _developerKey = "devkey",
              _application = "haskell", _description = "things are broken", _event = "Some Event",
              _priority = Last {getLast = Just Emergency}, _url = "", _contentType = ""}
@
-}
data Notification = Notification {
  _apiKey :: [T.Text]
  , _developerKey :: T.Text
  , _application :: T.Text
  , _description :: T.Text
  , _event :: T.Text
  , _priority :: Last PriorityLevel
  , _url :: T.Text
  , _contentType :: T.Text
  }
  deriving (Eq, Show, Generic)

makeLenses ''Notification

instance Monoid Notification where
  mempty  = memptydefault
  mappend = mappenddefault

instance Semigroup Notification

instance Default Notification where
  def = mempty

-- | A default (empty) notification from which you can more easily build your notification.
notification :: Notification
notification = mempty

params :: Notification -> [FormParam]
params n =
  ["application" := n ^. application,
    "description" := n ^. description,
    "event" := n ^. event,
    "url" := n ^. url,
    "contentType" := n ^. contentType,
    "developerkey" := n ^. developerKey]
  <> ["apikey" := x | x <- n ^. apiKey]
  <> case getLast (n ^. priority) of
       Nothing -> []
       Just x -> ["priority" := (subtract 2.fromEnum) x]

-- Msg, Calls Remaining, Time Left.
data Response = Response { _msg :: T.Text, _code :: Int, _remaining :: Int, _timeLeft :: Int }
  deriving (Show, Eq)

makeLenses ''Response

-- | Parse an XML response from NotifyMyAndroid.
parseResponse :: BS.ByteString -> Either Response Response
parseResponse b =
  case X.fold open attr end txt close cdata (pure $ Response "" 0 0 0) b of
    Left x -> Left (Response ((T.pack . show) x) 0 0 0)
    Right s -> toEither s

  where open = const

        attr :: SymEither Response -> C.ByteString -> C.ByteString -> SymEither Response
        attr r "remaining" = eread r remaining
        attr r "resettimer" = eread r timeLeft
        attr r "code" = eread r code
        attr r _ = const r

        -- eread :: SymEither Response -> Simple Lens Response Int -> C.ByteString -> SymEither Response
        eread re f c = re >>= f %%~ (const.fromEither 0.readEither.C.unpack) c

        end = const

        txt wr x = (msg <>~ (T.strip . T.pack . C.unpack) x) <$> wr

        close wr "error" = left wr
        close x _ = x

        cdata = const

transmit :: String -> Notification -> IO (Either Response Response)
transmit u note = do
  r <- post u (params note)
  guard $ r ^. responseStatus . statusCode == 200
  pure $ parseResponse $ L.toStrict $ r ^. responseBody

{-|
Send a notification.

e.g.,

> notify $ Notification ["myapikey"] "mydevkey" "ghci" "Hello" "Test" (pure Normal) "" ""
-}
notify :: Notification -> IO (Either Response Response)
notify = transmit "https://www.notifymyandroid.com/publicapi/notify"

{-|
Verify credentials.  Verify uses the same parameter type as notification, but most of
the fields aren't required.  e.g.:

> verify $ def & apiKey .~ ["myapikey"]
-}
verify :: Notification -> IO (Either Response Response)
verify = transmit "https://www.notifymyandroid.com/publicapi/verify"
