{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NMA (PriorityLevel(..)
           , Notification(..)
           , apiKey, developerKey, application, description, event, priority, url, contentType
           , Response(..)
           , remaining, msg, timeLeft
           , notify
           , verify
           -- for testing
           , parseResponse
           ) where

import SymEither

import Control.Lens
import Control.Lens.TH
import Control.Monad (guard)
import Data.Semigroup (Semigroup, (<>))
import Network.Wreq (post, FormParam(..), responseBody, responseStatus, statusCode)
import Text.Read (readEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Xeno.SAX as X

-- | Priority levels for a notification.
data PriorityLevel = Undefined | VeryLow | Moderate | Normal | High | Emergency
  deriving(Eq, Bounded, Enum, Show, Read)

instance Monoid PriorityLevel where
  mappend a Undefined = a
  mappend _ b = b
  mempty = Undefined

instance Semigroup PriorityLevel

-- | Notification to be delivered to an android device.
data Notification = Notification {
  _apiKey :: [T.Text]
  , _developerKey :: T.Text
  , _application :: T.Text
  , _description :: T.Text
  , _event :: T.Text
  , _priority :: PriorityLevel
  , _url :: T.Text
  , _contentType :: T.Text
  }
  deriving (Eq, Show)

makeLenses ''Notification

instance Monoid Notification where
  mappend a b = a & apiKey <>~ b ^. apiKey
    & developerKey <>~ b ^. developerKey
    & application <>~ b ^. application
    & description <>~ b ^. description
    & event <>~ b ^. event
    & priority <>~ b ^. priority
    & url <>~ b ^. url
    & contentType <>~ b ^. contentType

  mempty = Notification mempty mempty mempty mempty mempty mempty mempty mempty

instance Semigroup Notification

params :: Notification -> [FormParam]
params n =
  ["application" := n ^. application,
    "description" := n ^. description,
    "event" := n ^. event,
    "url" := n ^. url,
    "contentType" := n ^. contentType,
    "developerkey" := n ^. developerKey]
  <> ["apikey" := x | x <- n ^. apiKey]
  <> case n ^. priority of
       Undefined -> []
       x -> ["priority" := (subtract 3.fromEnum) x]

-- Msg, Calls Remaining, Time Left.
data Response = Response { _msg :: T.Text, _remaining :: Int, _timeLeft :: Int }
  deriving (Show, Eq)

makeLenses ''Response

-- | Parse an XML response from NotifyMyAndroid.
parseResponse :: BS.ByteString -> Either Response Response
parseResponse b =
  case X.fold open attr end txt close cdata (pure $ Response "" 0 0) b of
    Left x -> Left (Response ((T.pack . show) x) 0 0)
    Right s -> toEither s

  where open = const

        attr :: SymEither Response -> C.ByteString -> C.ByteString -> SymEither Response
        attr r "remaining" = eread r remaining
        attr r "resettimer" = eread r timeLeft
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

-- | Send a notification.
notify :: Notification -> IO (Either Response Response)
notify = transmit "https://www.notifymyandroid.com/publicapi/notify"

-- | Verify credentials.
verify :: Notification -> IO (Either Response Response)
verify = transmit "https://www.notifymyandroid.com/publicapi/verify"

