{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NMA (PriorityLevel(..)
           , Notification(..)
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
  deriving(Enum, Show, Read)

-- | Notification to be delivered to an android device.
data Notification = Notification {
  apiKey :: [T.Text]
  , developerKey :: T.Text
  , application :: T.Text
  , description :: T.Text
  , event :: T.Text
  , priority :: PriorityLevel
  , url :: T.Text
  , contentType :: T.Text
  }

params n =
  ["application" := application n,
    "description" := description n,
    "event" := event n,
    "priority" := (subtract 2.fromEnum.priority) n,
    "url" := url n,
    "contentType" := contentType n,
    "developerkey" := developerKey n] <> ["apikey" := x | x <- apiKey n]

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

-- | Send a notification.
notify :: Notification -> IO (Either Response Response)
notify note = do
  r <- post "https://www.notifymyandroid.com/publicapi/notify" (params note)
  guard $ r ^. responseStatus . statusCode == 200
  pure $ parseResponse $ L.toStrict $ r ^. responseBody

-- | Verify credentials.
verify :: Notification -> IO (Either Response Response)
verify note = do
  r <- post "https://www.notifymyandroid.com/publicapi/verify" (params note)
  guard $ r ^. responseStatus . statusCode == 200
  pure $ parseResponse $ L.toStrict $ r ^. responseBody
