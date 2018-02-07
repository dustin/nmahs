{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NMA (PriorityLevel(..)
           , Notification(..)
           , NMA(..)
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
  application :: T.Text
  , description :: T.Text
  , event :: T.Text
  , priority :: PriorityLevel
  , url :: T.Text
  , contentType :: T.Text
  }

notifparams n =
  ["application" := application n,
    "description" := description n,
    "event" := event n,
    "priority" := (subtract 2.fromEnum.priority) n,
    "url" := url n,
    "contentType" := contentType n]

-- | Common NMA client info.
data NMA = NMA {
  apiKey :: [T.Text]
  , developerKey :: T.Text
  }

nmaparams n = ["apikey" := x | x <- apiKey n] <> ["developerkey" := developerKey n]

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
notify :: NMA -> Notification -> IO (Either Response Response)
notify nma not = do
  let opts = nmaparams nma <> notifparams not
  r <- post "https://www.notifymyandroid.com/publicapi/notify" opts
  guard $ r ^. responseStatus . statusCode == 200
  pure $ parseResponse $ L.toStrict $ r ^. responseBody

-- | Verify credentials.
verify :: NMA -> IO (Either Response Response)
verify nma = do
  r <- post "https://www.notifymyandroid.com/publicapi/verify" (nmaparams nma)
  guard $ r ^. responseStatus . statusCode == 200
  pure $ parseResponse $ L.toStrict $ r ^. responseBody
