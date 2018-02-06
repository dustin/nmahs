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
  deriving(Enum)

-- | Notification to be delivered to an android device.
data Notification = Notification {
  application :: T.Text
  , description :: T.Text
  , event :: T.Text
  , priority :: PriorityLevel
  , url :: T.Text
  , contentType :: T.Text
  }

class Paramer a where
  params :: a -> [FormParam]

instance Paramer Notification where
  params n =
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

instance Paramer NMA where
  params n = ["apikey" := x | x <- apiKey n] <> ["developerkey" := developerKey n]

-- Msg, Calls Remaining, Time Left.
data Response = Response { _msg :: T.Text, _remaining :: Int, _timeLeft :: Int }
  deriving (Show, Eq)

makeLenses ''Response

-- A couple helpers for symmetrical Eithers.

-- smap is like fmap, but doesn't care whether it's operating on Left or Right
smap :: (a -> a) -> Either a a -> Either a a
smap f (Right x) = Right (f x)
smap f (Left x) = Left (f x)

-- left shifts a Right x to a Left x
left :: Either a a -> Either a a
left (Right x) = Left x
left x = x


-- | Parse an XML response from NotifyMyAndroid.
parseResponse :: BS.ByteString -> Either Response Response
parseResponse b =
  case X.fold open attr end txt close cdata (Right $ Response "" 0 0) b of
    Left x -> Left (Response ((T.pack . show) x) 0 0)
    Right s -> s

  where open = const

        attr :: Either Response Response -> C.ByteString -> C.ByteString -> Either Response Response
        attr r "remaining" = eread r remaining
        attr r "resettimer" = eread r timeLeft
        attr r _ = const r

        -- Either Response Response -> Simple Lens Response Int -> C.ByteString -> Either Response Response
        eread re f c = re >>= f %%~ (const.estr.readEither.C.unpack) c
          where estr (Left x) = Left (Response (T.pack x) 0 0)
                estr (Right x) = Right x

        end = const

        txt wr x = smap (msg <>~ (T.strip . T.pack . C.unpack) x) wr

        close wr "error" = left wr
        close x _ = x

        cdata = const

-- | Send a notification.
notify :: NMA -> Notification -> IO (Either Response Response)
notify nma not = do
  let opts = params nma <> params not
  r <- post "https://www.notifymyandroid.com/publicapi/notify" opts
  guard $ r ^. responseStatus . statusCode == 200
  pure $ parseResponse $ L.toStrict $ r ^. responseBody

-- | Verify credentials.
verify :: NMA -> IO (Either Response Response)
verify nma = do
  r <- post "https://www.notifymyandroid.com/publicapi/verify" (params nma)
  guard $ r ^. responseStatus . statusCode == 200
  pure $ parseResponse $ L.toStrict $ r ^. responseBody
