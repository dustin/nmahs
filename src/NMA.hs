{-# LANGUAGE OverloadedStrings #-}

module NMA where

import Control.Lens
import Control.Monad (guard)
import Data.Semigroup ((<>))
import Network.Wreq (post, FormParam(..), responseBody, responseStatus, statusCode)
import Text.Read (readEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Xeno.SAX as X

data PriorityLevel = VeryLow | Moderate | Normal | High | Emergency
  deriving(Enum)

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

data NMA = NMA {
  apiKey :: [T.Text]
  , developerKey :: T.Text
  }

instance Paramer NMA where
  params n = ["apikey" := x | x <- apiKey n] <> ["developerkey" := developerKey n]

-- Msg, Calls Remaining, Time Left.
data Response = Response { msg :: T.Text, remaining :: Int, timeLeft :: Int }
  deriving (Show)

parseResponse :: BS.ByteString -> Either String Response
parseResponse b =
  case X.fold open attr end txt close cdata (Right $ Response "" 0 0) b of
    Left x -> (Left . show) x
    Right s -> s

  where open = const

        attr (Right r) "remaining" x = (\n -> r { remaining = n}) <$> eread x
        attr (Right r) "resettimer" x = (\n -> r { timeLeft = n}) <$> eread x
        attr r _ _ = r

        -- attr helper
        eread = readEither . C.unpack

        end = const

        txt (Right (Response m c t)) x = Right (Response (m <> (T.pack . C.unpack) x) c t)
        txt x _ = x

        close (Right (Response m _ t)) "error" = Left (T.unpack m <> " - " <> show t <> "s left")
        close x _ = x

        cdata = const

notify :: NMA -> Notification -> IO (Either String Response)
notify nma not = do
  let opts = params nma <> params not
  r <- post "https://www.notifymyandroid.com/publicapi/notify" opts
  guard $ r ^. responseStatus . statusCode == 200
  pure $ parseResponse $ L.toStrict $ r ^. responseBody
