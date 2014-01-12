module Web.Twitter.Types.Internal
    ( parseUTCTime
    , UTCTime (..)
    ) where

import Control.Applicative ((<$>))
import Data.Aeson.Types (Object, Parser, (.:))
import Data.Text (Text)
import System.Locale (defaultTimeLocale)
import Data.Time (parseTime, UTCTime (..))

parseUTCTime :: Object -> Text -> Parser UTCTime
parseUTCTime o name = maybe (error "failed to parse UTCTime") id <$> parseUTCTimeMaybe o name

parseUTCTimeMaybe :: Object -> Text -> Parser (Maybe UTCTime)
parseUTCTimeMaybe o name = parseTime defaultTimeLocale "%c" <$> o .: name
