module Web.Twitter.Types.Entities
    ( Entities (..)
    , HashTag (..)
    , Url (..)
    , UserMention (..)
    , Media (..)
    , MediaType (..)
    , MediaId
    , MediaSizes (..)
    , MediaSize (..)
    , MediaSizeResize (..)
    ) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Aeson (FromJSON (..), Value (..), withText, (.:), (.:?))
import Data.Int (Int64)
import Data.Maybe (maybeToList)
import Data.Text (Text, unpack)

import Web.Twitter.Types.Common

-- | <https://dev.twitter.com/docs/platform-objects/entities> 2013-12-16 15:46
--   done.
data Entities = Entities
    { entityHashTags :: [HashTag]
    , entitySymbols :: [Value] -- TODO
    , entityURLs :: [Url]
    , entityUserMentions :: [UserMention]
    , entityMedia :: [Media]
    } deriving (Show, Eq)

instance FromJSON Entities where
    parseJSON (Object o) = Entities
        <$> l "hashtags"
        <*> l "symbols"
        <*> l "urls"
        <*> l "user_mentions"
        <*> l "media"
      where
        l t = concat . maybeToList <$> o .:? t
    parseJSON v = fail $ "entities" ++ show v

-- | done.
data HashTag = HashTag
    { hashTagText :: Text -- ^ The Hashtag text
    , hashTagIndices :: [Int]
    } deriving (Show, Eq)

instance FromJSON HashTag where
    parseJSON (Object o) = HashTag
        <$> o .: "text"
        <*> o .: "indices"
    parseJSON v = fail $ show v

-- | done.
data Url = Url
    { urlUrl :: UrlString -- ^ The URL that was extracted
    , urlDisplayUrl :: UrlString -- ^ Not a URL but a string to display instead of the URL (only for t.co links)
    , urlExpandedUrl :: UrlString -- ^ The fully resolved URL (only for t.co links)
    , urlIndices :: [Int]
    } deriving (Show, Eq)

instance FromJSON Url where
    parseJSON (Object o) = Url
        <$> o .: "url"
        <*> o .: "display_url"
        <*> o .: "expanded_url"
        <*> o .: "indices"
    parseJSON v = fail $ show v

-- | done.
data UserMention = UserMention
    { userMentionId :: UserId
    , userMentionIdStr :: String
    , userMentionScreenName :: ScreenName
    , userMentionName :: UserName
    , userMentionIndices :: [Int]
    } deriving (Show, Eq)

instance FromJSON UserMention where
    parseJSON (Object o) = UserMention
        <$> o .: "id"
        <*> o .: "id_str"
        <*> o .: "screen_name"
        <*> o .: "name"
        <*> o .: "indices"
    parseJSON v = fail $ show v

-- | <https://dev.twitter.com/docs/platform-objects/entities#obj-media>
--   done.
data Media = Media
    { mediaId :: MediaId
    , mediaIdStr :: String
    , mediaMediaUrl :: UrlString
    , mediaMediaUrlHttps :: UrlString
    , mediaUrl :: UrlString
    , mediaDisplayUrl :: UrlString
    , mediaExpandedUrl :: UrlString
    , mediaType :: MediaType
    , mediaSizes :: MediaSizes
    , mediaIndices :: [Int]
    } deriving (Show, Eq)

instance FromJSON Media where
    parseJSON (Object o) = Media
        <$> o .: "type"
        <*> o .: "id"
        <*> o .: "id_str"
        <*> o .: "media_url"
        <*> o .: "media_url_https"
        <*> o .: "url"
        <*> o .: "display_url"
        <*> o .: "expanded_url"
        <*> o .: "sizes"
        <*> o .: "indices"
    parseJSON v = fail $ show v

-- | done.
data MediaType = Photo deriving (Show, Eq)

instance FromJSON MediaType where
    parseJSON = withText "Text" f
      where
        f "photo" = pure Photo
        f t = fail $ "Unknown media type: " ++ unpack t

type MediaId = Int64

-- | <https://dev.twitter.com/docs/platform-objects/entities#obj-sizes>
data MediaSizes = MediaSizes
    { mediaSizeSmall :: MediaSize
    , mediaSizeMedium :: MediaSize
    , mediaSizeLarge :: MediaSize
    , mediaSizeThumb :: MediaSize
    } deriving (Show, Eq)

instance FromJSON MediaSizes where
    parseJSON (Object o) = MediaSizes
        <$> o .: "small"
        <*> o .: "medium"
        <*> o .: "large"
        <*> o .: "thumb"
    parseJSON v = fail $ show v

-- | <https://dev.twitter.com/docs/platform-objects/entities#obj-size>
data MediaSize = MediaSize
    { mediaSizeWidth :: Int
    , mediaSizeHeight :: Int
    , mediaSizeResize :: MediaSizeResize
    } deriving (Show, Eq)

instance FromJSON MediaSize where
    parseJSON (Object o) = MediaSize
        <$> o .: "w"
        <*> o .: "h"
        <*> o .: "resize"
    parseJSON v = fail $ show v

-- | done.
data MediaSizeResize
    = MediaSizeResizeFit
    | MediaSizeResizeCrop
    deriving (Show, Eq)

instance FromJSON MediaSizeResize where
    parseJSON = withText "Text" f
      where
        f "fit" = pure MediaSizeResizeFit
        f "crop" = pure MediaSizeResizeCrop
        f t = fail $ "Unknown media resize: " ++ unpack t
