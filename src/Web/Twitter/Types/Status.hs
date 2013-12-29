module Web.Twitter.Types.Status where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON (..), Value (..), (.:), (.:?))
import Data.Text (Text)

import Web.Twitter.Types.Common
import Web.Twitter.Types.Entities
import Web.Twitter.Types.User

-- | <https://dev.twitter.com/docs/platform-objects/tweets> 2013-08-13 16:29
data Status = Status
    { statusCreatedAt :: DateString
    , statusId :: StatusId
    , statusIdStr :: String
    , statusText :: Text
    , statusSource :: Text
    , statusTruncated :: Bool
    , statusInReplyToStatusId :: Maybe StatusId
    , statusInReplyToStatusIdStr :: Maybe String
    , statusInReplyToUserId :: Maybe UserId
    , statusInReplyToUserIdStr :: Maybe String
    , statusInReplyToScreenName :: Maybe ScreenName
    , statusUser :: User
    , statusGeo :: Maybe Text -- TODO
    , statusCoordinates :: Maybe Coordinates
    , statusPlace :: Maybe Place
    , statusContributors :: Maybe Contributors
    , statusRetweetedStatus :: Maybe Status
    , statusRetweetCount :: Int
    , statusFavoriteCount :: Maybe Int
    , statusEntities :: Entities
    , statusFavorited :: Bool
    , statusRetweeted :: Bool
    , statusPossiblySensitive :: Maybe Bool
    , statusFilterLevel :: Maybe Text -- TODO
    , statusLang :: Maybe Text -- TODO
    } deriving (Show, Eq)

instance FromJSON Status where
    parseJSON (Object o) = Status
         <$> o .: "created_at"
         <*> o .: "id"
         <*> o .: "id_str"
         <*> o .: "text"
         <*> o .: "source"
         <*> o .: "truncated"
         <*> o .:? "in_reply_to_status_id"
         <*> o .:? "in_reply_to_status_id_str"
         <*> o .:? "in_reply_to_user_id"
         <*> o .:? "in_reply_to_user_id_str"
         <*> o .:? "in_reply_to_screen_name"
         <*> o .: "user"
         <*> o .:? "geo"
         <*> o .:? "coordinates"
         <*> o .:? "place"
         <*> o .:? "contributors"
         <*> o .:? "retweeted_status"
         <*> o .: "retweet_count"
         <*> o .:? "favorite_count"
         <*> o .: "entities"
         <*> o .: "favorited"
         <*> o .: "retweeted"
         <*> o .:? "possibly_sensitive"
         <*> o .:? "filter_level"
         <*> o .:? "lang"
    parseJSON v = fail $ show v

type Coordinates = Value -- TODO
type Place = Value -- TODO
type Contributors = Value -- TODO
