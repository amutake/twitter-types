module Web.Twitter.Types.User
    ( User (..)
    , UserEntities (..)
    , Account (..)
    , TimeZone (..)
    , SleepTime (..)
    , TrendLocation (..)
    , PlaceType (..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON (..), Value (..), (.:), (.:?))
import Data.Maybe (maybeToList)
import Data.Text (Text)

import Web.Twitter.Types.Common
import Web.Twitter.Types.Entities
import Web.Twitter.Types.Internal

type ColorCode = Text -- TODO

-- | <https://dev.twitter.com/docs/platform-objects/users> 2013-05-20 07:28
data User = User
    { userId :: UserId
    , userIdStr :: String
    , userName :: UserName
    , userScreenName :: ScreenName
    , userLocation :: Maybe Text
    , userUrl :: Maybe UrlString
    , userDescription :: Maybe Text
    , userProtected :: Bool
    , userFollowersCount :: Int
    , userFriendsCount :: Int
    , userListedCount :: Int
    , userCreatedAt :: UTCTime
    , userFavouritesCount :: Int -- ^ British spelling used in the field name for historical reasons.
    , userUtcOffset :: Maybe Int
    , userTimeZone :: Maybe Text -- TODO
    , userGeoEnabled :: Bool
    , userVerified :: Bool
    , userStatusesCount :: Int
    , userLang :: LanguageCode
    , userContributorsEnabled :: Bool
    , userIsTranslator :: Bool
    , userProfileBackgroundColor :: ColorCode
    , userProfileBackgroundImageUrl :: UrlString
    , userProfileBackgroundImageUrlHttps :: UrlString
    , userProfileBackgroundTile :: Bool
    , userProfileImageUrl :: UrlString
    , userProfileImageUrlHttps :: UrlString
    , userProfileBannerUrl :: Maybe UrlString
    , userProfileLinkColor :: ColorCode
    , userProfileSidebarBorderColor :: ColorCode
    , userProfileSidebarFillColor :: ColorCode
    , userProfileTextColor :: ColorCode
    , userProfileUseBackgroundImage :: Bool
    , userDefaultProfile :: Bool
    , userDefaultProfileImage :: Bool
    , userFollowing :: Maybe Bool
    , userFollowRequestSent :: Maybe Bool
    , userNotifications :: Maybe Bool
    , userEntities :: Maybe UserEntities
    } deriving (Show, Eq)

instance FromJSON User where
    parseJSON (Object o) = User
        <$> o .: "id"
        <*> o .: "id_str"
        <*> o .: "name"
        <*> o .: "screen_name"
        <*> o .:? "location"
        <*> o .:? "url"
        <*> o .:? "description"
        <*> o .: "protected"
        <*> o .: "followers_count"
        <*> o .: "friends_count"
        <*> o .: "listed_count"
        <*> parseUTCTime o "created_at"
        <*> o .: "favourites_count"
        <*> o .:? "utc_offset"
        <*> o .: "time_zone"
        <*> o .: "geo_enabled"
        <*> o .: "verified"
        <*> o .: "statuses_count"
        <*> o .: "lang"
        <*> o .: "contributors_enabled"
        <*> o .: "is_translator"
        <*> o .: "profile_background_color"
        <*> o .: "profile_background_image_url"
        <*> o .: "profile_background_image_url_https"
        <*> o .: "profile_background_tile"
        <*> o .: "profile_image_url"
        <*> o .: "profile_image_url_https"
        <*> o .:? "profile_banner_url"
        <*> o .: "profile_link_color"
        <*> o .: "profile_sidebar_border_color"
        <*> o .: "profile_sidebar_fill_color"
        <*> o .: "profile_text_color"
        <*> o .: "profile_use_background_image"
        <*> o .: "default_profile"
        <*> o .: "default_profile_image"
        <*> o .:? "following"
        <*> o .:? "follow_request_sent"
        <*> o .:? "notifications"
        <*> o .:? "entities"
    parseJSON v = fail $ show v

data UserEntities = UserEntities
    { userEntitiesDescription :: Entities
    } deriving (Show, Eq)

instance FromJSON UserEntities where
    parseJSON (Object o) = UserEntities
        <$> o .: "description"
    parseJSON v = fail $ show v

data Account = Account
    { accountTimeZone :: TimeZone
    , accountProtected :: Bool
    , accountScreenName :: ScreenName
    , accountAlwaysUseHttps :: Bool
    , accountUseCookiePersonalization :: Bool
    , accountSleepTime :: SleepTime
    , accountGeoEnabled :: Bool
    , accountLanguage :: LanguageCode
    , accountDiscoverableByEmail :: Bool
    , accountDiscoverableByMobilePhone :: Maybe Bool
    , accountDisplaySensitiveMedia :: Bool
    , accountTrendLocation :: [TrendLocation]
    } deriving (Show, Eq)

instance FromJSON Account where
    parseJSON (Object o) = Account
        <$> o .: "time_zone"
        <*> o .: "protected"
        <*> o .: "screen_name"
        <*> o .: "always_use_https"
        <*> o .: "use_cookie_personalization"
        <*> o .: "sleep_time"
        <*> o .: "geo_enabled"
        <*> o .: "language"
        <*> o .: "discoverable_by_email"
        <*> o .:? "discoverable_by_mobile_phone"
        <*> o .: "display_sensitive_media"
        <*> (concat . maybeToList <$> o .:? "trend_location")
    parseJSON v = fail $ show v

data TimeZone = TimeZone
    { timeZoneName :: Text
    , timeZoneUtcOffset :: Int
    , timeZoneTzInfoName :: Text
    } deriving (Show, Eq)

instance FromJSON TimeZone where
    parseJSON (Object o) = TimeZone
        <$> o .: "name"
        <*> o .: "utc_offset"
        <*> o .: "tzinfo_name"
    parseJSON v = fail $ show v

data SleepTime = SleepTime
    { sleepTimeEnabled :: Bool
    , sleepTimeStartTime :: Maybe Int
    , sleepTimeEndTime :: Maybe Int
    } deriving (Show, Eq)

instance FromJSON SleepTime where
    parseJSON (Object o) = SleepTime
        <$> o .: "enabled"
        <*> o .:? "end_time"
        <*> o .:? "start_time"
    parseJSON v = fail $ show v

data TrendLocation = TrendLocation
    { trendLocationName :: Text
    , trendLocationCountryCode :: Maybe Value -- TODO
    , trendLocationCountry :: Text
    , trendParentId :: Maybe Int
    , trendLocationUrl :: UrlString
    , trendLocationPlaceType :: PlaceType
    , trendLocationWoeId :: Int
    } deriving (Show, Eq)

instance FromJSON TrendLocation where
    parseJSON (Object o) = TrendLocation
        <$> o .: "name"
        <*> o .:? "countryCode"
        <*> o .: "country"
        <*> o .:? "parentid"
        <*> o .: "url"
        <*> o .: "placeType"
        <*> o .: "woeid"
    parseJSON v = fail $ show v

data PlaceType = PlaceType
    { placeTypeName :: Text
    , placeTypeCode :: Int
    } deriving (Show, Eq)

instance FromJSON PlaceType where
    parseJSON (Object o) = PlaceType
        <$> o .: "name"
        <*> o .: "code"
    parseJSON v = fail $ show v
