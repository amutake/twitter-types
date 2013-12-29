module Web.Twitter.Types.User where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON (..), Value (..), (.:), (.:?))
import Data.Text (Text)

import Web.Twitter.Types.Common

type ColorCode = Text -- TODO

-- | <https://dev.twitter.com/docs/platform-objects/users> 2013-05-20 07:28
data User = User
    { userId :: UserId
    , userIdStr :: String
    , userName :: UserName
    , userScreenName :: ScreenName
    , userLocation :: Text
    , userUrl :: Maybe UrlString
    , userDescription :: Maybe Text
    , userProtected :: Bool
    , userFollowersCount :: Int
    , userFriendsCount :: Int
    , userListedCount :: Int
    , userCreatedAt :: DateString
    , userFavouritesCount :: Int -- ^ British spelling used in the field name for historical reasons.
    , userUtcOffset :: Int
    , userTimeZone :: Text -- TODO
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
    , userProfileLinkColor :: ColorCode
    , userProfileSidebarBorderColor :: ColorCode
    , userProfileSidebarFillColor :: ColorCode
    , userProfileTextColor :: ColorCode
    , userProfileUseBackgroundImage :: Bool
    , userDefaultProfile :: Bool
    , userDefaultProfileImage :: Bool
    , userFollowing :: Maybe Value -- TODO
    , userFollowRequestSent :: Maybe Value -- TODO
    , userNotifications :: Maybe Value -- TODO
    } deriving (Show, Eq)

instance FromJSON User where
    parseJSON (Object o) = User
        <$> o .: "id"
        <*> o .: "id_str"
        <*> o .: "name"
        <*> o .: "screen_name"
        <*> o .: "location"
        <*> o .: "url"
        <*> o .:? "description"
        <*> o .: "protected"
        <*> o .: "followers_count"
        <*> o .: "friends_count"
        <*> o .: "listed_count"
        <*> o .: "created_at"
        <*> o .: "favourites_count"
        <*> o .: "utc_offset"
        <*> o .: "time_zone"
        <*> o .: "geo_enabled"
        <*> o .: "verified"
        <*> o .: "statuses_count"
        <*> o .: "lang"
        <*> o .: "contributors_enabled"
        <*> o .: "is_translator"
        <*> o .: "profile_background_color"
        <*> o .: "profile_background_image_url"
        <*> o .: "profile_background_url_https"
        <*> o .: "profile_background_tile"
        <*> o .: "profile_image_url"
        <*> o .: "profile_image_url_https"
        <*> o .: "profile_link_color"
        <*> o .: "profile_sidebar_border_color"
        <*> o .: "profile_sidebar_fill_color"
        <*> o .: "profile_text_color"
        <*> o .: "profile_use_background_image"
        <*> o .: "default_profile"
        <*> o .: "default_profile_image"
        <*> o .: "following"
        <*> o .: "follow_request_sent"
        <*> o .: "notifications"
    parseJSON v = fail $ show v
