module Web.Twitter.Types.Search where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON (..), Value (..), (.:), (.:?))
import Data.Text (Text)

import Web.Twitter.Types.Common
import Web.Twitter.Types.User

data SearchResult body = SearchResult
    { searchResultStatuses :: body
    , searchResultSearchMetadata :: SearchMetadata
    } deriving (Show, Eq)

instance FromJSON body => FromJSON (SearchResult body) where
    parseJSON (Object o) = SearchResult
        <$> o .:  "statuses"
        <*> o .:  "search_metadata"
    parseJSON v = fail $ show v

data SearchStatus = SearchStatus
    { searchStatusCreatedAt :: DateString
    , searchStatusId :: StatusId
    , searchStatusText :: Text
    , searchStatusSource :: Text
    , searchStatusUser :: User
    } deriving (Show, Eq)

instance FromJSON SearchStatus where
    parseJSON (Object o) = SearchStatus
        <$> o .:  "created_at"
        <*> o .:  "id"
        <*> o .:  "text"
        <*> o .:  "source"
        <*> o .:  "user"
    parseJSON v = fail $ show v

data SearchMetadata = SearchMetadata
    { searchMetadataMaxId :: StatusId
    , searchMetadataSinceId :: StatusId
    , searchMetadataRefreshUrl :: UrlString
    , searchMetadataNextResults :: Maybe UrlString
    , searchMetadataCount :: Int
    , searchMetadataCompletedIn :: Maybe Float
    , searchMetadataSinceIdStr :: String
    , searchMetadataQuery :: String
    , searchMetadataMaxIdStr :: String
    } deriving (Show, Eq)

instance FromJSON SearchMetadata where
    parseJSON (Object o) = SearchMetadata
        <$> o .: "max_id"
        <*> o .: "since_id"
        <*> o .: "refresh_url"
        <*> o .:? "next_results"
        <*> o .: "count"
        <*> o .:? "completed_in"
        <*> o .: "since_id_str"
        <*> o .: "query"
        <*> o .: "max_id_str"
    parseJSON v = fail $ show v
