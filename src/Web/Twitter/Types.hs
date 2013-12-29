{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Web.Twitter.Types
    ( DateString
    , UserId
    , Friends
    , URIString
    , UserName
    , ScreenName
    , StatusId
    , LanguageCode
    , StreamingAPI (..)
    , UserStream (..)
    , Status (..)
    , Ids (..)
    , SearchResult (..)
    , SearchStatus (..)
    , SearchMetadata (..)
    , RetweetedStatus (..)
    , DirectMessage (..)
    , EventTarget (..)
    , Event (..)
    , StatusDeletion (..)
    , User (..)
    , List (..)
    , Entities (..)
    , EntityIndices
    , Entity (..)
    , HashTagEntity (..)
    , UserMention (..)
    , URLEntity (..)
    , MediaEntity (..)
    , MediaSize (..)
    )  where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Control.Applicative

type DateString = String
type UserId = Integer
type Friends = [UserId]
type URIString = ByteString
type UserName = Text
type ScreenName = Text
type StatusId = Integer
type LanguageCode = String

data StreamingAPI
    = SStatus Status
    | SRetweetedStatus RetweetedStatus
    | SEvent Event
    | SDelete StatusDeletion
      -- | SScrubGeo ScrubGeo
    | SFriends Friends
    | SUnknown Value
    deriving (Show, Eq)

instance FromJSON StreamingAPI where
    parseJSON v@(Object o) =
        SRetweetedStatus <$> js <|>
        SStatus <$> js <|>
        SEvent <$> js <|>
        SDelete <$> js <|>
        SFriends <$> (o .: "friends") <|>
        return (SUnknown v)
      where
        js :: FromJSON a => Parser a
        js = parseJSON v
    parseJSON v = fail $ show v

data UserStream
    = UserStreamFriends [UserId]
    | UserStreamStatus Status
    | UserStreamEvent Event
    | UserStreamStatusDeletion StatusDeletion
    | UserStreamDirectMessage DirectMessage
    deriving (Show, Eq)

instance FromJSON UserStream where
    parseJSON v@(Object o) =
        UserStreamFriends <$> (o .: "friends") <|>
        UserStreamStatus <$> a <|>
        UserStreamEvent <$> a <|>
        UserStreamStatusDeletion <$> a <|>
        UserStreamDirectMessage <$> (o .: "direct_message")
      where
        a :: FromJSON a => Parser a
        a = parseJSON v
    parseJSON v = fail $ show v

-- | <https://dev.twitter.com/docs/platform-objects/tweets> 2013-08-13 16:29
data Status = Status
    { statusCreatedAt :: DateString
    , statusId :: StatusId
    , statusText :: Text
    , statusSource :: Text
    , statusTruncated :: Bool
    , statusEntities :: Maybe Entities
    , statusInReplyTo :: Maybe StatusId
    , statusInReplyToUser :: Maybe UserId
    , statusFavorite :: Maybe Bool
    , statusRetweetCount :: Maybe Integer
    , statusRetweeted :: Bool
    , statusRetweetedStatus :: Maybe Status
    , statusUser :: User
    } deriving (Show, Eq)

instance FromJSON Status where
    parseJSON (Object o) = Status
         <$> o .: "created_at"
         <*> o .: "id"
         <*> o .: "text"
         <*> o .: "source"
         <*> o .: "truncated"
         <*> o .:? "entities"
         <*> o .:? "in_reply_to_status_id"
         <*> o .:? "in_reply_to_user_id"
         <*> o .:? "favorited"
         <*> o .:? "retweet_count"
         <*> o .: "retweeted"
         <*> o .:? "retweeted_status"
         <*> o .: "user"
    parseJSON v = fail $ show v

data Ids = Ids
    { idsIds :: [Integer]
    , idsNextCursor :: Int
    , idsNextCursorStr :: String
    , idsPreviousCursor :: Int
    , idsPreviousCursorStr :: String
    } deriving (Show, Eq)

instance FromJSON Ids where
    parseJSON (Object o) = Ids
        <$> o .: "ids"
        <*> o .: "next_cursor"
        <*> o .: "next_cursor_str"
        <*> o .: "previous_cursor"
        <*> o .: "previous_cursor_str"
    parseJSON v = fail $ show v

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
    , searchMetadataRefreshUrl :: URIString
    , searchMetadataNextResults :: Maybe URIString
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

data RetweetedStatus = RetweetedStatus
    { rsCreatedAt :: DateString
    , rsId :: StatusId
    , rsText :: Text
    , rsSource :: Text
    , rsTruncated :: Bool
    , rsEntities :: Maybe Entities
    , rsUser :: User
    , rsRetweetedStatus :: Status
    } deriving (Show, Eq)

instance FromJSON RetweetedStatus where
    parseJSON (Object o) = RetweetedStatus
        <$> o .: "created_at"
        <*> o .: "id"
        <*> o .: "text"
        <*> o .: "source"
        <*> o .: "truncated"
        <*> o .:? "entities"
        <*> o .: "user"
        <*> o .: "retweeted_status"
    parseJSON v = fail $ show v

data DirectMessage = DirectMessage
    { directMessageId :: StatusId
    , directMessageIdStr :: String
    , directMessageText :: Text
    , directMessageSender :: User
    , directMessageSenderId :: UserId
    , directMessageSenderIdStr :: String
    , directMessageSenderScreenName :: ScreenName
    , directMessageRecipient :: User
    , directMessageRecipientId :: UserId
    , directMessageRecipientIdStr :: String
    , directMessageRecipientScreeName :: ScreenName
    , directMessageCreatedAt :: DateString
    , directMessageEntities :: Entities
    } deriving (Show, Eq)

instance FromJSON DirectMessage where
    parseJSON (Object o) = DirectMessage
        <$> o .: "id"
        <*> o .: "id_str"
        <*> o .: "text"
        <*> o .: "sender"
        <*> o .: "sender_id"
        <*> o .: "sender_id_str"
        <*> o .: "sender_screen_name"
        <*> o .: "recipient"
        <*> o .: "recipient_id"
        <*> o .: "recipient_id_str"
        <*> o .: "recipient_screen_name"
        <*> o .: "created_at"
        <*> o .: "entities"
    parseJSON v = fail $ show v

-- | <https://dev.twitter.com/docs/streaming-apis/messages#Events_event>
--   done.
data EventType
    = AccessRevokedEvent
    | BlockEvent
    | UnblockEvent
    | FavoriteEvent
    | UnfavoriteEvent
    | FollowEvent
    | UnfollowEvent
    | ListCreatedEvent
    | ListDestroyedEvent
    | ListUpdatedEvent
    | ListMemberAddedEvent
    | ListMemberRemovedEvent
    | ListUserSubscribedEvent
    | ListUserUnsubscribedEvent
    | UserUpdateEvent
    deriving (Show, Eq)

eventType :: Text -> EventType
eventType "access_revoked" = AccessRevokedEvent
eventType "block" = BlockEvent
eventType "unblock" = UnblockEvent
eventType "favorite" = FavoriteEvent
eventType "unfavorite" = UnfavoriteEvent
eventType "follow" = FollowEvent
eventType "unfollow" = UnfollowEvent
eventType "list_created" = ListCreatedEvent
eventType "list_destroy" = ListDestroyedEvent
eventType "list_updated" = ListUpdatedEvent
eventType "list_member_added" = ListMemberAddedEvent
eventType "list_member_removed" = ListMemberRemovedEvent
eventType "list_user_subscribed" = ListUserSubscribedEvent
eventType "list_user_unsubscribed" = ListUserUnsubscribedEvent
eventType "user_update" = UserUpdateEvent

-- | done.
data EventTarget
    = EventTargetStatus Status
    | EventTargetList List
    deriving (Show, Eq)

instance FromJSON EventTarget where
    parseJSON v =
        EventTargetStatus <$> parseJSON v <|>
        EventTargetList <$> parseJSON v

-- | done.
data Event = Event
    { eventEvent :: EventType
    , eventSource :: User
    , eventTarget :: User
    , eventTargetObject :: Maybe EventTarget
    , eventCreatedAt :: DateString
    } deriving (Show, Eq)

instance FromJSON Event where
    parseJSON (Object o) = Event
        <$> (eventType <$> o .: "event")
        <*> o .: "source"
        <*> o .: "target"
        <*> o .:? "target_object"
        <*> o .: "created_at"
    parseJSON v = fail $ show v

data StatusDeletion = StatusDeletion
    { statusDeletionId  :: StatusId
    , statusDeletionIdStr :: String
    , statusDeletionUserId :: UserId
    , statusDeletionUserIdStr :: String
    } deriving (Show, Eq)

instance FromJSON StatusDeletion where
  parseJSON (Object o) = do
    s <- o .: "delete" >>= (.: "status")
    StatusDeletion
        <$> s .: "id"
        <*> s .: "id_str"
        <*> s .: "user_id"
        <*> s .: "user_id_str"
  parseJSON v = fail $ show v

data User = User
    { userId :: UserId
    , userName :: UserName
    , userScreenName :: ScreenName
    , userDescription :: Maybe Text
    , userLocation :: Maybe Text
    , userProfileImageURL :: Maybe URIString
    , userURL :: Maybe URIString
    , userProtected :: Maybe Bool
    , userFollowers :: Maybe Int
    , userFriends :: Maybe Int
    , userTweets :: Maybe Int
    , userLangCode :: Maybe LanguageCode
    , userCreatedAt :: Maybe DateString
    } deriving (Show, Eq)

instance FromJSON User where
    parseJSON (Object o) = User
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "screen_name"
        <*> o .:? "description"
        <*> o .:? "location"
        <*> o .:? "profile_image_url"
        <*> o .:? "url"
        <*> o .:? "protected"
        <*> o .:? "followers_count"
        <*> o .:? "friends_count"
        <*> o .:? "statuses_count"
        <*> o .:? "lang"
        <*> o .:? "created_at"
    parseJSON v = fail $ show v

data List = List
    { listId :: Int
    , listName :: Text
    , listFullName :: Text
    , listMemberCount :: Int
    , listSubscriberCount :: Int
    , listMode :: Text
    , listUser :: User
    } deriving (Show, Eq)

instance FromJSON List where
    parseJSON (Object o) = List
        <$> o .:  "id"
        <*> o .:  "name"
        <*> o .:  "full_name"
        <*> o .:  "member_count"
        <*> o .:  "subscriber_count"
        <*> o .:  "mode"
        <*> o .:  "user"
    parseJSON v = fail $ show v
