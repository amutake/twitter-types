module Web.Twitter.Types.StreamMessage
    ( UserStreamMessage (..)
    , Event (..)
    , EventType (..)
    , EventTarget (..)
    , StatusDeletion (..)
    , DirectMessage (..)
    ) where

import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Data.Aeson (FromJSON (..), Value (..), (.:), (.:?), withText)
import Data.Aeson.Types (Parser)
import Data.Text (Text, unpack)

import Web.Twitter.Types.Common
import Web.Twitter.Types.Entities
import Web.Twitter.Types.List
import Web.Twitter.Types.Status
import Web.Twitter.Types.User

data UserStreamMessage
    = UserStreamFriends [UserId]
    | UserStreamEvent Event
    | UserStreamStatusDeletion StatusDeletion
    | UserStreamDirectMessage DirectMessage
    | UserStreamStatus Status
    deriving (Show, Eq)

instance FromJSON UserStreamMessage where
    parseJSON v@(Object o) =
        UserStreamFriends <$> (o .: "friends") <|>
        UserStreamEvent <$> a <|>
        UserStreamStatusDeletion <$> (o .: "delete") <|>
        UserStreamDirectMessage <$> (o .: "direct_message") <|>
        UserStreamStatus <$> a
      where
        a :: FromJSON a => Parser a
        a = parseJSON v
    parseJSON v = fail $ show v

-- | done.
data Event = Event
    { eventEvent :: EventType
    , eventCreatedAt :: DateString
    , eventSource :: User
    , eventTarget :: User
    , eventTargetObject :: Maybe EventTarget
    } deriving (Show, Eq)

instance FromJSON Event where
    parseJSON (Object o) = Event
        <$> o .: "event"
        <*> o .: "created_at"
        <*> o .: "source"
        <*> o .: "target"
        <*> o .:? "target_object"
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

instance FromJSON EventType where
    parseJSON = withText "Text" f
      where
        f "access_revoked" = pure AccessRevokedEvent
        f "block" = pure BlockEvent
        f "unblock" = pure UnblockEvent
        f "favorite" = pure FavoriteEvent
        f "unfavorite" = pure UnfavoriteEvent
        f "follow" = pure FollowEvent
        f "unfollow" = pure UnfollowEvent
        f "list_created" = pure ListCreatedEvent
        f "list_destroy" = pure ListDestroyedEvent
        f "list_updated" = pure ListUpdatedEvent
        f "list_member_added" = pure ListMemberAddedEvent
        f "list_member_removed" = pure ListMemberRemovedEvent
        f "list_user_subscribed" = pure ListUserSubscribedEvent
        f "list_user_unsubscribed" = pure ListUserUnsubscribedEvent
        f "user_update" = pure UserUpdateEvent
        f t = fail $ "Unknown event type: " ++ unpack t

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
data StatusDeletion = StatusDeletion
    { statusDeletionId  :: StatusId
    , statusDeletionIdStr :: String
    , statusDeletionUserId :: UserId
    , statusDeletionUserIdStr :: String
    } deriving (Show, Eq)

instance FromJSON StatusDeletion where
  parseJSON (Object o) = do
    s <- o .: "status"
    StatusDeletion
        <$> s .: "id"
        <*> s .: "id_str"
        <*> s .: "user_id"
        <*> s .: "user_id_str"
  parseJSON v = fail $ show v

-- | done.
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
