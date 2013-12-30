module Web.Twitter.Types.DirectMessage
    ( DirectMessage (..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON (..), Value (..), (.:))
import Data.Text (Text)

import Web.Twitter.Types.Common
import Web.Twitter.Types.Entities
import Web.Twitter.Types.User

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
