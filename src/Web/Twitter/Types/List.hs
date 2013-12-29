module Web.Twitter.Types.List where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON (..), Value (..), (.:))
import Data.Text (Text)

import Web.Twitter.Types.User

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
