module Web.Twitter.Types.Common
    ( UserId
    , UserName
    , ScreenName
    , UrlString
    , StatusId
    , LanguageCode
    , IntStrs (..)
    , Ids (..)
    , UTCTime (..)
    ) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Aeson (FromJSON (..), Value (..), (.:))
import Data.Int (Int64)
import Data.Text (Text)

import Web.Twitter.Types.Internal

type UserId = Int64
type UserName = Text
type ScreenName = Text
type UrlString = Text -- TODO
type StatusId = Int64
type LanguageCode = Text -- TODO

data IntStrs = Ints [Int64] | Strs [String] deriving (Show, Eq)

data Ids = Ids
    { idsIds :: IntStrs
    , idsNextCursor :: Int
    , idsNextCursorStr :: String
    , idsPreviousCursor :: Int
    , idsPreviousCursorStr :: String
    } deriving (Show, Eq)

instance FromJSON Ids where
    parseJSON (Object o) = Ids
        <$> (Ints <$> o .: "ids" <|>
             Strs <$> o .: "ids")
        <*> o .: "next_cursor"
        <*> o .: "next_cursor_str"
        <*> o .: "previous_cursor"
        <*> o .: "previous_cursor_str"
    parseJSON v = fail $ show v
