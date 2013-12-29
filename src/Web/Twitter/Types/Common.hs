module Web.Twitter.Types.Common
    ( UserId
    , UserName
    , ScreenName
    ) where

import Data.Int (Int64)
import Data.Text (Text)

type UserId = Int64
type UserName = Text
type ScreenName = Text
