module Api.Vk.UsersGet
    ( UsersGet
    , idsToArgs
    , parseId
    , parseFirstName
    , parseLastName
    , parseNames
    ) where

import           Control.Arrow
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.List
import qualified Data.Text             as T

import Api

type Text = T.Text


data UsersGet
instance Method UsersGet where name = const "users.get"


idsToArgs :: [Int] -> Args UsersGet
idsToArgs xs = Args [ ("user_ids", intercalate "," . fmap show $ xs) ]

parseId :: Val UsersGet -> Int
parseId = select (^? nth 0 . key "uid" . _Integral ) "No parse for ID!"

parseFirstName :: Val UsersGet -> Text
parseFirstName = select (^? nth 0 . key "first_name" . _String ) "No parse for first name!"

parseLastName :: Val UsersGet -> Text
parseLastName = select (^? nth 0 . key "first_name" . _String) "No parse for last name!"

parseNames :: Val UsersGet -> [ (Text, Text) ]
parseNames = select selector ""
    where selector =    (^.. values . key "first_name" . _String)
                    &&& (^.. values . key "last_name"  . _String)
                    >>> uncurry zip >>> Just

