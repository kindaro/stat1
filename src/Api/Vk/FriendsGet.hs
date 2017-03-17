module Api.Vk.FriendsGet
    ( FriendsGet
    , idToArgs
    , parseFriends
    ) where

import           Control.Arrow
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.List
import qualified Data.Text             as T

import Api

type Text = T.Text

data FriendsGet
instance Method FriendsGet where name = const "friends.get"


idToArgs :: Int -> Args FriendsGet
idToArgs = Args . pure . ("user_id", ) . show

parseFriends :: Val FriendsGet -> [Int]
parseFriends (Val x) = x ^.. values . _Integral

