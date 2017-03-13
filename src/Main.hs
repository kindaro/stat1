module Main where

import Control.Arrow
import Control.Lens
import System.FilePath.Posix
import qualified Data.ByteString.Lazy as B
import Data.Aeson.Lens
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Text as T
import Network.URL
import Network.Wreq


target = "ignat_insarov"

main :: IO ()
main = do
    -- Discover ID.
    id <- parseId <$> api "users.get" [ ("user_ids", target) ]
    -- Fetch friends.
    friends <- parseFriends <$> api "friends.get" [ ("user_id", id) ]
    print friends
    friendsNames <- sequence $ (fmap parseName . api "users.get") <$> ( (:[]) . ("user_id",) . show <$> friends )
    -- Fetch their friends.
    friendsSquared <- Prelude.sequence $ fmap parseFriends . api "friends.get" <$> ( (:[]) . ("user_id", ) . show <$> friends)
    print $ friendsNames `zip` friendsSquared
    -- Compute mean.
    -- Compute deviation.
    
    where

    parseId = show . fromMaybe (error "No parse for ID!") . (^? nth 0 . key "uid" . _Integral )
    parseName = T.unpack . fromMaybe (error "No parse for name!") . (^? nth 0 . key "first_name" . _String )
    parseFriends = take 10 . ( ^.. values . _Integral )


api method args = parse <$> get url

    where

    url = exportURL URL
        { url_type = Absolute
            ( Host
                { protocol = HTTP True
                , host = "api.vk.com"
                , port = Nothing
                }
            )
        , url_path = "method" </> method
        , url_params = args
        }

    parse r = fromMaybe
        ( error . show $ r ^. responseBody . key "error" . key "error_msg" . _String )
        ( r ^? responseBody . key "response" )
