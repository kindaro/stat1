module Main where

import Control.Arrow
import Control.Lens
import Control.Monad.Loops
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.URL
import Network.Wreq
import System.Environment
import System.FilePath.Posix



main :: IO ()
main = do
    args <- getArgs
    let target = args !! 0
    let limit = read (args !! 1)
    -- Discover ID.
    id <- parseId <$> api "users.get" [ ("user_ids", target) ]
    -- Fetch friends.
    friends <- parseFriends limit <$> api "friends.get" [ ("user_id", id) ]
    print friends
    friendsNames <- (fmap parseName . api "users.get") `forkMapM` ( idToApiArg <$> friends )
    print friendsNames
    -- Fetch their friends.
    friendsSquared <- Prelude.sequence $ fmap (parseFriends limit) . api "friends.get" <$> ( idToApiArg <$> friends )
    print $ friendsNames `zip` friendsSquared
    -- Compute mean.
    -- Compute deviation.
    
    where

    parseId = show . fromMaybe (error "No parse for ID!") . (^? nth 0 . key "uid" . _Integral )
    parseName = T.unpack . fromMaybe (error "No parse for name!") . (^? nth 0 . key "first_name" . _String )
    parseFriends limit = take limit . ( ^.. values . _Integral )
    idToApiArg = pure . ("user_id", ) . show


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
