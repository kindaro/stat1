module Main where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Loops
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy  as B
import           Data.Either.Combinators
import           Data.List
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Vector           as V
import           Network.URL
import           Network.Wreq
import           System.Environment
import           System.FilePath.Posix



main :: IO ()
main = do
    args <- getArgs
    let target = args !! 0
    let limit = read (args !! 1)
    id <- parseId <$> api "users.get" [ ("user_ids", target) ]
    friends <- parseFriends limit <$> api "friends.get" [ ("user_id", T.unpack id) ]
    print friends
    friendsNames <- resolveToNames friends
    print friendsNames
    friendsSquared <- Prelude.sequence $ fmap (parseFriends limit) . api "friends.get" <$> ( idToApiArg <$> friends )
    print $ friendsNames `zip` friendsSquared

    -- Compute mean.
    -- Compute deviation.
    -- Get walls.
    -- Find correlation between wall length and friends length.

parseId :: Data.Aeson.Value -> T.Text
parseId = T.pack . show . fromMaybe (error "No parse for ID!") . (^? nth 0 . key "uid" . _Integral )

parseName :: Data.Aeson.Value -> T.Text
parseName = fromMaybe (error "No parse for name!") . (^? nth 0 . key "first_name" . _String )

parseFriends :: Int -> Data.Aeson.Value -> [Int]
parseFriends limit = take limit . ( ^.. values . _Integral )

idToApiArg :: Int -> [ (String, String) ]
idToApiArg = pure . ("user_id", ) . show

resolveToNames :: [Int] -> IO [ (T.Text, T.Text) ]
resolveToNames
     =   fmap show
     >>> intercalate ","
     >>> ("user_ids", )
     >>> pure
     >>> api "users.get"
     >>> fmap   (
                    (   (^.. values . key "first_name" . _String)
                    &&& (^.. values . key "last_name"  . _String)
                    )
                >>> uncurry zip
                )

api :: FilePath -> [ (String, String) ] -> IO Value
api method args = parse <$> get url

    where

    url = exportURL URL
        { url_type = Absolute Host
            { protocol = HTTP True
            , host = "api.vk.com"
            , port = Nothing
            }
        , url_path = "method" </> method
        , url_params = args
        }

    parse r = fromMaybe
        ( error . show $ r ^. responseBody . key "error" . key "error_msg" . _String )
        ( r ^? responseBody . key "response" )


