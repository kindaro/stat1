module Api where

import           Control.Arrow
import           Control.Lens
import           Network.URL
import           Network.Wreq
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Maybe
import           System.FilePath.Posix

class Method method where
    name :: Args method -> String

    runMethod :: (a -> Args method) -> (Val method -> b) -> a -> IO b
    runMethod inputFitting outputFitting =
        inputFitting >>> (\ m @ (Args x) -> api (name m) x) >>> fmap (Val >>> outputFitting)

newtype Args method = Args [ (String, String) ]
newtype Val method = Val Value

dropVal (Val x) = x

select :: (Value -> Maybe a) -> String -> Val method -> a
select selector errorMessage = dropVal >>> selector >>> fromMaybe (error errorMessage)

api :: String -> [ (String, String) ] -> IO Value
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
