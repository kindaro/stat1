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
    id <- parseId <$> api "users.get" [ ("user_ids", target) ]
    friends <- getFriends id
    friendsNames <- getNames friends
    friendsSquared <- Prelude.sequence $ fmap parseFriends . api "friends.get" <$> ( idToArg <$> friends )
    print $ friendsNames `zip` friendsSquared

    -- Compute mean.
    -- Compute deviation.
    -- Get walls.
    -- Find correlation between wall length and friends length.

-- ** Parser functions.
-- All of these have a general signature of form:
-- f :: Data.Aeson.Value -> a
-- -- or just for short:
-- f :: Value -> a

parseId :: Value -> Int -- ^ Operates on "users.get".
parseId = fromMaybe (error "No parse for ID!") . (^? nth 0 . key "uid" . _Integral )

parseName :: Value -> T.Text -- ^ Operates on "users.get".
parseName = fromMaybe (error "No parse for name!") . (^? nth 0 . key "first_name" . _String )

parseNames :: Value -> [ (T.Text, T.Text) ] -- ^ Operates on "users.get".
parseNames
        = ( (^.. values . key "first_name" . _String)
        &&& (^.. values . key "last_name"  . _String)
          ) >>> uncurry zip 

parseFriends :: Value -> [Int] -- ^ Operates on "friends.get".
parseFriends = ( ^.. values . _Integral )

-- ** Various pure helper functions used to construct arguments to api calls.

idToArg :: Int -> [ (String, String) ]
idToArg = pure . ("user_id", ) . show

idsToArg :: [Int] -> [ (String, String) ]
idsToArg xs = [ ("user_ids", intercalate "," . fmap show $ xs) ]

-- ** Getter functions.
-- All of these have a general signature of form:
-- f :: a -> IO b

getNames :: [Int] -> IO [ (T.Text, T.Text) ]
getNames =  idsToArg >>> api "users.get" >>> fmap parseNames

getFriends :: Int -> IO [Int]
getFriends u = parseFriends <$> api "friends.get" [ ("user_id", show u) ]

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

-- I need a function that does [uid] -> [uid, [uid] (friends)] conversion.
-- Then, I need another that does [uid, [uid] (friends)] -> [uid, [uid, [uid] (friends ^ 2) ] ].
-- Then, I need a third one that goes [uid, [uid, [uid]]] -> [uid, [uid]].
-- It would be a return, an fmap and a join.

-- nReturn :: [Int] -> (IO) [(Int, [Int])]
-- nReturn us = parseFriends 10 <$> api "friends.get"
-- nFmap :: [(Int, [Int])] -> [(Int, (IO) [(Int, [Int])])]
