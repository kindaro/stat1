module Main where

import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad         (ap)
import           Control.Monad.Loops
import qualified Data.ByteString.Lazy  as B
import           Data.Either.Combinators
import           Data.List             (foldl')
import qualified Data.Text             as T
import qualified Data.Vector           as V
import           System.Environment

import Api
import Api.Vk

type Text = T.Text

    -- Yes, I did find a method to string together just the right input fitting and output fitting
    -- functions. But it's not ideal for the following reasons:

    -- A. `name` has to accept as argument an Args value with the correct method associated. I
    -- have the benefit of having it available inside the lambda, but is it /joli/ ?

    -- B. Everywhere in the output fitting function definitions, I have to drop the constructor
    -- explicitly.

main :: IO ()
main =
    -- undefined
    do
    args <- getArgs
    let target = args !! 0
    id <- runMethod
        (\x -> Args [ ("user_ids", x) ] :: Args UsersGet)
        parseId
        target
    print id
    friends <- getFriends id
    friendsNames <- getNames friends
    print friendsNames
    friendsSquared <- Prelude.sequence $ (fmap force . getFriends) <$> friends
    print $ friendsNames `zip` friendsSquared

    -- Compute mean.
    -- Compute deviation.
    -- Get walls.
    -- Find correlation between wall length and friends length.


-- * Future development.

-- ** Strategy for evaluation.

-- I need a function that does [uid] -> [uid, [uid] (friends)] conversion.
-- Then, I need another that does [uid, [uid] (friends)] -> [uid, [uid, [uid] (friends ^ 2) ] ].
-- Then, I need a third one that goes [uid, [uid, [uid]]] -> [uid, [uid]].
-- It would be a return, an fmap and a join.

-- nReturn :: [Int] -> (IO) [(Int, [Int])]
-- nReturn us = parseFriends 10 <$> api "friends.get"
-- nFmap :: [(Int, [Int])] -> [(Int, (IO) [(Int, [Int])])]

-- My system should deepen the view with every iteration.
-- Maybe should I start with a definition of "view."
-- 
--

-- ** Pool of workers.
--
-- Use QSem and Concurrently?

-- ** Memoizing.
--
-- Memoize api or runMethod?
--
