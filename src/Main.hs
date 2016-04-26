{-# LANGUAGE OverloadedStrings #-}

import ConeServer.RunServer
import ConeServer.ConeTypes
import ConeServer.Types                         (RoseTree(..), enumerateTree)
import ConeServer.Utils

import Reddit
import Reddit.Types.Post
import Reddit.Types.Listing
import Reddit.Types.Subreddit                       (SubredditName(..))

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Debug.Trace

import Network.OAuth.OAuth2.Internal            (AccessToken(..))

import Control.Concurrent                       (threadDelay, forkIO)
-- import Control.DeepSeq

import Config
import ConeUtils

subredditPosts :: Monad m => SubredditName -> RedditT m [Post]
subredditPosts sr = do
    listing <- getPosts' (Options Nothing Nothing) Hot (Just sr)
    return $ contents listing


main = do
    -- let myTree = buildTree posts
    -- myTree <- subRedditTree posts

    ioData <- initServer srvPort baseDir False ()

    forkIO $ updater ioData
    forkIO $ frontend ioData

    forever . threadDelay $ 60 * 1000 * 1000

entryFromText :: Text.Text -> ConeEntry
entryFromText s = ConeEntry {
    ceEntryId       = 0,
    ceLabel         = s,
    ceTargetUri     = Nothing,
    ceComment       = Nothing,
    ceIconName      = Nothing,
    ceStlName       = Nothing,
    ceColor         = Nothing,
    ceIsLeaf        = True,
    ceTextId        = s
}

data SrData = SrData {
    srName      :: SubredditName,
    srPosts     :: [Post]
}

srData arg = SrData {
    srName      = fst arg,
    srPosts     = snd arg
}

-- Make a ConeTree from a list of Subreddit names
srTree :: [SrData] -> ConeTree
srTree [] = rootNode []
srTree sds = rootNode $ fmap (\sd -> postTree (srPosts sd) (srEntry sd)) sds
    where srEntry sd = entryFromText . Text.pack . show . srName $ sd

-- Make a ConeTree from a list of posts in a Subreddit and the Subreddit's entry
postTree :: [Post] -> ConeEntry -> ConeTree
postTree [] e = leafNode e
postTree ps e = node
    (fmap (commentTree . entryFromText . title) ps)
    e

commentTree :: ConeEntry -> ConeTree
commentTree e = leafNode e

updater :: IOData () -> IO ()
updater ioData = go
    where
        go = do
            ~(Right sds) <- runRedditAnon $ do
                -- posts <- subredditPosts $ R "AskReddit"
                let sns = map R ["AskReddit", "Berlin", "de"]
                tps <- mapM subredditPosts sns
                return . map srData $ zip sns tps

            let newModel = prepTree . srTree $ sds

            applyIOSetter ioData newModel setTestModel
            print "Updated cone model"

frontend :: IOData () -> IO ()
frontend ioData =
    runServer ioData Nothing Nothing $ emptyTree


-- posts <- getPosts from subreddits
--     getPosts action für frontpage
--     getPosts' :: Monad m => Options PostID -> ListingType -> Maybe SubredditName -> RedditT m PostListing
--
-- getposts from subreddits = for all subreddits get posts with comments
--
-- get posts with comments = for all posts get comments
--
--
-- makeConeTree = §
