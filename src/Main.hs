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

updater :: IOData () -> IO ()
updater ioData = getCustom ioData >>= go
    where
        go bearerToken = do
            runRedditAnon $ do
                posts <- subredditPosts $ R "AskReddit"
                liftIO . print . title $ head posts
            return ()

frontend :: IOData () -> IO ()
frontend ioData = do
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
