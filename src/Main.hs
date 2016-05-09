{-# LANGUAGE OverloadedStrings #-}

import ConeServer.RunServer
import ConeServer.ConeTypes
import ConeServer.Types                         (RoseTree(..), enumerateTree)
import ConeServer.Utils

import Reddit
import Reddit.Types.Post
import Reddit.Types.Listing
import Reddit.Types.Subreddit                       (SubredditName(..))
import qualified Reddit.Types.Comment           as C

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
    srPosts     :: [C.PostComments]
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
postTree :: [C.PostComments] -> ConeEntry -> ConeTree
postTree [] e = leafNode e
postTree ps e = node
    (fmap (\(C.PostComments p crs) -> commentTree crs (entryFromText . title $ p)) ps)
    e

commentTree :: [C.CommentReference] -> ConeEntry -> ConeTree
commentTree crs e = node (commentTreeBuilder crs []) e

commentTreeBuilder :: [C.CommentReference] -> [ConeTree] -> [ConeTree]
commentTreeBuilder [] ts = ts
commentTreeBuilder ((C.Actual c):crs) ts =
    commentTreeBuilder crs ((appendCom c):ts)
    where appendCom c = leafNode . entryFromText . Text.pack . show . C.author $ c
commentTreeBuilder ((C.Reference cr _):crs) ts =
    commentTreeBuilder crs ((appendRef cr):ts)
    where appendRef cr = leafNode . entryFromText . Text.pack . show $ cr

updater :: IOData () -> IO ()
updater ioData = go
    where
        go = do
            ~(Right sds) <- runRedditAnon $ do
                -- Collect subreddits to be included
                let names = map R ["AskReddit", "Berlin", "de"]

                -- Retrieve post listing from each of the subreddits
                ps <- mapM subredditPosts names

                -- Retrieve comments for each listing
                ps' <- mapM
                    -- (\p -> (p, getComments p))
                    (mapM (getPostComments . postID))
                    ps

                return . map srData $ zip names ps'

            -- Construct ConeTree from collected data
            let newModel = prepTree . srTree $ sds

            -- Update model with new ConeTree
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
