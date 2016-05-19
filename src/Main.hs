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

import Data.Prizm.Types
import Data.Prizm.Color
import Data.Prizm.Color.CIE.LCH

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
    ioData <- initServer srvPort baseDir False ()

    forkIO $ updater ioData
    forkIO $ frontend ioData

    forever . threadDelay $ 60 * 1000 * 1000

entryFromText :: Text.Text -> ConeEntry
entryFromText t = ConeEntry {
    ceEntryId       = 0,
    ceLabel         = t,
    ceTargetUri     = Nothing,
    ceComment       = Nothing,
    ceIconName      = Nothing,
    ceStlName       = Nothing,
    ceColor         = Nothing,
    ceIsLeaf        = False,
    ceTextId        = t
}

entryFromPost :: Post -> Integer -> ConeEntry
entryFromPost p norm = ConeEntry {
    ceEntryId       = 0,
    ceLabel         = title p,
    ceTargetUri     = Nothing,
    ceComment       = Nothing,
    ceIconName      = Nothing,
    ceStlName       = Nothing,
    ceColor         = Just . (colorFromScore norm) . score $ p,
    ceIsLeaf        = True,
    ceTextId        = Text.pack . show . postID $ p
}

entryFromComment :: C.Comment -> Integer -> ConeEntry
entryFromComment c norm = ConeEntry {
    ceEntryId       = 0,
    ceLabel         = label,
    ceTargetUri     = Nothing,
    ceComment       = Nothing,
    ceIconName      = Nothing,
    ceStlName       = Nothing,
    ceColor         = (colorFromScore norm) <$> C.score c,
    ceIsLeaf        = True,
    ceTextId        = Text.pack . show . C.commentID $ c
} where
    Username label = C.author $ c

-- Create a cone color by interpolating between two color values depending
-- on the score of the comment/post
colorFromScore :: Integer -> Integer -> ConeColor
colorFromScore norm i = ConeColor (
        ColAsFloat,
        cFloats . toRGB $ interpolate ipval (CIELCH 93 25 120, CIELCH 46 75 21)
    )
    where
        n = (fromIntegral norm) / 100
        ipval = floor $ log(((fromIntegral i) + n) / n) * 50
        -- Convert components of RGB values used by Prizm color mixer to floats
        cFloats (RGB r g b) = map ((* (1/255)) . fromIntegral) [r, g, b]

data SrData = SrData {
    srName      :: SubredditName,
    srPosts     :: [C.PostComments]
}

-- Make a ConeTree from a list of Subreddit names
srTree :: [SrData] -> ConeTree
srTree [] = rootNode []
srTree sds = rootNode $ fmap (\sd -> postTree (srPosts sd) (srEntry sd)) sds
    where srEntry sd = entryFromText . Text.pack . show . srName $ sd

-- Make a ConeTree from a list of posts in a Subreddit and the Subreddit's entry
postTree :: [C.PostComments] -> ConeEntry -> ConeTree
postTree [] e = leafNode e
postTree ps e =
    let norm = maximum $ map (\(C.PostComments p _) -> score p) ps
    in  node
        (fmap (\(C.PostComments p crs) -> commentTree crs (entryFromPost p norm)) ps)
        e

commentTree :: [C.CommentReference] -> ConeEntry -> ConeTree
commentTree crs e = node (commentTreeBuilder (reverse . take 15 $ crs) [] 1000) e

commentTreeBuilder :: [C.CommentReference] -> [ConeTree] -> Integer -> [ConeTree]
commentTreeBuilder [] ts _ = ts
commentTreeBuilder ((C.Actual c):crs) ts norm =
    commentTreeBuilder crs ((appendCom c):ts) norm
    where appendCom c = node
            (commentTreeBuilder (contents . C.replies $ c) [] norm)
            (entryFromComment c norm)
commentTreeBuilder ((C.Reference cr _):crs) ts norm =
    commentTreeBuilder crs ts norm
    -- commentTreeBuilder crs ((appendRef cr):ts)
    -- where appendRef cr = leafNode . entryFromText . Text.pack . show $ cr



updater :: IOData () -> IO ()
updater ioData = go
    where
        go = do
            sds <- runRedditAnon $ do
                liftIO $ putStrLn "Starting update"
                -- Collect subreddits to be included
                let names = map R ["AskReddit", "AskHistorians", "AskScience", "DataIsBeautiful", "LifeProTips", "TrueReddit", "FoodForThought", "IamA", "InterestingAsFuck"]

                -- Retrieve post listing from each of the subreddits
                ps <- mapM subredditPosts names
                liftIO $ putStrLn "Loaded post listings"

                -- Retrieve comments for each listing
                ps' <- mapM (mapM (getPostComments . postID)) ps
                liftIO $ putStrLn "Loaded comments"

                return . map (uncurry SrData) $ zip names ps'

            case sds of
                Left msg -> do
                    putStrLn "Error loading data"
                    putStrLn . show $ msg
                Right sds -> do
                    -- Construct ConeTree from collected data
                    let newModel = prepTree . srTree $ sds

                    -- Update model with new ConeTree
                    applyIOSetter ioData newModel setTestModel
                    putStrLn "Updated cone model"

frontend :: IOData () -> IO ()
frontend ioData =
    runServer ioData Nothing Nothing $ emptyTree
