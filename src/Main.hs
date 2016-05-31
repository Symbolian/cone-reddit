{-# LANGUAGE OverloadedStrings, PatternGuards, ScopedTypeVariables #-}

import ConeServer.RunServer
import ConeServer.ConeTypes
import ConeServer.Types                         (RoseTree(..), enumerateTree)
import ConeServer.Utils
import ConeServer.RestAPI

import Reddit
import Reddit.Types.Post
import Reddit.Types.Listing
import Reddit.Types.Subreddit                   (SubredditName(..))
import qualified Reddit.Types.Comment           as C

import Data.Prizm.Types
import Data.Prizm.Color
import Data.Prizm.Color.CIE.LCH
import Data.Char
import Data.Maybe
import Unsafe.Coerce

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class

import Data.Monoid
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.List as List
import Debug.Trace
import System.Directory                         (doesDirectoryExist, getCurrentDirectory)
import System.Environment                       (getArgs)
import System.IO                                (hSetBuffering, stdout, BufferMode(..))
import Text.Read                                (readMaybe)

import Network.OAuth.OAuth2.Internal            (AccessToken(..))

import Control.Concurrent                       (threadDelay, forkIO)
-- import Control.DeepSeq

import Config
import ConeUtils

entryFromText :: Text.Text -> ConeEntry
entryFromText t = ConeEntry {
    ceEntryId       = 0,
    ceLabel         = t,
    ceTargetUri     = Nothing,
    ceComment       = Just "nothing",
    ceIconName      = Nothing,
    ceStlName       = Nothing,
    ceColor         = Just $ colorFromScore 10000 scoreFromText,
    ceIsLeaf        = False,
    ceTextId        = t
} where
    scoreFromText = toInteger . ord . Text.head $ t

entryFromPost :: Post -> Integer -> ConeEntry
entryFromPost p norm = ConeEntry {
    ceEntryId       = 0,
    ceLabel         = title p,
    ceTargetUri     = Just . Text.pack . show . score $ p,
    ceComment       = Just $ postContent,
    ceIconName      = Nothing,
    ceStlName       = Nothing,
    ceColor         = Just . (colorFromScore norm) . score $ p,
    ceIsLeaf        = True,
    ceTextId        = Text.pack . show . postID $ p
} where
    postContent = case content p of
        SelfPost _ html -> buildField ["p", (textUsr $ author p), (rLink p), "",  html]
        Link uri        -> buildField ["p", (textUsr $ author p), (rLink p), uri, ""]
        TitleOnly       -> buildField ["p", (textUsr $ author p), (rLink p), "",  ""]
    buildField l = foldl1 Text.append $ List.intersperse "@@" l
    textUsr (Username t) = t
    rLink p = Text.append "http://reddit.com" $ permalink p

entryFromComment :: C.Comment -> Integer -> ConeEntry
entryFromComment c norm = ConeEntry {
    ceEntryId       = 0,
    ceLabel         = label,
    ceTargetUri     = Nothing,
    ceComment       = Just commentContent,
    ceIconName      = Nothing,
    ceStlName       = Nothing,
    ceColor         = (colorFromScore norm) <$> C.score c,
    ceIsLeaf        = True,
    ceTextId        = Text.pack . show . C.commentID $ c
} where
    Username label = C.author $ c
    commentContent = buildField [
        "c",
        (cID . C.commentID $ c),
        (C.bodyHTML c),
        (cScore . C.score $ c)]
    buildField l = foldl1 Text.append $ List.intersperse "@@" l
    cID (C.CommentID commentID) = commentID
    cScore :: Maybe Integer -> Text.Text
    cScore (Just score) = Text.pack . show $ score
    cScore Nothing = ""

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

-- Retrieve post listings from reddit
subredditPosts :: Monad m => SubredditName -> RedditT m [Post]
subredditPosts sr = do
    listing <- getPosts' (Options Nothing Nothing) Hot (Just sr)
    return $ contents listing

-- For collecting subreddit name and corresponding post listing
data SrData = SrData {
    srName      :: SubredditName,
    srPosts     :: [C.PostComments]
}

-- Make a ConeTree from a list of Subreddit names
srTree :: [SrData] -> ConeTree
srTree [] = rootNode []
srTree sds = rootNode $ fmap (\sd -> postTree (srPosts sd) (srEntry sd)) sds
    where
        srTitle (R t) = t
        srEntry sd = entryFromText . srTitle . srName $ sd

-- Make a ConeTree from a list of posts in a Subreddit and the Subreddit's entry
postTree :: [C.PostComments] -> ConeEntry -> ConeTree
postTree [] e = leafNode e
postTree ps e =
    let norm = maximum $ map (\(C.PostComments p _) -> score p) ps
    in  node
        (fmap (\(C.PostComments p crs) -> commentTree crs (entryFromPost p norm)) ps)
        e

-- Base of comment tree
commentTree :: [C.CommentReference] -> ConeEntry -> ConeTree
commentTree crs e = node (commentTreeBuilder (reverse . take 15 $ crs) [] 1000) e

-- Recursively descend into comments
commentTreeBuilder :: [C.CommentReference] -> [ConeTree] -> Integer -> [ConeTree]
commentTreeBuilder [] ts _ = ts
commentTreeBuilder ((C.Actual c):crs) ts norm =
    commentTreeBuilder crs ((appendCom c):ts) norm
    where appendCom c = node
            (commentTreeBuilder (reverse . contents . C.replies $ c) [] norm)
            (entryFromComment c norm)
commentTreeBuilder ((C.Reference cr _):crs) ts norm =
    commentTreeBuilder crs ts norm
    -- commentTreeBuilder crs ((appendRef cr):ts)
    -- where appendRef cr = leafNode . entryFromText . Text.pack . show $ cr

-- Main loop starts two threads for updater and user-facing part
main = do
    baseExists <- doesDirectoryExist baseDir
    baseDir' <- if baseExists
        then return baseDir
        else do
            cwd <- getCurrentDirectory
            putStrLn $ baseDir ++ " (hardcoded server root) not found; assuming " ++ cwd
            return cwd

    args <- getArgs
    let
        srvPort' = case args of
            "-p":p:_
                | Just port <- readMaybe p  -> port
            _                               -> srvPort

    token <- initServer srvPort' baseDir' False
    putStrLn $ "Starting web server on localhost:" ++ show srvPort'

    mvUpd  <- newMVar ()
    mvData <- newMVar (emptyContent, emptyTree)
    forkIO $ updater mvUpd mvData token
    forkIO $ frontend mvData token

    let
        waitUpdate = do
            threadDelay $ 1 * 1000 * 1000
            updRunning <- isEmptyMVar mvUpd
            if updRunning then waitUpdate else takeMVar mvUpd

        mainLoop "go" = do
            putMVar mvUpd ()
            waitUpdate
            mainLoop ""
        mainLoop "quit" =
            return ()
        mainLoop _ = do
            putStr "'go' for update; 'quit' to quit >> "
            getLine >>= mainLoop

    hSetBuffering stdout NoBuffering
    waitUpdate >> mainLoop ""


updater :: MVar () -> MVar SessionData -> ServerToken ContentStore -> IO ()
updater mvUpd mvData token@(sessGlobal, _) = forever $ do
    takeMVar mvUpd
    go
    putMVar mvUpd ()
    threadDelay $ 1 * 1000 * 1000
    where
        go = do
            sds <- runRedditWith redditOptions $ do
                liftIO $ putStrLn "Starting update"
                -- Collect subreddits to be included
                let names = map R ["AskReddit", "AskHistorians", "AskScience",
                                "DataIsBeautiful", "LifeProTips", "TrueReddit",
                                "FoodForThought", "IamA", "InterestingAsFuck"]
                -- let names = map R ["AskReddit"]

                -- Retrieve post listing from each of the subreddits
                ps <- mapM subredditPosts names
                liftIO $ putStrLn "Loaded post listings..."

                -- Retrieve comments for each post in each subreddit listing
                ps' <- mapM (mapM (getPostComments . postID)) ps
                liftIO $ putStrLn "Loaded comments..."

                return . map (uncurry SrData) $ zip names ps'

            case sds of
                Left msg -> do
                    putStrLn "Error loading data"
                    print msg
                    putStrLn "Retrying..."
                    go

                Right sds -> do
                    -- Construct ConeTree from collected data
                    let
                        new@(newContent, newModel) = extractContent . prepTree . srTree $ sds
                    swapMVar mvData new
                    -- Update model with new ConeTree
                    gUpdateUserSessions sessGlobal
                        (\sess -> return $ setModel (sess {slData = newContent}) newModel)
                    putStrLn $ List.foldl1 (++)
                        ["Updated cone model. Next update in ",
                        (show updateInterval), " minutes."]
                    threadDelay $ updateInterval * 60 * 1000 * 1000
                    go

frontend :: MVar SessionData -> ServerToken ContentStore -> IO ()
frontend mvData token@(sessGlobal, _) =
    runServer token "REDDIT" (Just [additionalAPI]) Nothing initUserSession
    where
        initUserSession :: IO (SessionLocal ContentStore)
        initUserSession = do
            (content, tree) <- readMVar mvData
            return $ setModel (emptySessionLocal sessGlobal content) tree

additionalAPI :: RestAPI
additionalAPI = RestAPI "RedditDemo"
    [ RestText "content of some reddit post; specify entry via ?id=<number>"
        ["content"] handlerContent
    , RestRedirect "start enjoying the reddit demo"
        [] (RestHandler $ \_ _ -> return $ Just "/html/index.html")
    ]

handlerContent :: RestHandlerText
handlerContent = RestHandler $ \(_, params, _) ioData -> do
    contentStore <- getCustom ioData
    return $ fromMaybe "" $ do
        entryId <- getParamInt "id" params
        lookupContent entryId (unsafeCoerce contentStore)
