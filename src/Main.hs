{-# LANGUAGE CPP, OverloadedStrings, PatternGuards #-}

import Config
import ConeUtils
import Favicon

import ConeServer.RunServer
import ConeServer.ConeTypes
import ConeServer.Utils
import ConeServer.RestAPI
import ConeServer.Caching

import Reddit
import Reddit.Types.Post
import Reddit.Types.Listing
import qualified Reddit.Types.Comment           as C

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as B
import qualified Data.ByteString.Char8          as BS (readFile)

import qualified Data.List                      as List
import Data.Maybe                               (fromMaybe)
import Data.Prizm.Types
import Data.Prizm.Color
import Data.Prizm.Color.CIE.LCH
import qualified Data.Text as Text
import Control.Exception

import Unsafe.Coerce                            (unsafeCoerce)

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class

import System.Directory                         (doesDirectoryExist, getCurrentDirectory, doesFileExist)
import System.Environment                       (getArgs)
import System.IO                                (hSetBuffering, stdout, BufferMode(..))
import Text.Read                                (readMaybe)

import  Data.Time.Clock.POSIX                   (getPOSIXTime)
import Control.Concurrent                       (threadDelay, forkIO, myThreadId)




entryFromSRData :: SrData -> ConeEntry
entryFromSRData sd = ConeEntry {
    ceEntryId       = 0,
    ceLabel         = subredditTitle,
    ceTargetUri     = Nothing,
    ceComment       = Just "nothing",
    ceIconName      = Nothing,
    ceStlName       = Nothing,
    ceColor         = Just $ ConeColor(ColAsFloat, [220/255, 187/255, 113/255]),
    ceIsLeaf        = False,
    ceTextId        = subredditTitle
} where
    subredditTitle = srTitle . srName $ sd
    srTitle (R t) = t

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
    buildField l = foldl1 Text.append $ List.intersperse ";-;" l
    textUsr (Username t) = t
    rLink = Text.append "http://reddit.com" . permalink

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
    buildField l = foldl1 Text.append $ List.intersperse ";-;" l
    cID (C.CommentID commentID) = commentID
    cScore = maybe "" (Text.pack . show)


-- Create a cone color by interpolating between two color values depending
-- on the score of the comment/post
colorFromScore :: Integer -> Integer -> ConeColor
colorFromScore norm i = ConeColor (
        ColAsFloat,
        cFloats . toRGB $ interpolate ipval (CIELCH 93 25 120, CIELCH 46 75 21)
    )
    where
        n = (fromIntegral norm) / 100 :: Double
        ipval = floor $ log(((fromIntegral i) + n) / n) * 50
        -- Convert components of RGB values used by Prizm color mixer to floats
        cFloats (RGB r g b) = map ((* (1/255)) . fromIntegral) [r, g, b]

-- Retrieve post listings from reddit
subredditPosts :: Monad m => SubredditName -> RedditT m [Post]
subredditPosts sr = do
    listing <- getPosts' (Reddit.Options Nothing Nothing) Hot (Just sr)
    return $ contents listing

-- For collecting subreddit name and corresponding post listing
data SrData = SrData {
    srName      :: SubredditName,
    srPosts     :: [C.PostComments]
}

-- Make a ConeTree from a list of Subreddit names
srTree :: [SrData] -> ConeTree
srTree [] = rootNode []
srTree sds = rootNode $ fmap (\sd -> postTree (srPosts sd) (entryFromSRData sd)) sds

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
    commentTreeBuilder crs (appendCom : ts) norm
    where appendCom = node
            (commentTreeBuilder (reverse . contents . C.replies $ c) [] norm)
            (entryFromComment c norm)
commentTreeBuilder ((C.Reference _ _):crs) ts norm =
    commentTreeBuilder crs ts norm
    -- commentTreeBuilder crs ((appendRef cr):ts)
    -- where appendRef cr = leafNode . entryFromText . Text.pack . show $ cr

toDisk :: ConeTree -> IO ()
toDisk =
    B.writeFile dataFileName . encode

fromDisk :: IO (Maybe ConeTree)
fromDisk = do
    putStrLn "Loading previous ConeTree"
    fileExists <- doesFileExist dataFileName
    unless fileExists $ writeFile dataFileName ""
    c <- BS.readFile dataFileName
    case eitherDecode $ B.fromStrict c of
        Left e -> do
            putStrLn e
            return Nothing
        Right decoded -> return $ Just decoded


data Updater = Running | Desired deriving Eq

-- Main loop starts two threads for updater and user-facing part
main :: IO ()
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
        static = "--static" `elem` args                                         -- no reddit updates; only use model previously dumped to disk
        config = defaultServerConfig {confPort = srvPort'}

    token <- initServer config baseDir' False
    putStrLn $ "Starting web server on localhost:" ++ show srvPort'

    mDiskTree <- fromDisk
    let
        diskTree                = prepTree $ fromMaybe emptyTree mDiskTree
        (iniContent, iniModel)  = extractContent diskTree

    mvUpd   <- newMVar Desired
    iniTag  <- getTimeStampTag
    mvTree  <- newMVar (iniContent, iniTag, iniModel)

    cache  <- initConeServerCache 64                                            -- 64MB max size

#ifdef RELEASE
    mapM_ (preloadStaticFile cache)
        [ "html/js/cone_reddit.mini.js"
        , "html/index.html"
        ]
#endif

    let
        cachePaths :: [(RestPath, CacheKeyModify)]
        cachePaths =
            [ (["iconlist"], CacheKeyNoModify)
            , (["model", "full"], CacheKeyTag)
            ]
    mapM_ (whitelistCache cache) cachePaths

    let token' = setRestCache cache token

    if static
        then putStrLn "Automatic updates from reddit are disabled"
        else void $ do
            forkIO $ updater mvUpd mvTree token'
            forkIO $ forever $ do
                threadDelay $ updateInterval * 60 * 1000 * 1000
                tryPutMVar mvUpd Desired

    mainThread <- myThreadId

    let
        console "go" = do
            tryPutMVar mvUpd Desired
            console ""
        console "quit" =
            throwTo mainThread ThreadKilled
        console _ = do
            putStr "'go' for update; 'quit' to quit >> "
            getLine >>= console

        autoShutdown minutes =
            threadDelay (minutes * 60 * 1000 * 1000) >> throwTo mainThread ThreadKilled

    -- this is if you need some kind of admin console
    --- hSetBuffering stdout NoBuffering >> forkIO (console "")

    -- this is only needed for profiling: there has to be a clean (i.e. not Ctrl-C)
    -- shutdown of the executable for GHC to evaluate runtime stats for profiling
    -- forkIO $ autoShutdown 3

    frontend mvTree token'



updater :: MVar Updater -> MVar SessionData -> ServerToken ContentStore -> IO ()
updater mvUpd mvTree token = forever $ do
    upd <- readMVar mvUpd
    when (upd == Desired) $ do
        swapMVar mvUpd Running
        handle (\(SomeException _ ) -> return ()) go
        void $ takeMVar mvUpd
    where
        go = do
            sds_ <- runRedditWith redditOptions $ do
                liftIO $ putStrLn "Starting update"

                -- Retrieve post listing from each of the subreddits
                liftIO $ putStrLn "Retrieving subreddit listings"
                ps <- mapM subredditPosts subredditNames

                -- Retrieve comments for each post in each subreddit listing
                ps' <- mapM
                    (\ a -> do
                        let R cur = subreddit . head $ a
                        liftIO . putStrLn $ ("Loading " ++ (Text.unpack cur) ++ " comment data")
                        mapM (getPostComments . postID) a)
                    ps

                return . map (uncurry SrData) $ zip subredditNames ps'

            case sds_ of
                Left msg -> do
                    putStrLn "Error loading data"
                    print msg
                    putStrLn "Retrying..."
                    go

                Right sds -> do
                    -- Construct ConeTree from collected data
                    let
                        newTree     = srTree sds
                        fullModel   = prepTree newTree
                        (content', model') = extractContent fullModel
                    putStrLn $ "Payload size full: " ++ show (payloadSize fullModel `div` 1000) ++ "K chars"
                    putStrLn $ "Payload size reduced: " ++ show (payloadSize model' `div` 1000) ++ "K chars"

                    toDisk newTree
                    newTag <- getTimeStampTag
                    swapMVar mvTree (content', newTag, model')

                    {-
                    -- currently, we do not update user's existing sessions with the new model
                    gUpdateUserSessions (getSessionGlobal token)
                        (\sess -> return $ setModel (sess {slData = newContent, slTag = const newTag}) newModel)
                    -}

                    putStrLn $ concat
                        [ "Updated cone model. Next update in "
                        , show updateInterval
                        , " minutes."
                        ]


frontend :: MVar SessionData -> ServerToken ContentStore -> IO ()
frontend mvData token =
    runServer token "REDDIT" (Just [additionalAPI]) Nothing initUserSession
  where
    initUserSession :: IO (SessionLocal ContentStore)
    initUserSession = do
      (contStore, tag, tree) <- readMVar mvData
      let newSession = (emptySessionLocal (getSessionGlobal token) contStore) {slTag = const tag}
      return $ setModel newSession tree


getTimeStampTag :: IO Text.Text
getTimeStampTag = Text.pack . show <$> getPOSIXTime

additionalAPI :: RestAPI
additionalAPI = RestAPI "RedditDemo"
    [ RestText "content of some reddit post; specify entry via ?id=<number>"
        ["content"] handlerContent
    , RestBinary "favicon handler"
        ["favicon.ico"] handlerFavicon
    , RestStatic "the base path for .html, .js and .css files"
        "html"
    , RestRedirect "start enjoying the reddit demo"
        [] (RestHandler $ \_ _ -> return $ Just "/html/index.html")
    ]

handlerContent :: RestHandlerText
handlerContent = RestHandler $ \(_, params, _) ioData -> do
    contentStore <- getCustom ioData
    return $ fromMaybe "" $ do
        entryId <- getParamInt "id" params
        lookupContent entryId (unsafeCoerce contentStore)

handlerFavicon :: RestHandlerBinary
handlerFavicon = RestHandler $ \_ _ -> return favicon
