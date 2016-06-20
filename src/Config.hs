{-# LANGUAGE OverloadedStrings, CPP #-}

module Config where

import Reddit
import Network.Wai.Handler.Warp                 (Port)
import Data.Text                                (Text)


-- base directory of cone for reddit
baseDir :: FilePath

-- cone server port
srvPort :: Port

-- how often is new data is crawled on reddit (in minutes)
updateInterval :: Int


#ifndef RELEASE

-- DEVELOP config
baseDir         = "/Users/work/code/cc-reddit/"
srvPort         = 8080
updateInterval  = 15

#else

-- LIVE config
baseDir         = "/var/www/"
srvPort         = 80
updateInterval  = 15

#endif



-- location at which serialised data is stored
dataFileName :: FilePath
dataFileName = "cone.model"


-- show this in upper left hand corner of conecanvas
domainLabel :: Text
domainLabel = "Reddit"

-- Reddit API requires a custo user agent string, which is why these options
-- need to be specified instead of using the defaults.
redditOptions :: RedditOptions
redditOptions = RedditOptions
    { rateLimitingEnabled   = True
    , connectionManager     = Nothing
    , loginMethod           = Anonymous,
    , customUserAgent       = Just "symbolian.cone:v.1.0.0 (by /u/RelevantBits)"
    }

-- subreddits to be considered
subredditNames :: [SubredditName]
subredditNames = map R
    [ "AskReddit"
    , "gifs"
    , "AskScience"
    , "worldnews"
    , "todayilearned"
    , "AdviceAnimals"
    , "technology"
    , "woahdude"
    , "IamA"
    , "InterestingAsFuck"
    ]
