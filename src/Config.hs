{-# LANGUAGE OverloadedStrings #-}

module Config where

import Reddit
import Network.Wai.Handler.Warp                 (Port)
import Data.Text                                (Text)


-- base directory of cone for reddit
baseDir :: FilePath
baseDir = "/Users/work/code/cc-reddit/"

-- location at which serialised data is stored
dataFileName :: FilePath
dataFileName = "cone.model"

-- cone server port
srvPort :: Port
srvPort = 8080

-- show this in upper left hand corner of conecanvas
domainLabel :: Text
domainLabel = "Reddit"

-- How often is new data added (in minutes)
updateInterval :: Int
updateInterval = 15

-- Reddit API requires a custo user agent string, which is why these options
-- need to be specified instead of using the defaults.
redditOptions :: RedditOptions
redditOptions = RedditOptions {
    rateLimitingEnabled     = True,
    connectionManager       = Nothing,
    loginMethod             = Anonymous,
    customUserAgent         = Just "symbolian.cone:v.1.0.0 (by /u/RelevantBits)"
}
