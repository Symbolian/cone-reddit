{-# LANGUAGE OverloadedStrings #-}

module Config where


import Network.Wai.Handler.Warp                 (Port)
import Data.Text                                (Text)


-- base directory for cone server
baseDir :: FilePath
baseDir = "/Users/work/code/ConeServer"

-- base directory for TweetCone
tcBaseDir :: FilePath
tcBaseDir = "/Users/work/code/cc-reddit/"

-- cone server port
srvPort :: Port
srvPort = 8080

-- show this in upper left hand corner of conecanvas
domainLabel :: Text
domainLabel = "Trending search queries"
