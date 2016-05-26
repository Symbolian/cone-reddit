{-# LANGUAGE OverloadedStrings #-}

module Config where


import Network.Wai.Handler.Warp                 (Port)
import Data.Text                                (Text)


-- base directory of cone for reddit
baseDir :: FilePath
baseDir = "/Users/work/code/cc-reddit/"

-- cone server port
srvPort :: Port
srvPort = 8080

-- show this in upper left hand corner of conecanvas
domainLabel :: Text
domainLabel = "Reddit"

-- How often is new data added (in minutes)
updateInterval :: Int
updateInterval = 15
