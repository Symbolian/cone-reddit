{-# LANGUAGE OverloadedStrings #-}

module ConeUtils where

import Data.Text                                as T (Text, append, take, pack, length)
import Data.Text.Encoding                       (encodeUtf8)
import qualified Data.IntMap.Lazy               as M
import qualified Data.ByteString.Lazy.Char8     as BL
import ConeServer.ConeTypes
import ConeServer.Types                         (RoseTree(..), enumerateTree)

import Config


type ContentStore   = M.IntMap BL.ByteString

type SessionData    = (ContentStore, Text, ConeTree)

-- Utilities for building ConeTrees
node :: [ConeTree] -> ConeEntry -> ConeTree
node [] e = RoseLeaf e {ceIsLeaf = ceIsLeaf e && True, ceTextId = "tId_" `append` ceLabel e} (-1) []
node cs e = RoseLeaf e {ceIsLeaf = False, ceTextId = "tId_" `append` ceLabel e} (-1) cs

leafNode :: ConeEntry -> ConeTree
leafNode = node []

rootNode :: [ConeTree] -> ConeTree
rootNode = RoseLeaf
    emptyLeaf {
        ceIsLeaf    = False,
        ceTextId    = "tId_root",
        ceLabel     = domainLabel,
        ceColor     = Just $ ConeColor (ColAsFloat, [214/255, 19/255, 69/255])
    } (-1)

prepTree :: ConeTree -> ConeTree
prepTree c = enumerateTree coneEntrySetId 1 c

emptyContent :: ContentStore
emptyContent = M.empty

lookupContent :: Int -> ContentStore -> Maybe BL.ByteString
lookupContent = M.lookup

payloadSize :: ConeTree -> Int
payloadSize = sum . fmap commentSize
  where
    commentSize ConeEntry {ceComment = Just content} = T.length content
    commentSize _ = 0

extractContent :: ConeTree -> (ContentStore, ConeTree)
extractContent t =
    (foldl getContent M.empty t, truncateContent <$> t)
  where
    getContent m ConeEntry {ceComment = Just content, ceEntryId = eId} =
        M.insert eId (BL.fromStrict . encodeUtf8 $ content) m
    getContent m _ = m

    truncateContent e@ConeEntry {ceComment = Just content, ceEntryId = eId} =
        e {ceComment = Just $ (T.take 1 content) `append` T.pack (show eId)}
    truncateContent e = e
