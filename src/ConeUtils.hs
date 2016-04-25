{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module ConeUtils where

import Data.Text                                (append)
import ConeServer.ConeTypes
import ConeServer.Types                         (RoseTree(..), enumerateTree)

import Config


-- Utilities for building ConeTrees
node :: [ConeTree] -> ConeEntry -> ConeTree
node [] e = RoseLeaf e {ceIsLeaf = ceIsLeaf e && True, ceTextId = "tId_" `append` ceLabel e} (-1) []
node cs e = RoseLeaf e {ceIsLeaf = False, ceTextId = "tId_" `append` ceLabel e} (-1) cs

leafNode :: ConeEntry -> ConeTree
leafNode = node []

rootNode :: [ConeTree] -> ConeTree
rootNode = RoseLeaf
    emptyLeaf {ceIsLeaf = False, ceTextId = "tId_root", ceLabel = domainLabel}
    (-1)

prepTree :: ConeTree -> ConeTree
prepTree c = enumerateTree coneEntrySetId 1 c
