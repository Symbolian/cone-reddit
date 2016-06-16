{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{- |
Module      : Favicon
Description : embeds binary data for a favicon directly into the executable
-}

module  Favicon (favicon)       where
import  Data.ByteString         as BS (ByteString)
import  Data.ByteString.Lazy    as BL (ByteString, fromStrict)
import  Data.FileEmbed          (embedFile)

favicon_ :: BS.ByteString
favicon_ = $(embedFile "assets/favicon.ico")

favicon :: (BS.ByteString, BL.ByteString)
favicon = ("image/vnd.microsoft.icon", fromStrict favicon_)
