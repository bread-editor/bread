{-# LANGUAGE OverloadedStrings #-}
-- | This module describes the way Bread interprets files. It also includes
-- any built-in functions related to file operations.
module Bread.Data.Buffer
       ( Buffer (..)
       , BufferMap
       , Bread.Data.Buffer.readFile
       , defaultBuffer ) where

import Bread.Data.Bundle
import Data.Text as T
import System.IO
import Data.MessagePack
import Data.HashMap as HM

-- | The 'Buffer' type describes the data structure that
-- represents the contents of a file.
data Buffer = Buffer { contents :: !Text
                     , name :: !Text
                     , path :: !(Maybe Text)
                     } deriving (Show, Eq)

instance Package Buffer where
  bundle buf = [TextVal $ contents buf, TextVal $ name buf, TextVal $ fpath $ path buf]
    where fpath :: Maybe Text -> Text
          fpath (Just p) = p
          fpath _        = "" 
  unwrap (TextVal c:TextVal n:TextVal p:_) = Buffer c n $ Just p
  unwrap _ = Buffer "*malformed buffer request*" "*malformed*" Nothing

instance MessagePack Buffer where
  toObject = toObject . bundle
  fromObject obj = let (Just x) = fromObject obj :: Maybe Bundle
                   in Just $ unwrap x

defaultBuffer :: Buffer
defaultBuffer = Buffer { contents = ""
                       , name = "*scratch*"
                       , path = Nothing }

-- | 'readFile' reads a file to a new 'Buffer'. 
readFile :: FilePath -> IO Buffer
readFile p = do
  handle <- openFile p ReadMode
  c <- T.pack <$> hGetContents handle
  return $ Buffer c
    ((Prelude.last . T.split (=='/') . T.pack) p) $ Just $ T.pack p

-- | This type is used by Bread to keep a map of Buffers
type BufferMap = HM.Map Text Buffer
