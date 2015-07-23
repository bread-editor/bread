{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | This module describes the way Bread interprets files. It also includes
-- any built-in functions related to file operations.
module Bread.Data.Buffer
       ( Buffer (..)
       , BufferMap
       , Bread.Data.Buffer.readFile
       , defaultBuffer
       , name
       , contents
       , path
       , cursorPos ) where

import Bread.Data.Bundle
import Data.Text as T
import System.IO
import Data.MessagePack
import Control.Lens
import Data.HashMap as HM

-- | The 'Buffer' type describes the data structure that
-- represents the contents of a file.
data Buffer = Buffer { _name :: !Text
                     , _contents :: !Text
                     , _path :: !(Maybe Text)
                     , _cursorPos :: !Int
                     } deriving (Show, Eq)
$(makeLenses ''Buffer)

instance Package Buffer where
  bundle buf = [ TextVal $ buf^.name, TextVal $ buf^.contents
               , TextVal $ fpath $ buf^.path, NumVal $ buf^.cursorPos]
    where fpath :: Maybe Text -> Text
          fpath (Just p) = p
          fpath _        = "" 
  unwrap (TextVal n:TextVal p:TextVal c:NumVal pos:_) = Buffer n c (Just p) pos
  unwrap _ = Buffer "*malformed buffer request*" "*malformed*" Nothing 0

instance MessagePack Buffer where
  toObject = toObject . bundle
  fromObject obj = let (Just x) = fromObject obj :: Maybe Bundle
                   in Just $ unwrap x

defaultBuffer :: Buffer
defaultBuffer = Buffer "*scratch*" "" Nothing 0

-- | 'readFile' reads a file to a new 'Buffer'. 
readFile :: FilePath -> IO Buffer
readFile p = do
  handle <- openFile p ReadMode
  c <- T.pack <$> hGetContents handle
  return $ Buffer ((Prelude.last . T.split (=='/') . T.pack) p) c (Just $ T.pack p) 0

-- | This type is used by Bread to keep a map of Buffers
type BufferMap = HM.Map Text Buffer
