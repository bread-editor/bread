-- | This module describes the way Bread interprets files. It also includes
-- any built-in functions related to file operations.
module Bread.Data.Files
       ( Buffer (..)
       , BufferList
       , Bread.Data.Files.readFile ) where

import Bread.Data.Bundle
import Data.Text as T
import System.IO
import Control.Monad
import Data.MessagePack
import Data.HashMap as M
import Data.Maybe
import Data.Vector as V

-- | The 'Buffer' type describes the data structure that
-- represents the contents of a file.
data Buffer = Buffer { contents :: !Text
                     , name :: !Text
                     , path :: !Text
                     } deriving (Show, Eq)

instance Package Buffer where
  bundle buf = [TextVal $ contents buf, TextVal $ name buf, TextVal $ path buf]
  unwrap ((TextVal c):(TextVal n):(TextVal p):_) = Buffer c n p

instance MessagePack Buffer where
  toObject = (toObject . bundle)
  fromObject obj = let (Just x) = fromObject obj :: Maybe Bundle
                   in Just $ unwrap x

-- | 'readFile' reads a file to a new 'Buffer'. 
readFile :: FilePath -> IO Buffer
readFile path = do
  handle <- openFile path ReadMode
  contents <- T.pack <$> hGetContents handle
  return $ Buffer contents
    ((Prelude.last . (T.split (=='/')). T.pack) path) $ T.pack path


-- | This type is used by Bread to keep a list of Buffers  
type BufferList = [Buffer]
