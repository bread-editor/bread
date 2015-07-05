-- | This module describes the way Bread interprets files. It also includes any built-in functions related to file operations.
module Bread.Data.Files
       ( Buffer
       , contents
       , name
       , path
       , BufferList
       , Bread.Data.Files.readFile ) where

import Data.Text as T
import System.IO
import Control.Monad
import Data.MessagePack
import Data.Map as M

-- | The 'Buffer' type describes the a 'Vector' of 'Char's that
-- represent the contents of a file.
data Buffer = Buffer { contents :: !Text
                     , name :: !Text
                     , path :: !Text
                     } deriving Show

instance MessagePack Buffer where
  toObject buf = toObject $ M.fromList [ (T.pack "contents", contents buf)
                                       , (T.pack "name", name buf)
                                       , (T.pack "path", path buf) ]
  fromObject obj = fromObject obj

-- | 'readFile' reads a file to a new 'Buffer'. 
readFile :: FilePath -> IO Buffer
readFile path = do
  handle <- openFile path ReadMode
  contents <- T.pack <$> hGetContents handle
  return $ Buffer contents
    ((Prelude.last . (T.split (=='/')). T.pack) path) $ T.pack path
  
type BufferList = [Buffer]




