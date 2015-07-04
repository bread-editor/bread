-- | This module describes the way Bread interprets files. It also includes any built-in functions related to file operations.
module Bread.Data.Files
       ( Buffer
       , contents
       , name
       , BufferList
       , Bread.Data.Files.readFile ) where

import Data.Text as T
import System.IO
import Control.Monad

-- | The 'Buffer' type describes the a 'Vector' of 'Char's that
-- represent the contents of a file.
data Buffer = Buffer { contents :: Text
                     , name :: Text
                     }

-- | 'readFile' reads a file to the form of 'FileContents'. 
readFile :: FilePath -> IO Buffer
readFile path = do
  handle <- openFile path ReadMode
  contents <- T.pack <$> hGetContents handle
  return $ Buffer contents $ (Prelude.last . (T.split (=='/')). T.pack) path
  
type BufferList = [Buffer]




