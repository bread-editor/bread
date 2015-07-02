-- | This module describes the way Bread interprets files. It also includes any built-in functions related to file operations.
module Bread.Data.Files
       ( Buffer
       , Bread.Data.Files.readFile ) where

import qualified Data.Sequence as S
import System.IO

-- | The 'Buffer' type describes the a 'Sequence' of 'Char's that
-- represent the contents of a file.
type Buffer = S.Seq Char

-- | 'readFile' reads a file to the form of 'FileContents'. 
readFile :: FilePath -> IO Buffer
readFile path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  return $ S.fromList contents
  
type BufferList = [Buffer]




