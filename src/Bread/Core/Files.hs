-- | This module describes the way Bread interprets files. It also includes any built-in functions related to file operations.
module Bread.Core.Files
       ( FileContents
       , Bread.Core.Files.readFile ) where

import qualified Data.Sequence as S
import System.IO

-- | The 'FileContents' type describes the contents of any text file.
-- It is a Sequence of Chars. This can be created using the 'readFile' function.
type FileContents = S.Seq Char

-- | 'readFile' reads a file to the form of 'FileContents'. 
readFile :: FilePath -> IO FileContents
readFile path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  return $ S.fromList contents
  





