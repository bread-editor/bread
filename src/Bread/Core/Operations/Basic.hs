-- | This module defines the most basic operations in Bread, which
-- are used to build many other operations
module Bread.Core.Operations.Basic ( getCursorPos ) where

import Bread.Core.BufferM
import Bread.Data.Buffer
import Control.Lens
import Control.Monad.State
import Data.Text as T

-- | Get current cursor position in buffer starting from 0
getCursorPos :: (Monad a) => BufferT a Int
getCursorPos = do
  buf <- get
  return (buf^.cursorPos)

-- | Get char at cursor position in buffer
getCurrentChar :: (Monad a) => BufferT a Char
getCurrentChar = do
  buf <- get
  currentPos <- getCursorPos
  return $ T.index (buf^.contents) currentPos
