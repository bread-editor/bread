-- | This module describes the Buffer Monad, which is used to interact with buffers
module Bread.Core.BufferM ( BufferT
                          , BufferIO
                          , withBuffer
                          , withCurrentBuffer ) where

import Bread.Data.Buffer
import Bread.Core.Editor
import Bread.Core.EditorM
import Control.Monad.State
import Control.Lens

type BufferT m a = StateT Buffer m a
type BufferIO a = BufferT IO a

withBuffer :: (Monad m) => Buffer -> BufferT m a -> m Buffer
withBuffer b f = execStateT f b

withCurrentBuffer :: (Monad m) => BufferT m a -> EditorT m Buffer
withCurrentBuffer f = do
  editor <- get 
  lift $ withBuffer (editor^.currentBuffer) f
