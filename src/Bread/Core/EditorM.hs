module Bread.Core.EditorM ( EditorT
                          , EditorIO
                          , withEditor) where

import Control.Monad.State
import Bread.Core.Editor

type EditorT m a = StateT Editor m a
type EditorIO a = EditorT IO a

withEditor :: Editor -> EditorIO a -> IO Editor
withEditor e f = execStateT f e 
