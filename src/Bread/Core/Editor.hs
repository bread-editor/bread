-- | This module is used to describe the "state" of the editor and some other things
-- that I haven't written yet
module Bread.Core.Editor ( Editor (..) ) where

import Bread.Data.Buffer
import Bread.Config.Types
import Bread.Config.Default
import Bread.Server
import Data.HashMap as HM

data Editor = Editor { buffers :: BufferMap
                     , activeBuffer :: Buffer
                     , server :: Int -> IO ()
                     , options :: OptionMap
                     }

initEditor :: Editor
initEditor = Editor { buffers = HM.singleton "*scratch*" buf
                    , activeBuffer = buf
                    , server = runServer
                    , options = defaultOptions
                    }
  where buf = Buffer "" "*scratch*" Nothing

data EditorM a = EditorM Editor a
