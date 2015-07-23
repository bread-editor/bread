{-# LANGUAGE TemplateHaskell #-}
-- | This module is used to describe the "state" of the editor and some other things
-- that I haven't written yet
module Bread.Core.Editor ( Editor (..)
                         , defaultEditor
                         , buffers
                         , currentBuffer
                         , server
                         , options ) where

import Bread.Data.Buffer
import Bread.Config.Types
import Bread.Config.Default
import Bread.Server
import Control.Lens
import Data.HashMap as HM

-- | The data structure that represents the Editor in Bread
data Editor = Editor { _buffers :: BufferMap
                     , _currentBuffer :: Buffer
                     , _server :: Int -> IO ()
                     , _options :: OptionMap
                     } 
$(makeLenses ''Editor)

-- | A "Default" Editor state
defaultEditor :: Editor
defaultEditor = Editor { _buffers = HM.singleton "*scratch*" defaultBuffer
                       , _currentBuffer = defaultBuffer
                       , _server = runServer
                       , _options = defaultOptions
                       }
