module Bread.Core.Main ( main ) where

import Bread.Core.EditorM
import Bread.Core.Editor
import Bread.Core.Utility.Log as BL
import Control.Monad.Trans

main :: IO ()
main = do
  withEditor defaultEditor $ do
    BL.log "Hello"
    BL.log "World"
  return ()
