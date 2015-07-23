module Bread.Core.Utility.Log ( Bread.Core.Utility.Log.log ) where

import Bread.Core.Editor
import Bread.Core.EditorM
import Bread.Config.Types
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Data.Text as T
import Data.Text.IO as T
import Data.HashMap as HM

log :: (MonadIO m) => Text -> EditorT m ()
log str = do
  editor <- get
  let optExists = do MapOpt debugOpts <- HM.lookup "debug" (editor^.options)
                     HM.lookup "logging" debugOpts
  case optExists of
    Just (BoolOpt True) -> liftIO $ T.putStrLn str
    Nothing             -> return ()
