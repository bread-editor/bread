{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
-- | This module describes the Buffer Monad, which is used to interact with buffers
module Bread.Core.BufferM ( BufferM 
                          , test) where

import Bread.Data.Buffer
import Control.Monad.State
import Data.Text as T

type BufferM a = State Buffer a

withBuffer :: Buffer -> State Buffer a -> Buffer
withBuffer b f = execState f b

replaceAll :: Text -> State Buffer ()
replaceAll t = do
  buf <- get
  put $ buf { contents = t }

appendText :: Text -> State Buffer ()
appendText t = do
  buf <- get
  let con = contents buf
      res = con `T.append` t
  put $ buf { contents = res }

test :: Buffer
test = withBuffer defaultBuffer $ do
  replaceAll "Hello "
  appendText "World!"
  appendText " This is an example of how you can\nprogramatically add text to a buffer"
  
