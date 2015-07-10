{-# LANGUAGE ExistentialQuantification #-}
-- | Module for interacting with Buffers as a Domain-Specific Language of sorts
module Bread.Core.BufferM ( ) where

import Bread.Data.Buffer

data BufferM a = Buffer a
