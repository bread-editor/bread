{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | This module holds the structure and related functions for representing
-- the contents of a buffer in Bread. For right now, it basically splits files
-- by lines. However, this may change. Nevertheless, the Text representation
-- will always be available with `smush`.
module Bread.Data.Contents ( Contents(_lns)
                           , CPos (..)
                           , lns ) where

import qualified Data.Text as T
import Control.Lens

-- | The data structure for holding buffer contents
data Contents = Contents { _lns :: [T.Text] }
$(makeLenses ''Contents)

-- | A type for representing positions in Contents in (y, x) notation
newtype CPos = CPos (Int, Int)

-- | Smush `Contents` to `Text`
smush :: Contents -> T.Text
smush = T.concat . (^.lns)

-- | Get number of lines in a `Contents`
numLines :: Contents -> Int
numLines = Prelude.length . (^.lns)

-- | Get total length of `Contents`
length :: Contents -> Int
length c = foldr (\a acc -> acc + T.length a) 0 (c^.lns)

-- | Get line from a contents safely
line :: Int -> Contents -> Maybe T.Text
line i c = if i < numLines c then
             Just $ (c^.lns) !! i
           else Nothing

-- | Return character at position safely
index :: CPos -> Contents -> Maybe Char
index (CPos (y, x)) c = do
  ln <- line y c
  if x < T.length ln then
    return $ ln `T.index` x
  else Nothing
