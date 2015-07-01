-- | This module defines a type 'Board' which is used to
-- represent clipboards in Bread
module Bread.Boards ( Board ) where

import Data.Sequence

-- | 'Board' is the clipboard type used in Bread
type Board = Seq Char
