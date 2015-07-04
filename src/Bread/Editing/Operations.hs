-- | This module is for all of the built-in editing operations
-- one can perform on a file.
module Bread.Editing.Operations
       ( insert
       , insertString
       , delete
       , deleteRange
       , Bread.Editing.Operations.copy
       , copyRange
       , cut
       , cutRange) where

import Bread.Data.Files as B
import Bread.Editing.TextObjects as O
import Bread.Data.Boards
import Data.Text as T

-- | 'insert' inserts a 'O.Char' at 'O.Pos' in a 'Text'
-- and returns a 'Text'
insert :: O.Char -> O.Pos -> Text -> Text
insert char pos buf = let (before, after) = T.splitAt pos buf
                           in  before `append` (singleton char `append` after)

-- | 'insertString' inserts a 'Text' at 'O.Pos' in a
-- 'Text' and returns a 'Text'
insertString :: Text -> O.Pos -> Text -> Text
insertString string pos buf = let (before, after) = T.splitAt pos buf
                                   in  before `append` string `append` after

-- | 'delete' deletes a 'O.Char' at 'O.Pos' in a 'Text'
-- and returns a 'Text'
delete :: O.Pos -> Text -> Text
delete pos buf = let (before, after) = T.splitAt pos buf
                      in  before `append` (T.drop 1 after)

-- | 'deleteRange' deletes a 'O.Range' in a 'Text' and
-- returns a 'Text'
deleteRange :: O.Range -> Text -> Text
deleteRange (begin, end) buf = let (before, after) = T.splitAt begin buf
                                    in  before `append` (T.drop (end - begin + 1) after)

-- | 'copy' copies the 'O.Char' at 'O.Pos' in the 'Text'
-- to the 'Board' that it returns
copy :: O.Pos -> Text -> Board
copy pos buf = T.take 1 $ T.drop pos buf

-- | 'copyRange' copies a range of 'O.Char's from the
-- 'Text' to the 'Board' that it returns
copyRange :: O.Range -> Text -> Board
copyRange (begin, end) buf = T.take (end - begin + 1) $ T.drop begin buf

-- | 'cut' copies a 'O.Char' at 'O.Pos' from the 'Text'
-- to a 'Board' and returns a 'Tuple' of ('Text', 'Board')
-- containing the modified 'Text' and the 'Board'
cut :: O.Pos -> Text -> (Text, Board)
cut pos buf = (delete pos buf, Bread.Editing.Operations.copy pos buf)

-- | 'cutRange' copies a range of 'O.Char' from the
-- 'Text' to a 'Board' and returns a 'Tuple' of ('Text',
-- 'Board') containing the modified 'Text' and the 'Board'
cutRange :: O.Range -> Text -> (Text, Board)
cutRange range buf = (deleteRange range buf, copyRange range buf)
