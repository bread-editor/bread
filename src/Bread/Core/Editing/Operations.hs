-- | This module is for all of the built-in editing operations
-- one can perform on a file.
module Bread.Core.Editing.Operations
       ( insert
       , insertString
       , delete
       , deleteRange
       , copy
       , copyRange
       , cut
       , cutRange) where

import Bread.Core.Files as B
import Bread.Core.Editing.TextObjects as O
import Bread.Core.Boards
import Data.Sequence as S

-- | 'insert' inserts a 'O.Char' at 'O.Pos' in a 'B.Buffer'
-- and returns the modified 'B.Buffer'
insert :: O.Char -> O.Pos -> B.Buffer -> B.Buffer
insert char pos buf = let (before, after) = S.splitAt pos buf
                           in  before >< (char <| after)

-- | 'insertString' inserts a 'String' at 'O.Pos' in a
-- 'B.Buffer' and returns the modified 'B.Buffer'
insertString :: String -> O.Pos -> B.Buffer -> B.Buffer
insertString string pos buf = let (before, after) = S.splitAt pos buf
                                   in  before >< (S.fromList string) >< after

-- | 'delete' deletes a 'O.Char' at 'O.Pos' in a 'B.Buffer'
-- and returns the modified 'B.Buffer'
delete :: O.Pos -> B.Buffer -> B.Buffer
delete pos buf = let (before, after) = S.splitAt pos buf
                      in  before >< (S.drop 1 after)

-- | 'deleteRange' deletes a 'O.Range' in a 'B.Buffer' and
-- returns the modified 'B.Buffer'
deleteRange :: O.Range -> B.Buffer -> B.Buffer
deleteRange (begin, end) buf = let (before, after) = S.splitAt begin buf
                                    in  before >< (S.drop (end - begin + 1) after)

-- | 'copy' copies the 'O.Char' at 'O.Pos' in the 'B.Buffer'
-- to the 'Board' that it returns
copy :: O.Pos -> B.Buffer -> Board
copy pos buf = S.take 1 $ S.drop pos buf

-- | 'copyRange' copies a range of 'O.Char's from the
-- 'B.Buffer' to the 'Board' that it returns
copyRange :: O.Range -> B.Buffer -> Board
copyRange (begin, end) buf = S.take (end - begin + 1) $ S.drop begin buf

-- | 'cut' copies a 'O.Char' at 'O.Pos' from the 'B.Buffer'
-- to a 'Board' and returns a 'Tuple' of ('Buffer', 'Board')
-- containing the modified 'Buffer' and the 'Board'
cut :: O.Pos -> B.Buffer -> (Buffer, Board)
cut pos buf = (delete pos buf, copy pos buf)

-- | 'cutRange' copies a range of 'O.Char' from the
-- 'B.Buffer' to a 'Board' and returns a 'Tuple' of ('Buffer',
-- 'Board') containing the modified 'Buffer' and the 'Board'
cutRange :: O.Range -> B.Buffer -> (Buffer, Board)
cutRange range buf = (deleteRange range buf, copyRange range buf)
