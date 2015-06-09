module Bread.Core.Editing.Operations
       ( insert
       , delete) where

import Bread.Core.Files as B
import Bread.Core.Editing.TextObjects as O
import Data.Sequence as S

insert :: O.Char -> O.Pos -> B.FileContents -> B.FileContents
insert char pos contents = let (before, after) = S.splitAt pos contents
                           in  before >< (char <| after)

delete :: O.Pos -> B.FileContents -> B.FileContents
delete pos contents = let (before, after) = S.splitAt pos contents
                      in  before >< (S.drop 1 after)
