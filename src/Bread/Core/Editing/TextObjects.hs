-- | This module describes the basic "text objects" used in Bread.
-- A text object can be thought of as a collection of characters. For example,
-- "sandwich" is a text object of type 'Word'.
-- 
module Bread.Core.Editing.TextObjects
       ( Bread.Core.Editing.TextObjects.Char
       , Pos
       , Range
       , Line
       , Paragraph
       , Bread.Core.Editing.TextObjects.Word
       , Sentence
       , getChars
       , getCharSeq
       ) where

import Data.Sequence as S
import Data.Foldable (toList)
import Bread.Core.Files as B

-- | Just a 'Prelude.Char'
type Char      = Prelude.Char
-- | 'Pos' is a position referring to the position of a character
-- in a 'FileContents' sequence.
type Pos       = Int
-- | 'Range' describes a range of characters between two 'Pos's
type Range     = (Pos, Pos)
-- | 'Line' is a line of characters. From the character right after "\\n" to the
-- next "\\n"
type Line      = Range
-- | 'Paragraph' is a 'Range' between a line that contains no characters to the next.
type Paragraph = Range
-- | 'Word' is a 'Range' of characters between whitespace
type Word      = Range
-- | 'Sentence' is a 'Range' of characters until the the regex "[\.\?\!] " is matched.
type Sentence  = Range

-- | 'getChars' takes a 'Range' and a 'FileContents' and returns the characters in
-- the range (inclusive) as a 'String'
getChars :: Range -> B.FileContents -> String
getChars (start, end) contents = toList $ S.take end $ S.drop start contents

-- | 'getCharSeq' takes a 'Range' and a 'FileContents' and returns the characters in
-- the range (inclusive) as a 'Seq Char'
getCharSeq :: Range -> B.FileContents -> Seq Bread.Core.Editing.TextObjects.Char
getCharSeq (start, end) contents = S.take end $ S.drop start contents
