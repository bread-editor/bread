-- | This module describes the basic "text objects" used in Bread.
-- A text object can be thought of as a collection of characters. For example,
-- "sandwich" is a text object of type 'Word'.
-- 
module Bread.Editing.TextObjects
       ( Bread.Editing.TextObjects.Char
       , Pos
       , Range
       , Line
       , Paragraph
       , Bread.Editing.TextObjects.Word
       , Sentence
       , getChars
       , getCharSeq
       ) where

import Data.Text as T
import Data.Foldable (toList)
import Bread.Data.Files as B

-- | Just a 'Prelude.Char'
type Char      = Prelude.Char
-- | 'Pos' is a position referring to the position of a character
-- in a 'Buffer' sequence.
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

-- | 'getChars' takes a 'Range' and a 'Text' and returns the characters in
-- the range (inclusive) as a 'String'
getChars :: Range -> Text -> String
getChars (start, end) string = T.unpack $ T.take end $ T.drop start string

-- | 'getCharSeq' takes a 'Range' and a 'Text' and returns the characters in
-- the range (inclusive) as a 'Text'
getCharSeq :: Range -> Text -> Text
getCharSeq (start, end) string = T.take end $ T.drop start string
