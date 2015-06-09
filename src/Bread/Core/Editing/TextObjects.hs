module Bread.Core.Editing.TextObjects
       ( Char
       , Pos
       , Range
       , Line
       , Paragraph
       , Word
       , Sentence) where

import Data.Sequence

type Char      = Char
type Pos       = Int
type Range     = (Pos, Pos)
type Line      = Range
type Paragraph = Range
type Word      = Range
type Sentence  = Range


