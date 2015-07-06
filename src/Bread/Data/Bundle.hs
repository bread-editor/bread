{-# LANGUAGE LambdaCase #-}
-- | A module that defines data `Bundle`s
module Bread.Data.Bundle ( Bundle
                         , Package (..)
                         , Val (..) ) where

import Data.MessagePack as MP
import Data.Text as T
import Data.Maybe
import Data.Vector as V

-- | Container type for creating MsgPack objects in a JSON-like way
data Val = NumVal  Int
         | TextVal Text
         | ListVal Bundle
         | NilVal deriving (Show, Ord, Eq)

-- | `Bundle` type
type Bundle = [Val]

instance MessagePack Val where
  toObject = \case
    NumVal  a  -> toObject a
    TextVal t  -> toObject t
    ListVal xs -> toObject xs
    NilVal     -> ObjectNil

  fromObject = \case
    ObjectInt   a  -> Just $ NumVal  a :: Maybe Val
    ObjectStr   t  -> Just $ TextVal t :: Maybe Val
    ObjectArray xs -> Just $ ListVal $ catMaybes $ fromObject <$> V.toList xs
    ObjectNil      -> Just NilVal
    _              -> Nothing

-- | Class used for data that can be bundled. Anything that uses this class should
-- satisfy the constraint that
-- > (unwrap . bundle) == id
--
-- Bundles are then sent via MessagePack to clients, so any data inside a bundle
-- has to be an instance of the `MessagePack` class
class Package a where
  bundle :: a -> Bundle
  unwrap :: Bundle -> a

