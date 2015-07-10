{-# LANGUAGE TemplateHaskell #-}
-- | This module defines the types used in configuring Bread.
module Bread.Config.Types ( OptionMap
                          , Option (..) ) where

import Data.Text as T
import Data.HashMap as HM
import Control.Lens

-- | A `HM.Map` of option names to the associated value
type OptionMap = HM.Map Text Option

-- | Option data type
data Option  = TextOpt Text
             | NumOpt  Int
             | BoolOpt Bool 
             | NilOpt   -- ^ Used to manually set an option to nil
             | UnsetOpt -- ^ Used when option has not been set
             | ListOpt [Option]
             | MapOpt  OptionMap -- ^ Map of options, useful for complex options
                                 -- or "namespacing" options
