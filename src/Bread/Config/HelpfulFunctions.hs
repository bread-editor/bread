-- | Convenience functions for use in configuration
module Bread.Config.HelpfulFunctions ( oMap ) where

import Data.HashMap as HM
import Data.Hashable

-- | Convenience function for making an options map
oMap :: (Ord k, Hashable k) => [(k, v)] -> HM.Map k v
oMap = HM.fromList
