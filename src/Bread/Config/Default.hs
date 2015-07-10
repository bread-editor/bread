-- | Default configuration for Bread. If you'd like to configure Bread, a good way is to
-- use defaultOptions and the update the map based on the options you'd like to configure
module Bread.Config.Default (  ) where

import Bread.Config.Types
import Bread.Config.HelpfulFunctions
import Data.Text as T
import Data.HashMap as HM

-- | Type that we use to construct the list of default options
type TOption = (Text, Option)

-- | The default set of options for Bread
defaultOptions :: OptionMap
defaultOptions = oMap [ defTabOptions
                      ]

-- | The default set of options with regards to using tabs or spaces
defTabOptions :: TOption
defTabOptions = ("tabs", MapOpt $ oMap [ ("use", BoolOpt True)
                                               , ("tab-width", NumOpt 2)
                                               , ("num-spaces", NumOpt 2)
                                               ])
