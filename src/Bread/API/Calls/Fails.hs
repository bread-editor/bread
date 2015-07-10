-- | This module defines some more basic fail calls for ease of development
module Bread.API.Calls.Fails ( failNumArgs
                             , failTypeArgs ) where

import Bread.API
import Data.Text as T

-- | Fail because the number of arguments didn't match
failNumArgs meth exp act = Fail 2 $ "Wrong number of arguments to '"
                           `append` meth `append` "'. Expected " `append`
                           (T.pack . show $ exp) `append` ", received "
                           `append` (T.pack . show $ act)

-- | Fail because the type of arguments didn't match
failTypeArgs meth = Fail 3 $ "Wrong type of arguments sent to " `append` meth
