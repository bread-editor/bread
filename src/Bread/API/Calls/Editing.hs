-- | API Calls relating to editing
module Bread.API.Calls.Editing ( apiInsert ) where

import Bread.Data.Bundle
import Bread.API
import Bread.API.Calls.Fails
import Data.Text as T

-- | Insert a character
apiInsert :: Int -> Bundle -> APIResult
apiInsert numArgs args =
  if (Prelude.length args) == numArgs
  then result args 
  else failNumArgs "insert" numArgs $ Prelude.length args
  where result ((TextVal t):(NumVal pos):_) = Result [TextVal "Inserted"]
        result _                            = failTypeArgs "insert"
