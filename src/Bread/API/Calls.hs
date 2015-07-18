-- | This module defines all possible calls available for clients to use
module Bread.API.Calls ( CallMap
                       , _calls_ ) where

import qualified Data.HashMap as HM
import Data.Text as T
import Bread.API
import Bread.API.Calls.Editing

-- | Map of call names and `APICall`s 
type CallMap = HM.Map Text APICall

-- | The `Map` of all possible client calls
_calls_ :: CallMap
_calls_ = HM.fromList [
  insert
  ]

type CallSingleton = (Text, APICall)

insert :: CallSingleton
insert = ( "insert", Call apiInsert 2 )
