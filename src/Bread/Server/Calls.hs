module Bread.Server.Calls ( CallMap
                          , _calls_ ) where

import Data.HashMap as HM
import Data.Text as T
import Bread.Server.API
import Bread.Data.Bundle
import Bread.Data.Files
import Bread.Editing.Operations as O
import Bread.Editing.TextObjects

type CallMap = HM.Map Text APICall

_calls_ = HM.fromList [
  ( "insert" :: Text, Call apiInsert 2 )
  ]

apiInert :: Int -> Bundle -> APIResult
apiInsert numArgs args = case Prelude.length args of
  numArgs -> case args of
    ((TextVal char):(NumVal pos):_) -> let x = Buffer "test" "ayy lmao" "butts"
                                           c = O.insert (T.head char) pos $ contents x
                                       in Result $ TextVal c : []
    _                               -> Fail 3 "Wrong arg types"
  _       -> Fail 2 $ "Wrong number of arguments to `insert`. Sent "
             `append` (T.pack . show $ Prelude.length args) `append` ", expected 2"

