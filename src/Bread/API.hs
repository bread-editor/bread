-- | This module is for handling API requests. All requests and results
-- are to be formatted in the following manner (as of Bread 0.0.1.0 and
-- subject to potential change)
--
-- Each request and result is a MessagePack'd array.
-- 
-- = Requests
-- Requests are formatted in the following manner:
-- 
-- @ [ String RequestName, [args] ] @
-- 
-- Args will vary based on the request, but they must be specified in a
-- list.
--
-- = Responses
-- Responses will be delivered in two different formats, one for failures,
-- and one for successes.
--
-- == Success
-- Success responses will be delivered in the following formats
-- 
-- @ [ 1, [ Effects ], [ Results ] ] @
-- 
-- Here, results will be different depending on the API call. They will be
-- able to be handled by MessagePack and should map to your chosen
-- programming language's types. Although results will be a "tuple",
-- MessagePack converts tuples to lists.
--
-- == Failure
-- Failure responses will be delivered in the following formats
-- 
-- @ [ 0, Int FailCode, String ErrorMessage ] @
--
-- This should be pretty much self-explanatory. If this is still confusing,
-- submit a bug report.
--
-- = Effect Types
-- - bMod: buffer modified
--
-- = Fail Codes
-- - 0: Unknown Error
-- - 1: Parse Error
-- - 2: Wrong Number of arguments
-- - 3: Wrong Type of arguments
-- - >100: Special

module Bread.API ( APIResult  (..)
                 , APICall    (..)
                 , APIRequest (..) ) where

import Bread.Data.Bundle
import Data.Text as T
import System.IO as IO
import Data.ByteString.Lazy as BSL
import Data.ByteString as BS
import Data.MessagePack as MP
import Data.HashMap as HM

-- | `APIResult` is used to handle the results from API calls to bread.
data APIResult = Fail Int Text
               | Result Bundle
                 deriving (Show, Eq, Ord)


instance Package APIResult where
  bundle (Result b) = NumVal 1 : ListVal b : []
  -- | ^ Sets the head of the result to 1, indicating success. Appends
  -- the result of the operation
  bundle (Fail a t) = [NumVal 0, NumVal a, TextVal t]
  -- | ^ Sets the head of the result to 0, indicating failure. Appends
  -- the fail code, and any text from the failure.
  unwrap (x:xs) = case x of NumVal 1 -> Result xs
                            NumVal 0 -> let ((NumVal c):(TextVal m):_) = xs
                                        in Fail c m

instance MessagePack APIResult where
  toObject = (toObject . bundle)
  fromObject obj = case (fromObject obj :: Maybe Bundle) of
    Just bundle -> Just $ unwrap bundle
    _           -> Nothing

-- | This structure is used for defining different APICalls, and the
-- methods (used internally only)
data APICall = Call { method  :: !(Int -> Bundle -> APIResult)
                    , numArgs :: !Int
                    }

-- | This structure defines the way APIRequests come in from clients
data APIRequest = Request { name :: !Text
                          , args :: !Bundle
                          } deriving Show

instance MessagePack APIRequest where
  fromObject obj = case (fromObject obj :: Maybe Bundle)
                   of Just req -> Just $ Request name args where
                        ((TextVal name):(ListVal args):_) = req
                      _        -> Nothing
  toObject req = toObject [TextVal $ name req, ListVal $ args req]

