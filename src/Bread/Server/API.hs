module Bread.Server.API ( apiHandle ) where

import Bread.Data.Bundle
import Data.Text as T
import System.IO

-- | `APIResult` is used to handle the results from API calls to bread.
data APIResult = Fail Int Text
               | Result Bundle
                 deriving (Show, Eq, Ord)

instance Package APIResult where
  bundle (Result b) = NumVal 1 : b
  -- | ^ Sets the head of the result to 1, indicating success. Appends the result of the operation
  bundle (Fail a t) = [NumVal 0, NumVal a, TextVal t]
  -- | ^ Sets the head of the result to 0, indicating failure. Appends the fail code, and any text from the failure.
  unwrap (x:xs) = case x of NumVal 1 -> Result xs
                            NumVal 0 -> let ((NumVal c):(TextVal m):_) = xs
                                        in Fail c m

data APICall = Call { method :: Bundle -> APIResult
                    }

-- | The main function that handles connections and api calls. This function will take a handle given to it by the `onConnect` function, and then reads and parses each method call sent by a client. It will call the appropriate method, and then return the data.
apiHandle :: Handle -> IO ()
apiHandle h = do
  hPutStrLn h "Hello world!"
