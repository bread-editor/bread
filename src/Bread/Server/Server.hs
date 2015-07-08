module Bread.Server.Server ( runServer ) where

import Data.Conduit.Network as CN
import Data.Conduit as C
import Data.MessagePack as MP
import Data.Text as T
import Data.ByteString.Lazy as BSL
import Data.ByteString as BS
import Data.HashMap as HM
import System.IO as IO
import Control.Monad.Trans
import Bread.Server.API
import Bread.Server.Calls
import Bread.Data.Bundle

runServer :: Int -> IO ()
runServer port = runTCPServer (serverSettings port "*") serv

serv :: AppData -> IO ()
serv apd = do
  let src  = appSource apd
      sink = appSink apd :: Sink BS.ByteString IO ()
    in src $$ process =$ sink

process :: Conduit BS.ByteString IO BS.ByteString
process = do
  mreq <- await
  case mreq of
    Just req -> do
      liftIO $ BS.putStrLn req
      let (Just reqB) = (MP.unpack . fromStrict) req
          res = doRequest reqB _calls_
      liftIO $ (IO.putStrLn . show) res
      yield $ (toStrict . MP.pack) res
      process
    _ -> return ()

doRequest :: APIRequest -> CallMap -> APIResult
doRequest req calls = let (reqName, reqArgs) = (name req, args req)
                      in  case HM.lookup reqName calls of
                      Just call -> (method call) 1 reqArgs
                      _         -> Fail 1 $ "Can't find call " `T.append` reqName

-- servLoop :: Socket -> IO ()
-- servLoop sock = do
--   connection <- accept sock
--   onConnect connection
--   servLoop sock

-- onConnect :: (Socket, SockAddr) -> IO ()
-- onConnect (client, client_addr) = do
--   IO.putStrLn "Got a connection"
--   h <- socketToHandle client ReadWriteMode
--   hSetBuffering h NoBuffering
--   apiHandle h

-- -- | The main function that handles connections and api calls. This function
-- -- will take a handle given to it by the `onConnect` function, and then reads
-- -- and parses each method call sent by a client. It will call the appropriate
-- -- method, and then return the data.
-- apiHandle :: Handle -> IO ()
-- apiHandle h = do
--   req <- BSL.hGetContents h
--   IO.putStrLn "Got some contents: "
--   IO.putStrLn $ show req
--   let call = parseRequest req
--   return ()

-- -- | This function will parse a MessagePack'd request from a client
-- parseRequest :: BSL.ByteString -> CallMap -> Maybe APICall
-- parseRequest buf calls = let Just (method:_) = MP.unpack buf
--                          in  HM.lookup method calls
