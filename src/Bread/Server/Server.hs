module Bread.Server.Server ( runServer ) where

import Network.Socket
import Network.Socket.ByteString as NSB
import Data.MessagePack as MP
import Data.Text as T
import Data.ByteString.Lazy as BSL
import Data.ByteString as BS

runServer :: Integer -> IO ()
runServer port = withSocketsDo $ do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet (fromInteger port) iNADDR_ANY)
  listen sock 100
  servLoop sock

servLoop :: Socket -> IO ()
servLoop sock = do
  connection <- accept sock
  onConnect connection
  servLoop sock

onConnect :: (Socket, SockAddr) -> IO ()
onConnect (client, client_addr) = do
  Prelude.putStrLn "Got a client"
  bytes <- NSB.send client $ BS.concat $ BSL.toChunks $ MP.pack $ T.pack "Hello"
  Prelude.putStrLn "Sent client a message"
  close client
