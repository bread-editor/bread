import Network.Socket
import Network.Socket.ByteString.Lazy as NSBL
import Network.BSD
import Control.Monad
import System.IO as IO
import Data.Text as T
import Data.ByteString.Lazy as BSL
import Data.MessagePack as MP
import Bread.Data.Bundle
import Bread.Server.API

main = withSocketsDo $ do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  addr <- liftM hostAddresses $ getHostByName "localhost"
  connect sock $ SockAddrInet (fromInteger 8585) (Prelude.head addr)
  let testReq = Request "insert" [TextVal "a", NumVal 8]
  NSBL.sendAll sock $ MP.pack testReq
  res <- NSBL.getContents sock
  close sock
