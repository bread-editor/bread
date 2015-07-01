import System.IO
import Network

main = withSocketsDo $ do
  socket <- listenOn $ PortNumber 9090
  (handle, hostname, port) <- accept socket
  hPutStr handle msg
  hFlush  handle
  hClose  handle

msg = "HTTP/1.1 200 OK\nServer: Bread/0.0.1.0\nContent-Type: text/plain\n\nHello\n"


