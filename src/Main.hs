import System.IO
import Control.Concurrent
import Network

main = withSocketsDo $ do
  socket <- listenOn $ PortNumber 9090
  loop socket

loop socket = do
  (h, _, _) <- accept socket
  forkIO $ body h
  loop socket

body handle = do
  hSetBuffering handle LineBuffering
  putStrLn "Server: Got a connection!"
  putStrLn "Server: Getting contents from client..."
  loop
  where loop = do
          contents <- hGetLine handle
          if contents /= "stop"
            then do putStrLn $ "Server: Got something! Here's what we got: " ++ contents
                    putStrLn "Server: Sending information back..."
                    hPutStrLn handle $ reverse contents
                    loop
            else do hFlush handle
                    hClose handle
