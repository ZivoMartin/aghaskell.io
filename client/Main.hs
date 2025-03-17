{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import SDL.Primitive (circle)
import SDL

import Control.Concurrent.STM (atomically, newTVarIO, TVar, readTVarIO, modifyTVar')



import Messages (ClientMessage (..), ServerMessage (..), sendMessage, PlayerData, Ip, Position)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when)
import Network.Socket.ByteString (recvFrom)
import Data.Binary (encode, decodeOrFail)
import Data.ByteString ( toStrict, fromStrict )
import Const (servIp, local, mapWidth, mapHeight)
import Data.Word (Word8)
import Data.Bits (shiftR, (.&.))
import Network.Socket

data Memory = Memory {
  getPlayers :: [PlayerData],
  getMyIp :: Ip
  }


main :: IO ()
main = do
  initializeAll
  window <- createWindow "Aghaskell" (defaultWindow { windowInitialSize = V2 (fromIntegral mapWidth) (fromIntegral mapHeight) })
  renderer <- createRenderer window (-1) defaultRenderer
  memory <- newTVarIO (Memory [] ("", ""))

  _ <- forkIO $ udpServer memory
  appLoop memory renderer
  destroyWindow window
  memValue <- readTVarIO memory
  closeClient (getMyIp memValue)

mousePositionSender :: Ip -> IO ()
mousePositionSender ip = mousePosSpamer (0, 0)
  where
    mousePosSpamer :: Position  -> IO ()
    mousePosSpamer prevMousePos = do
      threadDelay 100000
      (P (V2 x y)) <- getAbsoluteMouseLocation  
      let newMousePos = (fromIntegral x, fromIntegral y)
      if prevMousePos /= newMousePos
        then do
          sendMessage servIp (toStrict (encode (NewMousePos ip newMousePos)))
          mousePosSpamer newMousePos
        else mousePosSpamer newMousePos
  

handleServerMessage :: TVar Memory -> ServerMessage -> IO ()
handleServerMessage memory (Refresh players) = do
  atomically $ modifyTVar' memory $ \(Memory _ ip) ->
    Memory players ip


getIp :: Socket -> IO (String, String)
getIp sock = do
    addr <- getSocketName sock
    case addr of
        SockAddrInet port _ -> do
          ip <- getOnlyIp
          return (if local then "127.0.0.1" else ip, show port)
        SockAddrInet6 port _ _ _ -> return ("[IPv6 unsupported]", show port)
        _ -> error "Unsupported socket address type"
  where
    getOnlyIp :: IO String
    getOnlyIp = do
       googleSock <- socket AF_INET Datagram defaultProtocol
       connect googleSock (SockAddrInet 53 (tupleToHostAddress (8,8,8,8)))
       SockAddrInet _ localHost <- getSocketName googleSock
       close googleSock
       return $ hostAddressToIP localHost
     where 
       hostAddressToIP :: HostAddress -> String
       hostAddressToIP addr =
         let b1 = fromIntegral ((addr `shiftR` 24) .&. 0xFF) :: Word8
             b2 = fromIntegral ((addr `shiftR` 16) .&. 0xFF) :: Word8
             b3 = fromIntegral ((addr `shiftR` 8) .&. 0xFF) :: Word8
             b4 = fromIntegral (addr .&. 0xFF) :: Word8
         in show b1 ++ "." ++ show b2 ++ "." ++ show b3 ++ "." ++ show b4

connectClient :: Ip -> IO ()
connectClient myIp = sendMessage servIp (toStrict (encode (Connect myIp)))

closeClient :: Ip -> IO ()
closeClient myIp = do
  sendMessage servIp (toStrict (encode (Disconnect myIp)))

udpServer :: TVar Memory -> IO ()
udpServer memory = withSocketsDo $ do
    addr <- resolve "0"
    sock <- openTheSocket addr
    myIp <- getIp sock
    _ <- forkIO $ mousePositionSender myIp
    putStrLn $ "ip: " ++ show myIp 
    atomically $ modifyTVar' memory $
      \ _ -> Memory [] myIp

    connectClient myIp
    putStrLn "Listening on client"
    _ <- forever $ do
      (msg, _) <- recvFrom sock 1024
      case decodeOrFail (fromStrict msg) of
        Left (_, _, err) -> putStrLn $ "Decoding error: " ++ err
        Right (_, _, serverMessage) -> handleServerMessage memory serverMessage
    close sock
    
  where
    resolve :: String -> IO AddrInfo
    resolve port = do
        let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Datagram }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
        return addr
    openTheSocket addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      bind sock (addrAddress addr)
      return sock



handleEvent :: Renderer -> Event -> IO Bool
handleEvent _ event = 
    case eventPayload event of
        QuitEvent -> return False
        _ -> return True

renderPlayer :: Renderer -> PlayerData -> IO ()
renderPlayer renderer ((x, y), _, size) = do
  _ <- fillRect renderer (Just $ Rectangle (P $ V2 (fromIntegral x) (fromIntegral y)) (V2 (fromIntegral size) (fromIntegral size)))
  putStr ""


appLoop :: TVar Memory -> Renderer -> IO ()
appLoop memory renderer = do
  
  rendererDrawColor renderer $= V4 0 0 0 255 
  clear renderer
    
  rendererDrawColor renderer $= V4 255 255 255 255
  
  mem <- readTVarIO memory

  mapM_ (renderPlayer renderer) $ getPlayers mem
    
  events <- pollEvents
  shouldContinue <- fmap and (mapM (handleEvent renderer) events)
  present renderer
  threadDelay 40000
  when shouldContinue $ appLoop memory renderer


