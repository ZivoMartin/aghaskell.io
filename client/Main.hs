{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import SDL.Primitive (circle)
import SDL
import WindowRenderer
import Foreign.C.Types (CInt)
import Control.Concurrent.STM (atomically, newTQueueIO, newTVarIO, TVar, readTVarIO, modifyTVar')

import Network.Socket
import Messages (ClientMessage (..), ServerMessage (..), sendMessage, PlayerData, Ip)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when)
import Network.Socket.ByteString (recvFrom)
import Data.Binary (encode, decodeOrFail)
import Data.ByteString ( ByteString, toStrict, fromStrict )
import Const (servIp)

data Memory = Memory {
  getPlayers :: [PlayerData],
  getMyIp :: Ip
  }


main :: IO ()
main = do
  initializeAll
  window <- createWindow "Aghaskell" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  memory <- newTVarIO (Memory [] ("", ""))

  _ <- forkIO $ udpServer memory
  threadDelay 2000000
  appLoop memory renderer HomeScreen
  destroyWindow window
  memValue <- readTVarIO memory
  closeClient (getMyIp memValue)

handleServerMessage :: TVar Memory -> ServerMessage -> IO ()
handleServerMessage memory (Refresh players) = do
  atomically $ modifyTVar' memory $ \(Memory _ ip) ->
    Memory players ip


getIp :: AddrInfo -> IO (String, String)
getIp addr = do
    return $ case addrAddress addr of
        SockAddrInet port host -> (show host, show port)
        SockAddrInet6 _ _ (h1, h2, h3, h4) _ -> 
            return $ show h1 ++ ":" ++ show h2 ++ ":" ++ show h3 ++ ":" ++ show h4
        _ -> return "Unknown"

connectClient :: Ip -> IO ()
connectClient myIp = sendMessage servIp (toStrict (encode (Connect myIp)))

closeClient :: Ip -> IO ()
closeClient myIp = do
  sendMessage servIp (toStrict (encode (Disconnect myIp)))

udpServer :: TVar Memory -> IO ()
udpServer memory = withSocketsDo $ do
    addr <- resolve "0"
    sock <- openTheSocket addr
    myIp <- getIp addr
    putStrLn $ "ip: " ++ show myIp 
    atomically $ modifyTVar' memory $
      \ _ -> Memory [] myIp

    connectClient myIp
    putStrLn "Listening on client"
    _ <- forever $ do
      (msg, _) <- recvFrom sock 1024
      putStrLn "Message from server"
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
handleEvent renderer event = 
    case eventPayload event of
        MouseButtonEvent mouseEvent -> do
            let P (V2 x y) = mouseButtonEventPos mouseEvent
                radius = 20
            if mouseButtonEventMotion mouseEvent == Pressed
            then do
                return True
            else return True

        QuitEvent -> return False
        _ -> return True

appLoop :: TVar Memory -> Renderer -> WindowState -> IO ()
appLoop memory renderer state = do
    
    rendererDrawColor renderer $= V4 0 255 0 255 
    clear renderer
    drawWindow renderer state
    events <- pollEvents
    -- shouldContinue <- True
    present renderer

    when False $ appLoop memory renderer state

