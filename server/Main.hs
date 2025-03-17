module Main where

import Const (mapWidth, mapHeight)

import Network.Socket
import Network.Socket.ByteString (recvFrom)
import Messages ( ClientMessage(..), ServerMessage (..), PlayerData, Ip, sendMessage, Position )

import Control.Monad (forever)
import Data.ByteString ( ByteString, toStrict, fromStrict )
import Data.Binary (encode, decodeOrFail)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TQueue, atomically, readTQueue, writeTQueue, newTQueueIO, newTVarIO, TVar, readTVarIO, modifyTVar')


-- | The server protocol will be as follow:
-- | Some threads called receivers threads handle the client messages reception and deserialisation
-- | Some threads call broadcaster threads handle the server refresh messages broadcast to all the clients
-- | They both work in parallel
-- | Server receives all the clients messages forwarded by the receivers threads and modify its local view of the game
-- | Every 200 ms, server interrupt the slows broadcaster threads and give the a new cloned view of the game
-- | Broadcaster threads will all use the shared client list to refresh client memory
-- | Once we finished to modify the client list we restard the client message reception phase

import System.Random (randomRIO)

randomPosition :: IO Position
randomPosition = do
    x <- randomRIO (0, mapWidth)
    y <- randomRIO (0, mapHeight)
    return (x, y)

data Client = Client {
  getPlayerData :: !PlayerData,
  getIp :: !Ip
  } 

defaultPlayerData :: IO PlayerData
defaultPlayerData = do
  pos <- randomPosition
  return (pos, 10) 

newtype Server = Server {
  getClients :: [Client]
 } 


serverBroadcastDatas :: TVar Server -> IO ([Ip], [PlayerData])
serverBroadcastDatas server = do
  serverValue <- readTVarIO server  
  let clients = getClients serverValue  
  return (serverBroadcastDatasRec clients [] [])

  where
    serverBroadcastDatasRec :: [Client] -> [Ip] -> [PlayerData] -> ([Ip], [PlayerData])
    serverBroadcastDatasRec [] ips playersDatas = (ips, playersDatas)
    serverBroadcastDatasRec (client : t) ips playersDatas =
      serverBroadcastDatasRec t (getIp client : ips) (getPlayerData client : playersDatas)


serverHandleClientMessage :: TVar Server -> ClientMessage -> IO ()
serverHandleClientMessage server (Connect ip) = do
  playerData <- defaultPlayerData
  atomically $ modifyTVar' server $ \(Server clients) ->
    Server (Client playerData ip : clients)

serverHandleClientMessage server (Disconnect ip) = do
  atomically $ modifyTVar' server $ \(Server clients) -> Server (removeClient clients)
    where
      removeClient :: [Client] -> [Client]
      removeClient [] = []
      removeClient (Client clientData clientIp : t)
        | ip == clientIp = t
        | otherwise =  Client clientData clientIp : removeClient t


  -- | Listen on the network, deserialize messages and send it to the main
serverListener :: TQueue ClientMessage -> TVar Server -> IO ()
serverListener queue server = forever $ do
  msg <- atomically $ readTQueue queue
  serverHandleClientMessage server msg


broadcastGameState :: [Ip] -> [PlayerData] -> IO ()
broadcastGameState ips playersDatas =
  mapM_ (sendEncodedMessage (toStrict (encode (Refresh playersDatas)))) ips 
  where
    sendEncodedMessage :: ByteString -> Ip -> IO ()
    sendEncodedMessage playerData ip = sendMessage ip playerData

broadcastInitializer :: TVar Server -> IO ()
broadcastInitializer server = forever $ do
  threadDelay 200000 -- 200 ms
  (ips, playersDatas) <- serverBroadcastDatas server 
  forkIO $ broadcastGameState ips playersDatas


serverPort :: String
serverPort = "4242"

-- | Connect on UDP and forward all deserialized messages to the server 
udpServer :: TQueue ClientMessage -> IO ()
udpServer queue = withSocketsDo $ do
    addr <- resolve serverPort
    sock <- openTheSocket addr
    putStrLn $ "Listening on port  " ++ serverPort
    _ <- forever $ do
      (msg, _) <- recvFrom sock 1024  -- Réception d'un message en `ByteString strict`
      putStrLn "New message !"
      case decodeOrFail (fromStrict msg) of
        Left (_, _, err) -> putStrLn $ "Decoding error: " ++ err
        Right (_, _, clientMessage) -> atomically $ writeTQueue queue clientMessage
          
    close sock
  where
    resolve port = do
        let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Datagram }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
        return addr
    openTheSocket addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      bind sock (addrAddress addr)
      return sock


main :: IO ()
main = do
  server <- newTVarIO (Server [])
  queue <- newTQueueIO 
  _ <- forkIO $ serverListener queue server
  _ <- forkIO $ broadcastInitializer server
  udpServer queue
