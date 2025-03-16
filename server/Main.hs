module Main where
import Messages ( ClientMessage(..), ServerMessage (..), PlayerData, Ip, sendMessage )
import Control.Concurrent.STM (TQueue)
import Control.Monad (forever)
import Data.ByteString.Lazy ( ByteString )
import Data.Binary (encode)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, readTQueue, readTVar, newTQueueIO, writeTQueue, newTVarIO, TVar, readTVarIO)

-- | The server protocol will be as follow:
-- | Some threads called receivers threads handle the client messages reception and deserialisation
-- Some threads call broadcaster threads handle the server refresh messages broadcast to all the clients
-- | They both work in parallel
-- | Server receives all the clients messages forwarded by the receivers threads and modify its local view of the game
-- | Every 200 ms, server interrupt the slows broadcaster threads and give the a new cloned view of the game
-- | Broadcaster threads will all use the shared client list to refresh client memory
-- | Once we finished to modify the client list we restard the client message reception phase


type Position = (Int, Int)

data Client = Client {
  getPlayerData :: PlayerData,
  getIp :: Ip
  } 

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

serverHandleClientMessage :: ClientMessage -> IO ()
serverHandleClientMessage ClientDummy = putStrLn "Dummy"

serverRemoveClient :: Server -> Server
serverRemoveClient server = server


serverNewClient :: Server -> Client  -> Server
serverNewClient server client =
  Server (client : getClients server)


  -- | Listen on the network, deserialize messages and send it to the main
serverListener :: TQueue ClientMessage -> TVar Server -> IO ()
serverListener queue server = forever $ do
  msg <- atomically $ readTQueue queue
  putStrLn "Message"

broadcastGameState :: [Ip] -> [PlayerData] -> IO ()
broadcastGameState ips playersDatas =
  if null ips || null playersDatas
    then putStrLn "Error: Empty input lists"
    else mapM_ (sendEncodedMessage (encode (Refresh playersDatas))) ips 
  where
    sendEncodedMessage :: ByteString -> Ip -> IO ()
    sendEncodedMessage playerData ip = sendMessage ip playerData




broadcastInitializer :: TVar Server -> IO ()
broadcastInitializer server = forever $ do
  threadDelay 200000 -- 200 ms
  (ips, playersDatas) <- serverBroadcastDatas server 
  _ <- forkIO $ broadcastGameState ips playersDatas
  putStrLn "OK"


main :: IO ()
main = do
  server <- newTVarIO (Server [])
  queue <- newTQueueIO 
  _ <- forkIO $ serverListener queue server
  _ <- forkIO $ broadcastInitializer server
  forever $ do
    _ <- atomically $ writeTQueue queue ClientDummy
    threadDelay (500 * 1000) -- 500ms delay
