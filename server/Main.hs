module Main where

-- | The server protocol will be as follow:
-- | Some threads called receivers threads handle the client messages reception and deserialisation
-- Some threads call broadcaster threads handle the server refresh messages broadcast to all the cllients
-- | They both work in parallel
-- | Server receives all the clients messages forwarded by the receivers threads and modify its local view of the game
-- | Every 200 ms, server interrupt the slows broadcaster threads and give the a new cloned view of the game
-- | Broadcaster threads will all use the shared client list to refresh client memory
-- | Once we finished to modify the client list we restard the client message reception phase


import Messages
import Messages (ClientMessage(ClientDummy), ServerMessage)

type Position = (Int, Int)
type Ip = String

data Client = Client {
  getPlayerData :: PlayerData,
  getIp :: Ip
  } 

newtype Server = Server {
  getClients :: [Client]
  }

sendMessage :: ServerMessage -> ()
sendMessage msg = ()

serverHandleClientMessage :: ClientMessage -> IO ()
serverHandleClientMessage ClientDummy = putStrLn "Dummy"

serverRemoveClient :: Server -> Server
serverRemoveClient server = server


serverNewClient :: Server -> Client  -> Server
serverNewClient server client =
  Server (client : getClients server)

main :: IO ()
main = do
  serverHandleClientMessage ClientDummy
