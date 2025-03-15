module Main where

import Messages
import Messages (ClientMessage(ClientDummy))

type Position = (Int, Int)
type Ip = String

newtype Client = Client {
  getPosition :: Position,
  getSize :: Int,
  getIp :: Ip
  }

newtype Server = Server {
  getClients :: [Client]
  }


removeClient :: Sever -> Server
removeClient server = undefined

handleClientMessage :: ClientMessage -> IO ()
handleClientMessage ClientDummy = putStrLn "Dummy"



main :: IO ()
main = do
  handleClientMessage ClientDummy
