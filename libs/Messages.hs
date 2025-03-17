{-# LANGUAGE DeriveGeneric #-}

module Messages where

import Vec2 (Vec2)
import Data.ByteString ( ByteString )
import Network.Socket.ByteString (sendTo)
import Network.Socket

import GHC.Generics (Generic)
import Data.Binary (Binary)



-- | x, y, size
type PlayerData = (Position, Position, Int) -- Player position, Mouse position, size
type Ip = (String, String) -- ip, port
type Position = Vec2

sendMessage :: Ip -> ByteString -> IO ()
sendMessage (ip, port) message = withSocketsDo $ do
    addr <- resolve ip port
    sock <- openTheSocket addr
    _ <- sendTo sock message (addrAddress addr)
    close sock
  where
    resolve host p = do
        let hints = defaultHints { addrSocketType = Datagram }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just p)
        return addr
    openTheSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

-- | Messages sent by the server
data ServerMessage =
  -- | An array representing the position and the size of all the player in the game
  Refresh [PlayerData]
  deriving (Show, Read, Generic)

instance Binary ServerMessage

-- | Messages sent by the client
data ClientMessage =
  -- | A simple Dummy message
  Connect Ip
  | Disconnect Ip
  | NewMousePos Ip Position
  deriving (Show, Read, Generic)

instance Binary ClientMessage
