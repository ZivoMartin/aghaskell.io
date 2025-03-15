module Messages where 

import Data.ByteString


-- | x, y, size
type PlayerData = (Int, Int, Int)

-- | Messages sent by the server
data ServerMessage =
  -- | An array representing the position and the size of all the player in the game
  -- | The position of the player that receiv the message, eventually with a new size
  -- | Note that the players list only contains other players
  Refresh [PlayerData] PlayerData
  -- | Sent when someone eat the player. 
  | Killed
  deriving (Show, Read)

-- | Messages sent by the client
data ClientMessage =
  -- | A simple Dummy message
  ClientDummy
  deriving (Show, Read)


server_serialize :: ServerMessage -> ByteString
server_serialize msg = undefined

server_deserialize :: ByteString -> ServerMessage
server_deserialize msg = undefined


client_serialize :: ClientMessage -> ByteString
client_serialize msg = undefined

client_deserialize :: ByteString -> ClientMessage
client_deserialize msg = undefined
