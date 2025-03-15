module Messages where 

import Data.ByteString

-- | Messages sent by the server
data ServerMessage =
  -- | A simple Dummy message
  ServerDummy
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
