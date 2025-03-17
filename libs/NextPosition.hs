module NextPosition where

import Messages (PlayerData)
import Vec2 (vec2Norm, vec2Dist, vec2Scale)
import Const (defaultSpeed)

nextPosition :: PlayerData -> PlayerData
nextPosition ((x, y), mPos, size) =
    if mPos == (x, y)
    then ((x, y), mPos, size)
    else
      let norm = vec2Norm (x, y) mPos
      in let (dx, dy) = (vec2Scale norm defaultSpeed)
      in ((x + floor dx, y + floor dy), mPos, size)
