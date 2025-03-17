{-# LANGUAGE DeriveGeneric #-}

module Vec2 where


type Vec2 = (Int, Int)

vec2Norm :: Vec2 -> Vec2 -> Int -> Vec2
vec2Norm (x1, y1) (x2, y2) dist = 
  (quot x dist, quot y dist)
  where
    x = x1 - x2
    y = y1 - y2

vec2Dist :: Vec2 -> Vec2 -> Int
vec2Dist (x1, y1) (x2, y2) = floor(sqrt (fromIntegral d))
  where
    d = dist_x * dist_x + dist_y * dist_y
      where
        dist_x = x2 - x1
        dist_y = y2 - y1
