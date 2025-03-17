module Vec2 where


type Vec2 = (Int, Int)

vec2Norm :: Vec2 -> Vec2 -> (Float, Float)
vec2Norm (x1, y1) (x2, y2) =
  let dist = vec2Dist (x1, y1) (x2, y2) in
    (fromIntegral x / fromIntegral dist, fromIntegral y / fromIntegral dist)
  where
    x = x2 - x1
    y = y2 - y1

vec2Dist :: Vec2 -> Vec2 -> Int
vec2Dist (x1, y1) (x2, y2) = floor(sqrt (fromIntegral d))
  where
    d = dist_x * dist_x + dist_y * dist_y
      where
        dist_x = x2 - x1
        dist_y = y2 - y1

vec2Scale :: (Float, Float) -> Int -> (Float, Float)
vec2Scale (x, y) d = (x * fromIntegral d, y * fromIntegral  d)
