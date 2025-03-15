module WindowRenderer
  ( WindowState (..),
    drawWindow,
  )
where

import SDL

-- | CurrentWindow to be displayed
data WindowState = HomeScreen | GameScreen deriving (Eq, Show)

-- | Execute specific code depending on window to be displayed
drawWindow :: Renderer -> WindowState -> IO ()
drawWindow renderer currentWindow = do
  case currentWindow of
    HomeScreen ->
      rendererDrawColor renderer $= V4 0 0 255 255
    GameScreen ->
      rendererDrawColor renderer $= V4 0 255 0 255
