{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import SDL
import WindowRenderer

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer HomeScreen
  destroyWindow window

appLoop :: Renderer -> WindowState -> IO ()
appLoop renderer state = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed
              && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          QuitEvent -> True
          _ -> False
      eventIsLeftClick event =
        case eventPayload event of
          MouseButtonEvent buttonEvent ->
            mouseButtonEventButton buttonEvent == ButtonLeft
          _ -> False
      leftClicked = any eventIsLeftClick events
      qPressed = any eventIsQPress events
      newState = if leftClicked && (state == HomeScreen) then GameScreen else state
  drawWindow renderer newState
  clear renderer
  present renderer
  unless qPressed (appLoop renderer newState)
