{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Time.Clock as DTC
import Data.Word
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified SDL
import qualified Linear as L
import Config
import GameState
import Simulation
import qualified Keys as K

main :: IO ()
main = do
  config <- readConfig
  print config
  SDL.initialize [SDL.InitVideo, SDL.InitEvents]
  let (width, height) = ((fromIntegral $ windowWidth config), (fromIntegral $ windowHeight config))
  win <- SDL.createWindow "Hasteroids" $ 
         SDL.defaultWindow {
           SDL.windowInitialSize = L.V2 width height,
           SDL.windowOpenGL = Just SDL.defaultOpenGL}
  SDL.glCreateContext win
  GL.clearColor $= GL.Color4 0 0 0 1
  GL.lineWidth $= 1
  GL.lineSmooth $= GL.Enabled
  GL.viewport $= (GL.Position 0 0, GL.Size width height)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  let orthoWidth = 100.0
  let orthoHeight = orthoWidth * ((fromIntegral height) / (fromIntegral width))
  GL.ortho2D (-orthoWidth) orthoWidth (-orthoHeight) orthoHeight
  GL.matrixMode $= GL.Modelview 0
  start <- DTC.getCurrentTime
  mainLoop config win $ initState start                    
  SDL.destroyWindow win
  SDL.quit

mainLoop :: Config -> SDL.Window -> GameState -> IO ()
mainLoop cfg win state@(GameState {lastUpdate = t}) = do
  event <- SDL.pollEvent
  case event of
   Just (SDL.Event _ SDL.QuitEvent) -> return ()
   Just (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ t False k))) -> mainLoop cfg win $ handleKey t k state
   Nothing -> do
     ct <- DTC.getCurrentTime
     let state' = updateState cfg state ct
     renderWindow cfg state' win
     mainLoop cfg win state'
   _ -> mainLoop cfg win state
   
handleKey :: SDL.InputMotion -> SDL.Keysym -> GameState -> GameState
handleKey SDL.Pressed key state = handleKeyDown key state
handleKey SDL.Released key state = handleKeyUp key state

handleKeyDown :: SDL.Keysym -> GameState -> GameState
handleKeyDown key state | SDL.keysymKeycode key == SDL.KeycodeLeft = keyDown state K.left
                        | SDL.keysymKeycode key == SDL.KeycodeRight = keyDown state K.right
                        | otherwise = state

handleKeyUp :: SDL.Keysym -> GameState -> GameState
handleKeyUp key state | SDL.keysymKeycode key == SDL.KeycodeLeft = keyUp state K.left
                      | SDL.keysymKeycode key == SDL.KeycodeRight = keyUp state K.right
                      | otherwise = state

renderWindow :: Config -> GameState -> SDL.Window -> IO ()
renderWindow cfg gs win = do
  GL.clear [GL.ColorBuffer]
  GL.loadIdentity
  GL.color $ GL.Color3 0.0 1.0 (0.0 :: GL.GLfloat)
  let shipScaleFactor = shipScale cfg
  GL.scale shipScaleFactor shipScaleFactor 1.0
  if (drawSquare cfg)
    then GL.renderPrimitive GL.LineLoop $ mapM_ vertex2f [(-30.0, -30.0), (-30.0, 30.0), (30.0, 30.0), (30.0, -30.0) ]
    else return ()
  GL.rotate (playerAngle gs) $ GL.Vector3 0.0 0.0 1.0
  GL.renderPrimitive GL.LineLoop $ mapM_ vertex2f [(-5.0, -5.0), (0.0, 10.0), (5.0, -5.0), (0.0, 0.0) ]
  SDL.glSwapWindow win

vertex2f :: (GL.GLfloat, GL.GLfloat) -> IO ()
vertex2f (x, y) = GL.vertex $ GL.Vertex2 x y
