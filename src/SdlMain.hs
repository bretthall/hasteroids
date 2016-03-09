{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Time.Clock as DTC
import Data.Word
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified SDL
import qualified Linear as L
import qualified Data.Configurator as Cfgr
import qualified Data.Configurator.Types as CfgrTypes
import Control.Exception.Base (catch)
import System.Exit (exitFailure)

data RotateDir = RotateNone | RotateCW | RotateCCW deriving Eq
data RotateState = RS RotateDir GL.GLfloat DTC.UTCTime

data Config = Config {
  windowWidth :: Int,
  windowHeight :: Int,
  playerAngularVelocity :: GL.GLfloat
  } deriving Show

readConfig :: IO Config
readConfig = do
  cfg <- catch (Cfgr.load [Cfgr.Required "hasteroids.config"]) handleMissingCfgr
  wd <- Cfgr.lookupDefault 640 cfg "window.width" 
  ht <- Cfgr.lookupDefault 480 cfg "window.height" 
  pav <- Cfgr.lookupDefault 100 cfg "player.angular-velocity"
  return (Config wd ht pav)
    where
      handleMissingCfgr :: CfgrTypes.ConfigError -> IO a
      handleMissingCfgr err = do
        print err
        exitFailure
              
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
  GL.ortho2D (-100.0) 100.0 (-100.0) 100.0
  GL.matrixMode $= GL.Modelview 0
  start <- DTC.getCurrentTime
  mainLoop config win $ RS RotateNone 0.0 start                    
  SDL.destroyWindow win
  SDL.quit

mainLoop :: Config -> SDL.Window -> RotateState -> IO ()
mainLoop cfg win state@(RS d a t) = do
  event <- SDL.pollEvent
  case event of
   Just (SDL.Event _ SDL.QuitEvent) -> return ()
   Just (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ t False k))) ->
     handleKey cfg t k state >>= mainLoop cfg win
   Nothing -> do
     ct <- DTC.getCurrentTime
     let w = playerAngularVelocity cfg
     let state' = RS d (updateAngle w d a t ct) ct
     renderWindow state' win
     mainLoop cfg win state'
   _ -> mainLoop cfg win state
   
handleKey :: Config -> SDL.InputMotion -> SDL.Keysym -> RotateState -> IO RotateState
handleKey cfg SDL.Pressed key state = handleKeyDown cfg key state
handleKey cfg SDL.Released key state = handleKeyUp cfg key state

handleKeyDown :: Config -> SDL.Keysym -> RotateState -> IO RotateState
handleKeyDown cfg key state@(RS d a t) | SDL.keysymKeycode key == SDL.KeycodeLeft = updateState RotateCCW
                                       | SDL.keysymKeycode key == SDL.KeycodeRight = updateState RotateCW
                                       | otherwise = return state
  where
    updateState dir = do
      ct <- DTC.getCurrentTime
      return $ RS dir (updateAngle w d a t ct) ct
    w = playerAngularVelocity cfg

handleKeyUp :: Config -> SDL.Keysym -> RotateState -> IO RotateState
handleKeyUp cfg key state@(RS d a t) | SDL.keysymKeycode key == SDL.KeycodeLeft = updateState RotateCCW
                                     | SDL.keysymKeycode key == SDL.KeycodeRight = updateState RotateCW
                                     | otherwise = return state
  where
    updateState dir | dir == d = do
                        ct <- DTC.getCurrentTime
                        return $ RS RotateNone (updateAngle w d a t ct) ct
                    | otherwise = return state
    w = playerAngularVelocity cfg

updateAngle :: GL.GLfloat -> RotateDir -> GL.GLfloat -> DTC.UTCTime -> DTC.UTCTime -> GL.GLfloat
updateAngle w d a t1 t2 = case d of
                         RotateCCW -> a + diff*w
                         RotateCW -> a - diff*w
                         RotateNone -> a
  where
    diff = realToFrac $ DTC.diffUTCTime t2 t1    

shipScaleFactor :: GL.GLfloat
shipScaleFactor = 0.35

renderWindow :: RotateState -> SDL.Window -> IO ()
renderWindow (RS _ a _) win = do
  GL.clear [GL.ColorBuffer]
  GL.loadIdentity
  GL.rotate a $ GL.Vector3 0.0 0.0 1.0
  GL.color $ GL.Color3 0.0 1.0 (0.0 :: GL.GLfloat)
  GL.scale shipScaleFactor shipScaleFactor 1.0
  GL.renderPrimitive GL.LineLoop $ mapM_ vertex2f [(-5.0, -5.0), (0.0, 10.0), (5.0, -5.0), (0.0, 0.0) ]
  SDL.glSwapWindow win

vertex2f :: (GL.GLfloat, GL.GLfloat) -> IO ()
vertex2f (x, y) = GL.vertex $ GL.Vertex2 x y
