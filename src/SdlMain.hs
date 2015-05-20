{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Time.Clock as DTC
import Data.Word
import qualified Foreign.C.String as FCS
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Storable as FS
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.SDL as SDL

data RotateDir = RotateNone | RotateCW | RotateCCW deriving Eq
data RotateState = RS RotateDir GL.GLfloat DTC.UTCTime
  
main :: IO ()
main = do
    initRes <- SDL.init SDL.SDL_INIT_EVERYTHING
    putStrLn $ "Init res = " ++ (show initRes)
    title <- FCS.newCString "Hasteroids"
    let (width, height) = (640, 640)
    win <- SDL.createWindow title 0 0 width height SDL.SDL_WINDOW_OPENGL
    SDL.glCreateContext win
    GL.clearColor $= GL.Color4 0 0 0 1
    GL.lineWidth $= 1
    GL.lineSmooth $= GL.Enabled
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D (-100.0) 100.0 (-100.0) 100.0
    GL.matrixMode $= GL.Modelview 0
    start <- DTC.getCurrentTime
    mainLoop win $ RS RotateNone 0.0 start                    
    SDL.destroyWindow win
    SDL.quit

mainLoop :: SDL.Window -> RotateState -> IO ()
mainLoop win state@(RS d a t) = do
  event <- checkForEvent
  case event of
   Just (SDL.QuitEvent _ _) -> return ()
   Just (SDL.KeyboardEvent t _ _ _ _ k) -> handleKey t k state >>= mainLoop win
   Nothing -> do
     ct <- DTC.getCurrentTime
     let state' = RS d (updateAngle d a t ct) ct
     renderWindow state' win
     mainLoop win state'
   _ -> mainLoop win state

checkForEvent :: IO (Maybe SDL.Event)
checkForEvent = FMA.alloca $ \event -> do
  res <- SDL.pollEvent event
  case res of
   1 -> FS.peek event >>= return.Just
   _ -> return Nothing
   
handleKey :: Word32 -> SDL.Keysym -> RotateState -> IO RotateState
handleKey t key state | t == SDL.SDL_KEYDOWN = handleKeyDown key state
                      | t == SDL.SDL_KEYUP = handleKeyUp key state
                      | otherwise = return state

handleKeyDown :: SDL.Keysym -> RotateState -> IO RotateState
handleKeyDown key state@(RS d a t) | SDL.keysymKeycode key == SDL.SDLK_LEFT = updateState RotateCCW
                                   | SDL.keysymKeycode key == SDL.SDLK_RIGHT = updateState RotateCW
                                   | otherwise = return state
  where
    updateState dir = do
      ct <- DTC.getCurrentTime
      return $ RS dir (updateAngle d a t ct) ct

handleKeyUp :: SDL.Keysym -> RotateState -> IO RotateState
handleKeyUp key state@(RS d a t) | SDL.keysymKeycode key == SDL.SDLK_LEFT = updateState RotateCCW
                                 | SDL.keysymKeycode key == SDL.SDLK_RIGHT = updateState RotateCW
                                 | otherwise = return state
  where
    updateState dir | dir == d = do
                        ct <- DTC.getCurrentTime
                        return $ RS RotateNone (updateAngle d a t ct) ct
                    | otherwise = return state

angVel :: GL.GLfloat
angVel = 100.0
                                  
updateAngle :: RotateDir -> GL.GLfloat -> DTC.UTCTime -> DTC.UTCTime -> GL.GLfloat
updateAngle d a t1 t2 = case d of
                         RotateCCW -> a + diff*angVel
                         RotateCW -> a - diff*angVel
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
