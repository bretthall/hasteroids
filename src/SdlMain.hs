{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.IORef
import Data.Time.Clock
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL as SDL

data RotateDir = RotateNone | RotateCW | RotateCCW deriving Eq
data RotateState = RS RotateDir GLfloat UTCTime
  
main :: IO ()
main = do
    initRes <- SDL.init SDL_INIT_EVERYTHING
    putStrLn $ "Init res = " ++ (show initRes)

    title <- newCString "Test"
    let (width, height) = (640, 480)
    win <- createWindow title 0 0 width height SDL.SDL_WINDOW_OPENGL
    glCreateContext win
    clearColor $= Color4 0 0 0 1
    lineWidth $= 1
    lineSmooth $= Enabled
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    matrixMode $= Projection
    loadIdentity
    ortho2D (-1.0) 1.0 (-1.0) 1.0
    matrixMode $= Modelview 0
    start <- getCurrentTime
    state <- newIORef $ RS RotateNone 0.0 start
    
    let mainLoop = do
          event <- alloca $ \event -> do
                     res <- pollEvent event
                     case res of
                       1 -> peek event >>= return.Just
                       _ -> return Nothing
          case event of
            Just (QuitEvent _ _) -> print "quit" >> return ()
            Just (KeyboardEvent t _ _ _ _ k) -> handleKey t k state >> mainLoop
            Nothing -> do
              ct <- getCurrentTime
              (RS d a t) <- readIORef state
              let s' = RS d (updateAngle d a t ct) ct
              writeIORef state s'
              renderWindow s' win
              mainLoop
            _ -> mainLoop
    mainLoop
                    
    destroyWindow win
    quit

handleKey :: Word32 -> Keysym -> IORef RotateState -> IO ()
handleKey t key state | t == SDL_KEYDOWN = handleKeyDown key state
                      | t == SDL_KEYUP = handleKeyUp key state
                      | otherwise = return ()

handleKeyDown :: Keysym -> IORef RotateState -> IO ()
handleKeyDown key state | keysymKeycode key == SDLK_LEFT = updateState RotateCCW
                        | keysymKeycode key == SDLK_RIGHT = updateState RotateCW
                        | otherwise = return ()
  where
    updateState dir = do
      ct <- getCurrentTime
      modifyIORef state (\(RS d a t) -> RS dir (updateAngle d a t ct) ct)

handleKeyUp :: Keysym -> IORef RotateState -> IO ()
handleKeyUp key state | keysymKeycode key == SDLK_LEFT = updateState RotateCCW
                      | keysymKeycode key == SDLK_RIGHT = updateState RotateCW
                      | otherwise = return ()
  where
    updateState dir = do
      ct <- getCurrentTime
      (RS d a t) <- readIORef state
      if d == dir
        then writeIORef state $ RS RotateNone (updateAngle d a t ct) ct
        else return () 
        
updateAngle :: RotateDir -> GLfloat -> UTCTime -> UTCTime -> GLfloat
updateAngle d a t1 t2 = case d of
                         RotateCCW -> a + diff*angVel
                         RotateCW -> a - diff*angVel
                         RotateNone -> a
  where
    diff = realToFrac $ diffUTCTime t2 t1
    
angVel :: GLfloat
angVel = 8.0

renderWindow :: RotateState -> Window -> IO ()
renderWindow (RS _ a _) win = do
  clear [ColorBuffer]
  loadIdentity
  rotate a $ Vector3 0.0 0.0 1.0
  color $ Color3 1.0 0.0 (0.0 :: GLfloat)
  renderPrimitive Lines $ mapM_ vertex2f [(-0.5, -0.5), (0.5, 0.5), (0.5, -0.5), (-0.5, 0.5) ]
  color $ Color3 0.0 1.0 (0.0 :: GLfloat)
  renderPrimitive LineLoop $ mapM_ vertex2f [(-0.5, -0.5), (-0.5, 0.5), (0.5, 0.5), (0.5, -0.5) ]
  glSwapWindow win

vertex2f :: (GLfloat, GLfloat) -> IO ()
vertex2f (x, y) = vertex $ Vertex2 x y
