{-# LANGUAGE OverloadedStrings #-}
module Main where

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL as SDL
    
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

    let mainLoop = do
          event <- alloca $ \event -> do
                     res <- pollEvent event
                     case res of
                       1 -> peek event >>= return.Just
                       _ -> return Nothing
          case event of
            Just (QuitEvent _ _) -> print "quit" >> return ()
            Just (KeyboardEvent _ _ _ _ _ _) -> print "key" >> return ()
            Nothing -> renderWindow win >> mainLoop -- no events pending => do rendering here
            _ -> mainLoop
    mainLoop
                    
    destroyWindow win
    quit

renderWindow :: Window -> IO ()
renderWindow win = do
  clear [ColorBuffer]
  loadIdentity
  color $ Color3 1.0 0.0 (0.0 :: GLfloat)
  renderPrimitive Lines $ mapM_ vertex2f [(-0.5, -0.5), (0.5, 0.5), (0.5, -0.5), (-0.5, 0.5) ]
  color $ Color3 0.0 1.0 (0.0 :: GLfloat)
  renderPrimitive LineLoop $ mapM_ vertex2f [(-0.5, -0.5), (-0.5, 0.5), (0.5, 0.5), (0.5, -0.5) ]
  glSwapWindow win

vertex2f :: (GLfloat, GLfloat) -> IO ()
vertex2f (x, y) = vertex $ Vertex2 x y
