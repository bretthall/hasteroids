{-# LANGUAGE ForeignFunctionInterface #-}
module SdlMain where

import Graphics.UI.SDL

foreign export ccall sdl_main :: IO ()
 
sdl_main :: IO ()
sdl_main = withInit [InitEverything] $ do

  setCaption "Foo says \"Hello!\"" []

  screen <- setVideoMode screenWidth screenHeight screenBmp [SWSurface]

  Graphics.UI.SDL.flip screen

  delay 2000

  where
    screenWidth = 640
    screenHeight = 480
    screenBmp = 32