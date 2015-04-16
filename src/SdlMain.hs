{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.UI.SDL as SDL
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
    
main :: IO ()
main = do
    initRes <- SDL.init SDL_INIT_EVERYTHING
    putStrLn $ "Init res = " ++ (show initRes)

    title <- newCString "Test"
    w <- createWindow title 0 0 500 500 0

    let mainLoop = do
          event <- alloca $ \event -> do
                     res <- pollEvent event
                     case res of
                       1 -> peek event >>= return.Just
                       _ -> return Nothing
          case event of
            Just (QuitEvent _ _) -> print "quit" >> return ()
            Just (KeyboardEvent _ _ _ _ _ _) -> print "key" >> return ()
            _ -> mainLoop
    mainLoop
                    
    destroyWindow w
    quit