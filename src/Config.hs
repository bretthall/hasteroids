{-# LANGUAGE OverloadedStrings #-}
module Config (Config (..),
               readConfig)
       where

import qualified Data.Configurator as Cfgr
import qualified Data.Configurator.Types as CfgrTypes
import qualified Graphics.Rendering.OpenGL as GL
import Control.Exception.Base (catch)
import System.Exit (exitFailure)

data Config = Config {
  windowWidth :: Int,
  windowHeight :: Int,
  playerAngularVelocity :: GL.GLfloat,
  shipScale :: GL.GLfloat,
  drawSquare :: Bool
  } deriving Show

readConfig :: IO Config
readConfig = do
  cfg <- catch (Cfgr.load [Cfgr.Required "hasteroids.config"]) handleMissingCfgr
  wd <- Cfgr.lookupDefault 640 cfg "window.width" 
  ht <- Cfgr.lookupDefault 480 cfg "window.height" 
  pav <- Cfgr.lookupDefault 100 cfg "player.angular-velocity"
  ss <- Cfgr.lookupDefault 0.35 cfg "player.ship-scale"
  ds <- Cfgr.lookupDefault False cfg "debug.draw-square"
  return (Config wd ht pav ss ds)
    where
      handleMissingCfgr :: CfgrTypes.ConfigError -> IO a
      handleMissingCfgr err = do
        print err
        exitFailure
