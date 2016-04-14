module Simulation (updateState
                  ) where

import qualified GameState as GS
import qualified Data.Time.Clock as DTC
import qualified Config as C
import qualified Graphics.Rendering.OpenGL as GL

updateState :: C.Config -> GS.GameState -> DTC.UTCTime -> GS.GameState
updateState (C.Config {C.playerAngularVelocity = angVel}) gs@(GS.GameState {GS.lastUpdate = lu, GS.rotateDir = dir, GS.playerAngle = oldAngle}) t =
  gs {GS.playerAngle = updateAngle angVel dir oldAngle lu t, GS.lastUpdate = t}

updateAngle :: GL.GLfloat -> GS.RotateDir -> GL.GLfloat -> DTC.UTCTime -> DTC.UTCTime -> GL.GLfloat
updateAngle w d a t1 t2 = case d of
                           GS.RotateCCW -> a + diff*w
                           GS.RotateCW -> a - diff*w
                           GS.RotateNone -> a
  where
    diff = realToFrac $ DTC.diffUTCTime t2 t1    


         
