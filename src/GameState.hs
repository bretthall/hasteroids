module GameState (RotateDir(..),
                  Angle,
                  GameState(..),
                  initState,
                  keyDown,
                  keyUp
                 ) where

import qualified Keys as K
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Time.Clock as DTC

data RotateDir = RotateNone | RotateCW | RotateCCW deriving Eq
type Angle = GL.GLfloat

data GameState = GameState {lastUpdate :: DTC.UTCTime,
                            keys :: K.KeysState,
                            rotateDir :: RotateDir,
                            playerAngle :: Angle}

initState :: DTC.UTCTime -> GameState
initState start = GameState {lastUpdate = start,
                             keys = K.noneDown,
                             rotateDir = RotateNone,
                             playerAngle = 0.0}

keyDown :: GameState -> K.KeyIndex -> GameState
keyDown gs@(GameState{keys = oldKeys, rotateDir = oldDir}) k = if K.isKeyDown oldKeys k
               then gs
               else gs {keys = K.keyDown oldKeys k, rotateDir = getDirAfterKeyDown k oldDir}

getDirAfterKeyDown :: K.KeyIndex -> RotateDir -> RotateDir
getDirAfterKeyDown k old | k == K.left = RotateCCW
                         | k == K.right = RotateCW
                         | otherwise = old

keyUp :: GameState -> K.KeyIndex -> GameState
keyUp gs@(GameState {keys = oldKeys, rotateDir = oldDir}) k = if K.isKeyDown oldKeys k
                                                              then gs {keys = newKeys, rotateDir = getDirAfterKeyUp newKeys oldDir}
                                                              else gs
  where
    newKeys = K.keyUp oldKeys k

getDirAfterKeyUp :: K.KeysState -> RotateDir -> RotateDir
getDirAfterKeyUp ks old | K.isKeyDown ks K.left = RotateCCW
                        | K.isKeyDown ks K.right = RotateCW
                        | otherwise = RotateNone

                            
