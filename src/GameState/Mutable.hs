module GameState.Mutable (MPositions,
                          MVelocities,
                          MOrientations,
                          MMeshIndexes,
                          MColors,
                          MGameState(..),
                          thawGameState,
                          freezeGameState
                          ) where

import GameState
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Monad.Primitive (PrimMonad, PrimState)

type MPositions s = UM.MVector s Position

type MVelocities s = UM.MVector s Velocity

type MOrientations s = UM.MVector s Orientation

type MMeshIndexes s = UM.MVector s MeshIndex

type MColors s = UM.MVector s Color

data MGameState s = MGameState {mpositions :: MPositions s,
                                mvelocities :: MVelocities s,
                                morientations :: MOrientations s,
                                mmeshes :: MMeshIndexes s,
                                mcolors :: MColors s}

thawGameState :: PrimMonad m => GameState -> m (MGameState (PrimState m))
thawGameState (GameState ps vs os ms cs) = do
  ps' <- U.unsafeThaw ps
  vs' <- U.unsafeThaw vs
  os' <- U.unsafeThaw os
  ms' <- U.unsafeThaw ms
  cs' <- U.unsafeThaw cs
  return $ MGameState ps' vs' os' ms' cs'

freezeGameState :: PrimMonad m => MGameState (PrimState m) -> m GameState
freezeGameState (MGameState ps vs os ms cs) = do
  ps' <- U.unsafeFreeze ps
  vs' <- U.unsafeFreeze vs
  os' <- U.unsafeFreeze os
  ms' <- U.unsafeFreeze ms
  cs' <- U.unsafeFreeze cs
  return $ GameState ps' vs' os' ms' cs'

                            
                            