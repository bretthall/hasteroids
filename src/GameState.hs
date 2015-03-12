module GameState where

import qualified Data.Vector.Unboxed as U

type Position = (Float, Float)
type Positions = U.Vector Position

type Velocity = (Float, Float)
type Velocities = U.Vector Velocity

type Orientation = Float
type Orientations = U.Vector Orientation

type MeshIndex = Int
type MeshIndexes = U.Vector MeshIndex

type Color = (Float, Float, Float)
type Colors = U.Vector Color

data GameState = GameState {positions :: Positions,
                            velocities :: Velocities,
                            orientations :: Orientations,
                            meshes :: MeshIndexes,
                            colors :: Colors}

                            
                            