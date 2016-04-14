module Keys (KeysState,
             KeyIndex,
             noneDown,
             keyDown,
             isKeyDown,
             keyUp,
             left,
             right
            ) where

import Data.Bits as DB

newtype KeysState = KeyState Int deriving (Show, Eq)
newtype KeyIndex = KeyIndex Int deriving (Show, Eq)

noneDown :: KeysState
noneDown = KeyState 0

keyDown :: KeysState -> KeyIndex -> KeysState
keyDown (KeyState s) (KeyIndex i) = KeyState $ DB.setBit s i 

isKeyDown :: KeysState -> KeyIndex -> Bool
isKeyDown (KeyState s) (KeyIndex i) = DB.testBit s i 

keyUp :: KeysState -> KeyIndex -> KeysState
keyUp (KeyState s) (KeyIndex i) = KeyState $ DB.clearBit s i 

left :: KeyIndex
left = KeyIndex 0

right :: KeyIndex
right = KeyIndex 1

