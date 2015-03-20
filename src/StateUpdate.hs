module StateUpdate
    (TimeDelta,
     Key,
     MessageData,
     Time,
     Message,
     StateUpdate
    ) where

import GameState

type TimeDelta = Float
type Key = Int
data MessageData = QuitMsg | KeyMsg Key
type Time = Int
data Message = Message Time MessageData
                 
type StateUpdate = GameState -> TimeDelta -> [Message] -> GameState
