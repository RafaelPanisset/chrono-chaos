module GameLogic where

import Types

timeTravel :: TimePeriod -> GameState -> GameState
timeTravel newTime state = state { currentTime = newTime, timeJumps = timeJumps state + 1 }

addItem :: Item -> GameState -> GameState
addItem item state = state { inventory = item : inventory state }

removeItem :: Item -> GameState -> GameState
removeItem item state = state { inventory = filter (/= item) (inventory state) }

resolveAnomaly :: Anomaly -> GameState -> Maybe GameState
resolveAnomaly anomaly state
    | all (`elem` inventory state) (requiredItems anomaly) =
        Just $ state { resolvedAnomalies = anomaly : resolvedAnomalies state }
    | otherwise = Nothing