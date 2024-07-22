module GameLogic where

import Types
import System.Random (randomRIO)

timeTravel :: TimePeriod -> GameState -> GameState
timeTravel newTime state = state { currentTime = newTime, timeJumps = timeJumps state + 1 }

addItem :: Item -> GameState -> GameState
addItem item state = state { inventory = item : inventory state }

removeItem :: Item -> GameState -> GameState
removeItem item state = state { inventory = filter (/= item) (inventory state) }

resolveAnomaly :: Anomaly -> GameState -> Maybe GameState
resolveAnomaly anomaly state
    | all (`elem` inventory state) (requiredItems anomaly) =
        Just $ state { activeAnomalies = filter (/= anomaly) (activeAnomalies state),
                       resolvedAnomalies = anomaly : resolvedAnomalies state }
    | otherwise = Nothing

generateAnomaly :: TimePeriod -> IO Anomaly
generateAnomaly period = do
    let anomalies = case period of
            AncientEgypt -> [
                Anomaly "A pharaoh is using a smartphone" [Item "Ancient Scroll" "A scroll explaining modern technology"] AncientEgypt,
                Anomaly "Pyramids are being built with modern tools" [Item "Manual Labor Guide" "A guide on ancient construction techniques"] AncientEgypt
                ]
            Renaissance -> [
                Anomaly "Leonardo da Vinci is painting with spray cans" [Item "Traditional Paintbrush" "A paintbrush from the correct era"] Renaissance,
                Anomaly "People are using calculators for math" [Item "Abacus" "An ancient calculation device"] Renaissance
                ]
            FutureCity -> [
                Anomaly "Flying cars are replaced with horse-drawn carriages" [Item "Hover Engine" "A futuristic propulsion device"] FutureCity,
                Anomaly "Robots are using outdated floppy disks" [Item "Quantum Drive" "A data storage device from the future"] FutureCity
                ]
    index <- randomRIO (0, length anomalies - 1)
    return $ anomalies !! index

addNewAnomaly :: GameState -> IO GameState
addNewAnomaly state = do
    newAnomaly <- generateAnomaly (currentTime state)
    return $ state { activeAnomalies = newAnomaly : activeAnomalies state }

displayAnomalies :: GameState -> IO ()
displayAnomalies state = do
    putStrLn "Current Anomalies:"
    mapM_ (\a -> putStrLn $ "- " ++ anomalyDescription a) (activeAnomalies state)

useItem :: String -> GameState -> (GameState, String)
useItem itemName state =
    case filter (\item -> itemName == Types.itemName item) (inventory state) of
        [] -> (state, "You don't have that item.")
        (item:_) ->
            case filter (\anomaly -> item `elem` requiredItems anomaly) (activeAnomalies state) of
                [] -> (state, "That item doesn't resolve any current anomalies.")
                (anomaly:_) ->
                    case resolveAnomaly anomaly state of
                        Just newState -> (newState, "Anomaly resolved: " ++ anomalyDescription anomaly)
                        Nothing -> (state, "You need more items to resolve this anomaly.")