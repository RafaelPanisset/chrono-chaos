module Main where

import Types
import GameLogic
import System.IO
import Control.Monad (when)

main :: IO ()
main = do
    initialState <- addNewAnomaly $ GameState AncientEgypt [] [] [] 0
    gameLoop initialState

gameLoop :: GameState -> IO ()
gameLoop state = do
    putStrLn $ "\nCurrent Time: " ++ show (currentTime state)
    putStrLn $ "Inventory: " ++ show (map itemName $ inventory state)
    putStrLn $ "Time Jumps: " ++ show (timeJumps state)
    displayAnomalies state
    putStrLn "\nEnter command (travel/pickup/use/quit):"
    command <- getLine
    case command of
        "travel" -> do
            putStrLn "Enter destination (AncientEgypt/Renaissance/FutureCity):"
            destinationStr <- getLine
            case reads destinationStr of
                [(destination, "")] -> do
                    let newState = timeTravel destination state
                    updatedState <- addNewAnomaly newState
                    gameLoop updatedState
                _ -> putStrLn "Invalid destination" >> gameLoop state
        "pickup" -> do
            putStrLn "Enter item name:"
            itemName <- getLine
            let newItem = Item itemName "A mysterious item"
            gameLoop $ addItem newItem state
        "use" -> do
            putStrLn "Enter item name to use:"
            itemName <- getLine
            let (newState, message) = useItem itemName state
            putStrLn message
            gameLoop newState
        "quit" -> putStrLn "Thanks for playing!"
        _ -> putStrLn "Unknown command" >> gameLoop state