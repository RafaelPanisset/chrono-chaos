module Main where

import Types
import GameLogic
import System.IO
import Text.Read (readMaybe)

main :: IO ()
main = gameLoop initialGameState

initialGameState :: GameState
initialGameState = GameState AncientEgypt [] [] 0

gameLoop :: GameState -> IO ()
gameLoop state = do
    putStrLn $ "Current Time: " ++ show (currentTime state)
    putStrLn $ "Inventory: " ++ show (map itemName $ inventory state)
    putStrLn $ "Time Jumps: " ++ show (timeJumps state)
    putStrLn "Enter command (travel/pickup/use/quit):"
    command <- getLine
    case command of
        "travel" -> do
            putStrLn "Enter destination (AncientEgypt/Renaissance/FutureCity):"
            destinationStr <- getLine
            case reads destinationStr of
                [(destination, "")] -> gameLoop $ timeTravel destination state
                _ -> putStrLn "Invalid destination" >> gameLoop state
        "pickup" -> do
            putStrLn "Enter item name:"
            itemName <- getLine
            let newItem = Item itemName "A mysterious item"
            gameLoop $ addItem newItem state
        "use" -> putStrLn "Item usage not implemented yet" >> gameLoop state
        "quit" -> putStrLn "Thanks for playing!"
        _ -> putStrLn "Unknown command" >> gameLoop state