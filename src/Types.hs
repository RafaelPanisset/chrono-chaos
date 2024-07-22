module Types where

import Text.Read (readPrec, lexP)
import Text.Read.Lex (Lexeme(Ident))

data TimePeriod = AncientEgypt | Renaissance | FutureCity deriving (Show, Eq)

instance Read TimePeriod where
    readPrec = do
        Ident s <- lexP
        case s of
            "AncientEgypt" -> return AncientEgypt
            "Renaissance" -> return Renaissance
            "FutureCity" -> return FutureCity
            _ -> fail "Invalid TimePeriod"

data Item = Item { itemName :: String, itemDescription :: String } deriving (Show, Eq)

data Anomaly = Anomaly { 
    anomalyDescription :: String,
    requiredItems :: [Item],
    timePeriod :: TimePeriod
} deriving (Show, Eq)

data GameState = GameState {
    currentTime :: TimePeriod,
    inventory :: [Item],
    resolvedAnomalies :: [Anomaly],
    activeAnomalies :: [Anomaly],
    timeJumps :: Int
} deriving (Show)