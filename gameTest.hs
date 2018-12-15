module GameTest where
import System.IO
import System.Directory
import Data.Char
import qualified Data.Map as Map
import qualified Logo as Logo
import qualified Intro as Intro

data Direction   = North | East | South | West deriving (Show, Eq, Ord, Read)
data Location    = Empty | Floor0 | ElevatorRoom0 | ChiefsRoom | MachineRoom | SnakeFarm | TerminalRoom | Laboratory | PanoramaRoomW | PreperationRoom | AirlockRoom | ElevatorRoom1 | PanoramaRoomE | Floor1N | Floor1S | IncubationRoom | StockRoom | Floor1E | ElevatorRoom2 | TransmissionRoom deriving (Show, Eq, Ord, Read)
data Item        = Item1 | Item2 deriving (Show, Eq, Ord, Read)
data Action      = ElevUp Location | ElevDown Location | JumpDown Location Bool deriving (Show, Eq, Ord, Read)
data RoomData    = RoomData {directions :: Map.Map Direction DoorOpen, items :: [Item], actions :: [Action]} deriving (Show, Eq, Ord, Read)
data PlayerData  = PlayerData {inventory :: [Item], health :: Int, maxHealth :: Int, movesTilWaterDeath :: Int, movesTilReactorDeath :: Int, getName :: String, gameOver :: Bool} deriving (Show, Eq, Ord, Read)

type DoorOpen    = (Location, Bool)
type MapState    = Map.Map Location RoomData
type PlayerState = (PlayerData, Location)
type GameState   = (MapState, PlayerState)

-- ### Main Loops ###

main = do
    name <- Intro.playIntro
    let gameState = (initialmS, initialpS name) -- Initial GameState
    Logo.printLogo
    gameLoop gameState

gameLoop (mapState, playerState) = do
    putStr $ "You are currently in " ++ (show $ snd playerState) ++ "\n\n> "
    input <- (getLine >>= (\x -> return (map toLower . unwords $ words x)))
    let newGameState = newState input (mapState, playerState)
    if gameOver $ fst playerState
        then endGame "GameOver"
        else if quitChars input
                 then endGame "UserQuit"
                 else gameLoop newGameState

endGame a = putStrLn $ "\n\n" ++ a

-- ### Interaction ###

newState :: String -> GameState -> GameState
newState input (mS, pS)
    | moveChars input = move input (mS, pS)
    | elevChars input = elev input (mS, pS)
    | otherwise       = (mS, pS)

move :: String -> GameState -> GameState
move dir (mS, pS)
    | elem dir ["n", "go north"] = if snd (getRoom (snd pS) mS North) == False then (mS, pS) else (mS, (fst pS, fst (getRoom (snd pS) mS North)))
    | elem dir ["e", "go east"]  = if snd (getRoom (snd pS) mS East)  == False then (mS, pS) else (mS, (fst pS, fst (getRoom (snd pS) mS East)))
    | elem dir ["s", "go south"] = if snd (getRoom (snd pS) mS South) == False then (mS, pS) else (mS, (fst pS, fst (getRoom (snd pS) mS South)))
    | elem dir ["w", "go west"]  = if snd (getRoom (snd pS) mS West)  == False then (mS, pS) else (mS, (fst pS, fst (getRoom (snd pS) mS West)))
    | otherwise = (mS, pS)

elev :: String -> GameState -> GameState
elev dir (mS, pS)
    | elem dir ["u", "go up"]   = if null $ filterUp then (mS, pS) else (mS, (fst pS, (\(ElevUp loc) -> loc) $ head filterUp))
    | elem dir ["d", "go down"] = if null $ filterDown then (mS, pS) else (mS, (fst pS, (\(ElevDown loc) -> loc) $ head filterDown))
    | otherwise = (mS, pS)
    where filterUp   = filter (\x -> case x of ElevUp _ -> True; _ -> False) (getAction (snd pS) mS)
          filterDown = filter (\x -> case x of ElevDown _ -> True; _ -> False) (getAction (snd pS) mS)
          filterJump = filter (\x -> case x of JumpDown _ True -> True; _ -> False) (getAction (snd pS) mS)

-- ### Help Functions WIP ###

getRoom cR mS dir = (\(Just x) -> x) . Map.lookup dir . directions . (\(Just x) -> x) $ Map.lookup cR mS
getAction cR mS = actions . (\(Just x) -> x) $ Map.lookup cR mS 

-- ### Input Categories ###

quitChars input = elem input [":q", ":l", ":e", ":quit", ":leave", ":exit"]
moveChars input = elem input ["n", "e", "s", "w", "go north", "go east", "go south", "go west"]
elevChars input = elem input ["u", "d", "go up", "go down"]

-- ### Initial Values ###

initialmS = Map.fromList
    [(Laboratory,       RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (ElevatorRoom0, False)),   (South, (PanoramaRoomW, False)),    (West, (Empty, False))],           items = [], actions = []}),
     (ElevatorRoom0,    RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (ChiefsRoom, False)),      (South, (Floor0, True)),            (West, (Laboratory, False))],      items = [], actions = [ElevUp ElevatorRoom1]}),
     (ChiefsRoom,       RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (Empty, False)),           (South, (Empty, False)),            (West, (ElevatorRoom0, False))],   items = [], actions = []}),
     (PanoramaRoomW,    RoomData {directions = Map.fromList [(North, (Laboratory, False)),    (East, (Floor0, True)),           (South, (PreperationRoom, True)),   (West, (Empty, False))],           items = [], actions = []}),
     (Floor0,           RoomData {directions = Map.fromList [(North, (ElevatorRoom0, True)),  (East, (MachineRoom, False)),     (South, (TerminalRoom, False)),     (West, (PanoramaRoomW, True))],    items = [], actions = []}),
     (MachineRoom,      RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (Empty, False)),           (South, (Empty, False)),            (West, (Floor0, True))],           items = [], actions = []}),
     (PreperationRoom,  RoomData {directions = Map.fromList [(North, (PanoramaRoomW, True)),  (East, (TerminalRoom, False)),    (South, (Empty, False)),            (West, (Empty, False))],           items = [], actions = []}),
     (TerminalRoom,     RoomData {directions = Map.fromList [(North, (Floor0, False)),        (East, (SnakeFarm, False)),       (South, (Empty, False)),            (West, (PreperationRoom, False))], items = [], actions = []}),
     (SnakeFarm,        RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (Empty, False)),           (South, (Empty, False)),            (West, (TerminalRoom, False))],    items = [], actions = []}),
     (ElevatorRoom1,    RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (PanoramaRoomE, True)),    (South, (Floor1N, True)),           (West, (Empty, False))],           items = [], actions = [ElevUp ElevatorRoom2, ElevDown ElevatorRoom0]}),
     (PanoramaRoomE,    RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (Empty, False)),           (South, (Empty, False)),            (West, (ElevatorRoom1, True))],    items = [], actions = [JumpDown ChiefsRoom False]}),
     (Floor1N,          RoomData {directions = Map.fromList [(North, (ElevatorRoom1, True)),  (East, (Empty, False)),           (South, (Floor1S, True)),           (West, (Empty, False))],           items = [], actions = []}),
     (StockRoom,        RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (Floor1S, False)),         (South, (Empty, False)),            (West, (Empty, False))],           items = [], actions = []}),
     (Floor1S,          RoomData {directions = Map.fromList [(North, (Floor1N, True)),        (East, (Floor1E, False)),         (South, (Empty, False)),            (West, (StockRoom, False))],       items = [], actions = []}),
     (Floor1E,          RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (Empty, False)),           (South, (Empty, False)),            (West, (Floor1S, False))],         items = [], actions = [JumpDown SnakeFarm False]}),
     (ElevatorRoom2,    RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (Empty, False)),           (South, (TransmissionRoom, False)), (West, (Empty, False))],           items = [], actions = [ElevDown ElevatorRoom1]}),
     (TransmissionRoom, RoomData {directions = Map.fromList [(North, (ElevatorRoom2, False)), (East, (Empty, False)),           (South, (Empty, False)),            (West, (Empty, False))],           items = [], actions = []})]
initialpS name = (PlayerData {inventory = [], health = 20, maxHealth = 20, movesTilWaterDeath = -1, movesTilReactorDeath = -1, getName = name, gameOver = False}, Floor0)






