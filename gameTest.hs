import System.IO
import System.Directory
import qualified Data.Map as Map

data Direction  = North | East | South | West deriving (Show, Eq, Ord)
data Location   = Empty | Floor0 | ElevatorRoom0 | ChiefsRoom | MachineRoom | SnakeFarm | TerminalRoom | Laboratory | PanoramaRoomW | PreperationRoom | AirlockRoom | ElevatorRoom1 | PanoramaRoomE | Floor1N | Floor1S | IncubationRoom | StockRoom | Floor1E | ElevatorRoom2 | TransmissionRoom deriving (Show, Eq, Ord)
data Item       = Item1 | Item2 deriving (Show, Eq, Ord)
data Action     = Action1 | Action2 deriving (Show, Eq, Ord)
data RoomData   = RoomData {directions :: Map.Map Direction DoorOpen, items :: [Item], actions :: [Action]} deriving (Show, Eq, Ord)
data PlayerData = PlayerData {inventory :: [Item], health :: Int, maxHealth :: Int, movesTilWaterDeath :: Int, movesTilReactorDeath :: Int} deriving (Show, Eq, Ord)

type DoorOpen    = (Location, Bool)
type MapState    = Map.Map Location RoomData
type PlayerState = (PlayerData, Location)
type GameState   = (MapState, PlayerState)

main = do
    clear
    let mapState = Map.fromList [(Laboratory,      RoomData {directions = Map.fromList [(North, (Empty, False)),        (East, (ElevatorRoom0, False)),   (South, (PanoramaRoomW, False)),  (West, (Empty, False))],           items = [], actions = []}),
                                 (ElevatorRoom0,   RoomData {directions = Map.fromList [(North, (Empty, False)),        (East, (ChiefsRoom, False)),      (South, (Floor0, True)),          (West, (Laboratory, False))],      items = [], actions = []}),
                                 (ChiefsRoom,      RoomData {directions = Map.fromList [(North, (Empty, False)),        (East, (Empty, False)),           (South, (Empty, False)),          (West, (ElevatorRoom0, False))],   items = [], actions = []}),
                                 (PanoramaRoomW,   RoomData {directions = Map.fromList [(North, (Laboratory, False)),   (East, (Floor0, True)),           (South, (PreperationRoom, True)), (West, (Empty, False))],           items = [], actions = []}),
                                 (Floor0,          RoomData {directions = Map.fromList [(North, (ElevatorRoom0, True)), (East, (MachineRoom, False)),     (South, (TerminalRoom, False)),   (West, (PanoramaRoomW, True))],    items = [], actions = []}),
                                 (MachineRoom,     RoomData {directions = Map.fromList [(North, (Empty, False)),        (East, (Empty, False)),           (South, (Empty, False)),          (West, (Floor0, True))],           items = [], actions = []}),
                                 (AirlockRoom,     RoomData {directions = Map.fromList [(North, (Empty, False)),        (East, (PreperationRoom, False)), (South, (Empty, False)),          (West, (Empty, False))],           items = [], actions = []}),
                                 (PreperationRoom, RoomData {directions = Map.fromList [(North, (PanoramaRoomW, True)), (East, (TerminalRoom, False)),    (South, (Empty, False)),          (West, (AirlockRoom, False))],     items = [], actions = []}),
                                 (TerminalRoom,    RoomData {directions = Map.fromList [(North, (Floor0, False)),       (East, (SnakeFarm, False)),       (South, (Empty, False)),          (West, (PreperationRoom, False))], items = [], actions = []}),
                                 (SnakeFarm,       RoomData {directions = Map.fromList [(North, (Empty, False)),        (East, (Empty, False)),           (South, (Empty, False)),          (West, (TerminalRoom, False))],    items = [], actions = []})]
    let playerState = (PlayerData {inventory = [], health = 20, maxHealth = 20, movesTilWaterDeath = -1, movesTilReactorDeath = -1}, Floor0)
    printLogoFull
    gameLoop (mapState, playerState)

gameLoop (mapState, playerState) = do
--    showState input (mapState, playerState)
    putStr $ "You are currently in " ++ (show $ snd playerState) ++ "\n\n> "
    input <- getLine
    if quitChars input
        then return ()
        else gameLoop $ newState input (mapState, playerState)

newState :: String -> GameState -> GameState
newState input (mS, pS)
    | moveChars input = move input (mS, pS)
    | otherwise       = (mS, pS)

-- ### Interaktionsfunktionen ###

move :: String -> GameState -> GameState
move dir (mS, pS)
    | elem dir ["n", "go north"] = if snd (getRoom (snd pS) North mS) == False then (mS, pS) else (mS, (fst pS, fst (getRoom (snd pS) North mS)))
    | elem dir ["e", "go east"]  = if snd (getRoom (snd pS) East mS) == False then (mS, pS) else (mS, (fst pS, fst (getRoom (snd pS) East mS)))
    | elem dir ["s", "go south"] = if snd (getRoom (snd pS) South mS) == False then (mS, pS) else (mS, (fst pS, fst (getRoom (snd pS) South mS)))
    | elem dir ["w", "go west"]  = if snd (getRoom (snd pS) West mS) == False then (mS, pS) else (mS, (fst pS, fst (getRoom (snd pS) West mS)))
    | otherwise = (mS, pS)

-- ### Hilfsfunktionen WIP ###

getRoom cR dir mS = (\(Just x) -> x) . Map.lookup dir . directions . (\(Just x) -> x) $ Map.lookup cR mS

clear = putStr "\ESC[2J"

-- ### Eingabetests ###

quitChars input = elem input [":q", ":l", ":e", ":quit", ":leave", ":exit"]
moveChars input = elem input ["n", "e", "s", "w", "go north", "go east", "go south", "go west"]




printLogoShadow = do
    putStrLn "\n\n       \\        |                   |                                 _|    |    |          \n      _ \\    _` | \\ \\ /  -_)    \\    _|  |  |   _| -_) (_-<     _ \\   _|     _|    \\    -_) \n    _/  _\\ \\__,_|  \\_/ \\___| _| _| \\__| \\_,_| _| \\___| ___/   \\___/ _|     \\__| _| _| \\___| \n"
    putStrLn "              (             (        (                                             \n              )\\ )   *   )  )\\ )     )\\ )                                          \n             (()/( ` )  /( (()/(    (()/(  (                 )      (   (    (     \n              /(_)) ( )(_)) /(_))    /(_)) )\\  (    (   (   /((    ))\\  )(   )\\ )  \n                       (_))_|(_(_()) (_))     (_))_ ((_) )\\   )\\  )\\ (_))\\  /((_)(()\\ (()/(  \n                       | |_  |_   _| / __|     |   \\ (_)((_) ((_)((_)_)((_)(_))   ((_) )(_)) \n                       | __|_  | | _ \\__ \\ _   | |) || |(_-</ _|/ _ \\\\ V / / -_) | '_|| || | \n                       |_| (_) |_|(_)|___/(_)  |___/ |_|/__/\\__|\\___/ \\_/  \\___| |_|   \\_, | \n                                                                                       |__/  "
printLogoFull = do
    putStrLn "\n\n         _      _             _                          __   _   _        \n        /_\\  __| |_ _____ _ _| |_ _  _ _ _ ___ ___  ___ / _| | |_| |_  ___ \n       / _ \\/ _` \\ V / -_) ' \\  _| || | '_/ -_|_-< / _ \\  _| |  _| ' \\/ -_)\n      /_/ \\_\\__,_|\\_/\\___|_||_\\__|\\_,_|_| \\___/__/ \\___/_|    \\__|_||_\\___|\n"
    putStrLn "       (             (        (                                             \n       )\\ )   *   )  )\\ )     )\\ )                                          \n      (()/( ` )  /( (()/(    (()/(  (                 )      (   (    (     \n       /(_)) ( )(_)) /(_))    /(_)) )\\  (    (   (   /((    ))\\  )(   )\\ )  \n      (_))_|(_(_()) (_))     (_))_ ((_) )\\   )\\  )\\ (_))\\  /((_)(()\\ (()/(  \n      | |_  |_   _| / __|     |   \\ (_)((_) ((_)((_)_)((_)(_))   ((_) )(_)) \n      | __|_  | | _ \\__ \\ _   | |) || |(_-</ _|/ _ \\\\ V / / -_) | '_|| || | \n      |_| (_) |_|(_)|___/(_)  |___/ |_|/__/\\__|\\___/ \\_/  \\___| |_|   \\_, | \n                                                                      |__/  \n\n\n"
    putStrLn "You find yourself in a slightly lit up room with blood dripping from the cealing."

