module GameTest where
import System.IO
import System.Directory
import Data.Char
import qualified Data.Map as Map
import qualified Intro as Intro
import qualified Texts as Texts
import qualified Logo as Logo
import qualified Menu as Menu

data Direction   = North | East | South | West deriving (Show, Eq, Ord, Read)
data Location    = Empty | Floor0 | ElevatorRoom0 | ChiefsRoom | MachineRoom | SnakeFarm | TerminalRoom | Laboratory | PanoramaRoomW | PreperationRoom | AirlockRoom | ElevatorRoom1 | PanoramaRoomE | Floor1N | Floor1S | IncubationRoom | StockRoom | Floor1E | ElevatorRoom2 | TransmissionRoom deriving (Show, Eq, Ord, Read, Enum)
data Item        = ChiefsCode | Item2 deriving (Show, Eq, Ord, Read)
data Action      = ElevUp Location | ElevDown Location | JumpDown Location Bool | Disc deriving (Show, Eq, Ord, Read)
data RoomData    = RoomData {directions :: Map.Map Direction DoorOpen, items :: [Item], actions :: [Action]} deriving (Show, Eq, Ord, Read)
data PlayerData  = PlayerData {inventory :: [Item], health :: Int, maxHealth :: Int, movesTilWaterDeath :: Int, movesTilReactorDeath :: Int, getName :: String, gameOver :: Bool} deriving (Show, Eq, Ord, Read)

type DoorOpen    = (Location, Bool)
type MapState    = Map.Map Location RoomData
type PlayerState = (PlayerData, Location)
type GameState   = (String, (MapState, PlayerState))

-- ### Main Loops ###

main = do
    --Menu.main
    --name <- Intro.playIntro
    let name = "Debugger"
    let gameState = (Texts.wakeUp, (initialmS, initialpS name)) -- Initial GameState
    Logo.printLogo
    gameLoop gameState

gameLoop (msg, (mapState, playerState)) = do
    putStrLn msg
    putStr "\n> "
    input <- (getLine >>= (\x -> return (map toLower . unwords $ words x)))
    let newGameState = newState input (mapState, playerState)
    if gameOver $ fst playerState
        then endGame "GameOver"
        else if quitChars input
                 then endGame "UserQuit"
                 else gameLoop $ newGameState

endGame a = putStrLn $ "\n\n" ++ a

-- ### Interaction ###

newState :: String -> (MapState, PlayerState) -> GameState
newState input (mS, pS)
    | moveChars input = move input (mS, pS)
    | elevChars input = elev input (mS, pS)
    | lookChars input = lookAround (mS, pS)
    | examChars input = examine    (mS, pS)
    | takeChars input = takeItem (unwords . tail $ words input) (mS, pS)
    | dropChars input = dropItem (unwords . tail $ words input) (mS, pS)
    | otherwise       = ("Nothing happened", (mS, pS))

move :: String -> (MapState, PlayerState) -> GameState
move dir (mS, pS)
    | elem dir ["n"] = move' North (mS, pS)
    | elem dir ["e"] = move' East (mS, pS)
    | elem dir ["s"] = move' South (mS, pS)
    | elem dir ["w"] = move' West (mS, pS)
    where move' d (mS, pS)
              | (fst $ newRoom d) == Empty = ("There is no door.", (mS, pS))
              | snd $ newRoom d = (Texts.showR . fromEnum . fst $ newRoom d, (mS, (fst pS, fst $ newRoom d)))
              | not $ elem (fst $ newRoom d) [SnakeFarm, TransmissionRoom, MachineRoom] =
                  if elem ChiefsCode (inventory $ fst pS) then ("Code accepted.\n" ++ (Texts.showR . fromEnum . fst $ newRoom d), (newmS, (fst pS, fst $ newRoom d))) else ("Access denied.", (mS, pS))
              | otherwise = ("The door is blocked.", (mS, pS))
              where newmS = Map.fromList ((Map.toList mS) ++ [(snd pS, RoomData {directions = (Map.fromList ((Map.toList $ directions getRoomData) ++ [(d, (fst $ newRoom d, True))])), items = (items getRoomData), actions = (actions getRoomData)})])
                    getRoomData = (\(Just x) -> x) $ Map.lookup (snd pS) mS
          newRoom d = getRoom (snd pS) mS d

elev :: String -> (MapState, PlayerState) -> GameState
elev dir (mS, pS)
    | elem dir ["u", "go up"]   = if null $ filterUp then ("The lift does not go any further.", (mS, pS)) else (Texts.showR . fromEnum . (\(ElevUp loc) -> loc) $ head filterUp, (mS, (fst pS, (\(ElevUp loc) -> loc) $ head filterUp)))
    | elem dir ["d", "go down"] = if null $ filterDown then ("The lift does not go any further.", (mS, pS)) else (Texts.showR . fromEnum . (\(ElevDown loc) -> loc) $ head filterDown, (mS, (fst pS, (\(ElevDown loc) -> loc) $ head filterDown)))
    | otherwise = ("error", (mS, pS))
    where filterUp   = filter (\x -> case x of ElevUp _ -> True; _ -> False) (getAction (snd pS) mS)
          filterDown = filter (\x -> case x of ElevDown _ -> True; _ -> False) (getAction (snd pS) mS)
          filterJump = filter (\x -> case x of JumpDown _ True -> True; _ -> False) (getAction (snd pS) mS)

lookAround :: (MapState, PlayerState) -> GameState
lookAround (mS, pS) = (Texts.printT . fromEnum $ snd pS, (mS, pS))

examine :: (MapState, PlayerState) -> GameState
examine (mS, pS) = ("You are " ++ (getName $ fst pS), (mS, pS))

takeItem :: String -> (MapState, PlayerState) -> GameState
takeItem item (mS, pS) = ("Took absolutely nothing", (mS, pS))

dropItem :: String -> (MapState, PlayerState) -> GameState
dropItem item (mS, pS) = ("Dropped absolutely nothing", (mS, pS))

-- ### Help Functions WIP ###

getRoom cR mS dir = (\(Just x) -> x) . Map.lookup dir . directions . (\(Just x) -> x) $ Map.lookup cR mS
getAction cR mS = actions . (\(Just x) -> x) $ Map.lookup cR mS 

-- ### Input Categories ###

quitChars input = elem input [":q", ":l", ":e", ":quit", ":leave", ":exit"]
moveChars input = elem input ["n", "e", "s", "w", "go north", "go east", "go south", "go west"]
elevChars input = elem input ["u", "d", "go up", "go down"]
lookChars input = elem input ["l", "look around"]
examChars input = elem input ["x", "examine me"]
takeChars input = elem (head $ words input) ["take"]
dropChars input = elem (head $ words input) ["drop"]

-- ### Initial Values ###

initialmS = Map.fromList
    [(Laboratory,       RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (ElevatorRoom0, False)),   (South, (PanoramaRoomW, False)),    (West, (Empty, False))],           items = [], actions = []}),
     (ElevatorRoom0,    RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (ChiefsRoom, False)),      (South, (Floor0, True)),            (West, (Laboratory, False))],      items = [], actions = [ElevUp ElevatorRoom1]}),
     (ChiefsRoom,       RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (Empty, False)),           (South, (Empty, False)),            (West, (ElevatorRoom0, False))],   items = [], actions = []}),
     (PanoramaRoomW,    RoomData {directions = Map.fromList [(North, (Laboratory, False)),    (East, (Floor0, True)),           (South, (PreperationRoom, True)),   (West, (Empty, False))],           items = [], actions = []}),
     (Floor0,           RoomData {directions = Map.fromList [(North, (ElevatorRoom0, True)),  (East, (MachineRoom, False)),     (South, (TerminalRoom, False)),     (West, (PanoramaRoomW, True))],    items = [], actions = [Disc]}),
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
initialpS name = (PlayerData {inventory = [ChiefsCode], health = 20, maxHealth = 20, movesTilWaterDeath = -1, movesTilReactorDeath = -1, getName = name, gameOver = False}, Floor0)






