module AotD where
import Control.Exception
import Control.Monad
import System.IO
import System.Directory
import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Intro as Intro
import qualified Texts as Texts
import qualified Logo as Logo
import qualified Menu as Menu
import qualified Terminal as Terminal
import qualified RandomDungeon as Dungeon

data Direction   = North | East | South | West deriving (Show, Eq, Ord, Read)
data Location    = Empty | Floor0 | ElevatorRoom0 | ChiefsRoom | MachineRoom | SnakeFarm | TerminalRoom | Laboratory | PanoramaRoomW | PreperationRoom | AirlockRoom | ElevatorRoom1 | PanoramaRoomE | Floor1N | Floor1S | IncubationRoom | StockRoom | Floor1E | ElevatorRoom2 | TransmissionRoom | Cave deriving (Show, Eq, Ord, Read, Enum)
data Item        = ChiefsID | LFCM | UCU | VoiceMSG deriving (Show, Eq, Ord, Read, Enum)
data Action      = ElevUp Location | ElevDown Location | JumpDown Location Bool | Disc deriving (Show, Eq, Ord, Read)
data RoomData    = RoomData {directions :: Map.Map Direction DoorStatus, items :: [Item], actions :: [Action]} deriving (Show, Eq, Ord, Read)
data PlayerData  = PlayerData {inventory :: [Item], health :: Int, maxHealth :: Int, movesTilWaterDeath :: Int, movesTilReactorDeath :: Int, getName :: String, gameOver :: Int} deriving (Show, Eq, Ord, Read)

type DoorStatus  = (Location, Bool)
type MapState    = Map.Map Location RoomData
type PlayerState = (PlayerData, Location)
type GameState   = (String, (MapState, PlayerState))
type Coords      = (Integer, Integer)

-- ### Main Loops ###

main = do
    choice <- Menu.loop                      -- Asks user: New Game or Load Game
    randomDungeon <- Dungeon.generate
    game <- (if choice == "2" 
                 then do                     -- User wants to load a gamefile
                     catch loadGame loadHandler
                 else do                     -- User wants to start a new game
                     name <- Intro.playIntro
                     return $ show ((Texts.wakeUp, (initialmS, initialpS name)), randomDungeon))
    let (gameState, dungeon) = read game :: (GameState, ([(Coords, Dungeon.Room)], Coords))
    Logo.printLogo
    gameLoop gameState dungeon

gameLoop (msg, (mapState, playerState)) dungeon = do
    putStrLn msg
    if msg == Texts.enteringCave
        then Dungeon.dungeonLoop (("***You cannot save. You cannot quit. You can only move.", (0, 0)), dungeon)
        else return ()
    if msg == "Accessed terminal."
        then Terminal.terminalLoop (map toLower . getName $ fst playerState) ""
        else return ()
    mapState' <- testCode mapState msg
    putStr "\n> "
    input <- (getLine >>= (\x -> return (map toLower . unwords $ words x)))
    let newGameState = newState input (mapState', playerState)
    testEnvironmentalChars input newGameState dungeon msg

-- ### Interaction ###

testEnvironmentalChars input gS dungeon oldmsg
    | (gameOver . fst . snd $ snd gS) == 1 = endGame Texts.gameOverSnake
    | (gameOver . fst . snd $ snd gS) == 2 = endGame Texts.gameWon
    | quitChars input = do
        putStrLn "Do you want to save before you quit? y/n"
        saveQ <- getLine
        when (saveQ == "y") (catch (saveGame (("\n\n\n" ++ oldmsg, snd gS), dungeon)) (saveHandler (("\n\n\n" ++ oldmsg, snd gS), dungeon)))
        endGame "UserQuit"
    | saveChars input = do
        catch (saveGame (("\n\n\n" ++ oldmsg, snd gS), dungeon)) (saveHandler (("\n\n\n" ++ oldmsg, snd gS), dungeon))
        gameLoop ((fst gS) ++ oldmsg, snd gS) dungeon
    | otherwise = gameLoop gS dungeon

newState :: String -> (MapState, PlayerState) -> GameState
newState input (mS, pS)
    | saveChars input = ("Saved.\n\n", (mS, pS))
    | moveChars input = move input  (mS, pS)
    | elevChars input = elev input  (mS, pS)
    | lookChars input = lookAround  (mS, pS)
    | helpChars input = printHelp   (mS, pS)
    | examChars input = examine     (mS, pS)
    | takeChars input = takeItem    (unwords . tail $ words input) (mS, pS)
    | dropChars input = dropItem    (unwords . tail $ words input) (mS, pS)
    | pushChars input = push        (unwords . tail $ words input) (mS, pS)
    | inspChars input = useItem     (unwords . tail $ words input) (mS, pS)
    | openChars input = openLocker  (unwords . tail $ words input) (mS, pS)
    | putChars  input = putIn       (unwords . tail $ words input) (mS, pS)
    | useChars  input = useTerminal (unwords . tail $ words input) (mS, pS)
    | null input      = ("You wait. Nothing happened.", (mS, pS))
    | otherwise       = ("I don't understand.", (mS, pS))

printHelp :: (MapState, PlayerState) -> GameState
printHelp (mS, pS) = (Texts.help, (mS, pS))

move :: String -> (MapState, PlayerState) -> GameState
move dirInput (mS, pS)
    | elem dirInput ["e", "go east"] && snd pS == Floor1E = (Texts.enteringCave, (mS, pS))
    | elem dirInput ["s", "go south"] && snd pS == ElevatorRoom2 && (snd $ getDoorStatus mS ElevatorRoom2 South) == False = ("Input security door code:", (mS, pS))
    | elem dirInput ["n", "go north"] = move' North (mS, pS)
    | elem dirInput ["e", "go east"]  = move' East (mS, pS)
    | elem dirInput ["s", "go south"] = move' South (mS, pS)
    | elem dirInput ["w", "go west"]  = move' West (mS, pS)
    where move' dir (mS, pS)
              | newLoc == Empty = ("There is no door.", (mS, pS))
              | snd doorStatus = ((locName newLoc) ++ (if elem Disc (getActions mS newLoc) then [] else "\n" ++ (locText newLoc)) ++ (if null $ getItems mS newLoc then [] else showItems mS newLoc), (addMapAction mS newLoc Disc, (fst pS, newLoc)))
              | not $ elem newLoc [SnakeFarm, TransmissionRoom, MachineRoom] =
                  if elem ChiefsID (getInventory pS)
                      then ("Code accepted.\n" ++ (locName newLoc) ++ (if elem Disc (getActions mS newLoc) then [] else "\n" ++ (locText newLoc)) ++ (if null $ getItems mS newLoc then [] else showItems mS newLoc), (addMapAction (changeDoorState (changeDoorState mS loc dir) newLoc (opDir dir)) newLoc Disc, (fst pS, newLoc)))
                      else ("Access denied.", (mS, pS))
              | otherwise = ("The door is blocked.", (mS, pS))
              where doorStatus = getDoorStatus mS loc dir
                    newLoc     = fst doorStatus
                    loc        = snd pS

elev :: String -> (MapState, PlayerState) -> GameState
elev dir (mS, pS)
    | elem dir ["u", "go up"]              = if null $ filterUp
                                                then ("You cannot go that way.", (mS, pS))
                                                else ((locName newLocU) ++ (if elem Disc (getActions mS newLocU) then [] else "\n" ++ (locText newLocU)) ++ (if null $ getItems mS newLocU then [] else showItems mS newLocU), (addMapAction mS newLocU Disc, (fst pS, newLocU)))
    | elem dir ["d", "go down"]            = if null $ filterDown
                                                then ("You cannot go that way.", (mS, pS))
                                                else ((locName newLocD) ++ (if elem Disc (getActions mS newLocD) then [] else "\n" ++ (locText newLocD)) ++ (if null $ getItems mS newLocD then [] else showItems mS newLocD), (addMapAction mS newLocD Disc, (fst pS, newLocD)))
    | elem dir ["j", "jump", "jump down"] = if null $ filterJump
                                                then ("There is nothing to jump into.", (mS, pS))
                                                else ((locName newLocJ) ++ (if elem Disc (getActions mS newLocJ) then [] else "\n" ++ (locText newLocJ)) ++ (if null $ getItems mS newLocJ then [] else showItems mS newLocJ), (addMapAction mS newLocJ Disc, (newpS, newLocJ)))
    | otherwise = ("error", (mS, pS))
    where filterUp   = filter (\x -> case x of ElevUp _ -> True; _ -> False) (getActions mS (snd pS))
          filterDown = filter (\x -> case x of ElevDown _ -> True; _ -> False) (getActions mS (snd pS))
          filterJump = filter (\x -> case x of JumpDown _ True -> True; _ -> False) (getActions mS (snd pS))
          newLocU = (\(ElevUp loc) -> loc) $ head filterUp
          newLocD = (\(ElevDown loc) -> loc) $ head filterDown
          newLocJ = (\(JumpDown loc _) -> loc) $ head filterJump
          newpS   = if newLocJ == SnakeFarm then PlayerData {inventory = getInventory pS, health = health $ fst pS, maxHealth = maxHealth $ fst pS, movesTilWaterDeath = movesTilWaterDeath $ fst pS, movesTilReactorDeath = movesTilReactorDeath $ fst pS, getName = getName $ fst pS, gameOver = 1} else fst pS

lookAround :: (MapState, PlayerState) -> GameState
lookAround (mS, pS) = ((locText loc) ++ (if null $ getItems mS loc then [] else showItems mS loc), (mS, pS))
    where loc = snd pS

examine :: (MapState, PlayerState) -> GameState
examine (mS, pS) = ("You are Specialist " ++ (getName $ fst pS) ++ "." ++ (if null $ getInventory pS then [] else showInventory pS), (mS, pS))

takeItem :: String -> (MapState, PlayerState) -> GameState
takeItem itemInput (mS, pS)
    | elem itemInput ["chiefs id", "id"]                         = takeItem' ChiefsID (mS, pS)
    | elem itemInput ["long field communication module", "lfcm"] = takeItem' LFCM (mS, pS)
    | elem itemInput ["universal compression unit", "ucu"]       = takeItem' UCU (mS, pS)
    | elem itemInput ["voice message", "message"]                = takeItem' VoiceMSG (mS, pS)
    | itemInput == ""                                            = ("Take what?", (mS, pS))
    | otherwise                                                  = ("There is no " ++ itemInput ++ ".", (mS, pS))
    where takeItem' item (mS, pS)
              | elem item (getItems mS loc) = ("Took " ++ (itemName item) ++ ".", (removeMapItem mS loc item, addPlayerItem pS item))
              | otherwise = ("The is no " ++ (itemName item) ++ " in this room.", (mS, pS))
              where loc = snd pS

dropItem :: String -> (MapState, PlayerState) -> GameState
dropItem itemInput (mS, pS)
    | elem itemInput ["chiefs id", "id"]                         = dropItem' ChiefsID (mS, pS)
    | elem itemInput ["long field communication module", "lfcm"] = dropItem' LFCM (mS, pS)
    | elem itemInput ["universal compression unit", "ucu"]       = dropItem' UCU (mS, pS)
    | elem itemInput ["voice message", "message"]                = dropItem' VoiceMSG (mS, pS)
    | itemInput == ""                                            = ("Drop what?", (mS, pS))
    | otherwise                                                  = ("You have no " ++ itemInput ++ ".", (mS, pS))
    where dropItem' item (mS, pS)
              | elem item (getInventory pS) = ("Dropped " ++ (itemName item) ++ ".", (addMapItem mS loc item, removePlayerItem pS item))
              | otherwise = ("The is no " ++ (itemName item) ++ " in your inventory.", (mS, pS))
              where loc = snd pS

push :: String -> (MapState, PlayerState) -> GameState
push input (mS, pS)
    | input == ""      = ("Push what?", (mS, pS))
    | input /= "shelf" = ("That would not improve your situation.", (mS, pS))
    | otherwise        = push' (mS, pS)
    where push' (mS, pS)
              | loc == PanoramaRoomE && elem (JumpDown ChiefsRoom False) (getActions mS PanoramaRoomE) = (Texts.pushedShelf, (addMapAction (removeMapAction mS PanoramaRoomE (JumpDown ChiefsRoom False)) PanoramaRoomE (JumpDown ChiefsRoom True), pS))
              | loc == PanoramaRoomE = ("That would not improve your situation.", (mS, pS))
              | otherwise            = ("There is no shelf.", (mS, pS))
              where loc = snd pS

useItem :: String -> (MapState, PlayerState) -> GameState
useItem itemInput gS
    | itemInput == ""                                            = ("Use what?", gS)
    | elem itemInput ["voice message", "message"]                = (itemText VoiceMSG, gS)
    | elem itemInput ["usu", "universal compression unit"]       = (itemText UCU, gS)
    | elem itemInput ["lfcm", "long field communication module"] = (itemText LFCM, gS)
    | elem itemInput ["chiefs id", "id"]                         = (itemText ChiefsID, gS)
    | otherwise                                                  = ("There is no " ++ itemInput ++ " to use.", gS)

openLocker :: String -> (MapState, PlayerState) -> GameState
openLocker lockernr (mS, pS) = if snd pS == StockRoom then openLocker' lockernr else ("There is no " ++ lockernr ++ " to open.", (mS, pS))
    where openLocker' lockernr
              | elem lockernr ["", "locker"] = ("Which locker do you want to open?", (mS, pS))
              | elem lockernr ["first locker", "locker 1"]  = ("Input locker code (lockernr. I):", (mS, pS))
              | elem lockernr ["second locker", "locker 2"] = ("Input locker code (lockernr. II):", (mS, pS))
              | elem lockernr ["third locker", "locker 3"]  = ("Input locker code (lockernr. III):", (mS, pS))
              | elem lockernr ["fourth locker", "locker 4"] = ("Input locker code (lockernr. IV):", (mS, pS))
              | otherwise                                   = ("What do you want?", (mS, pS))

putIn :: String -> (MapState, PlayerState) -> GameState
putIn input (mS, pS)
    | elem input ["lfcm in transmission tower", "lfcm in tower", "long field communication module in transmission tower", "long field communication module in tower"] =
        if snd pS == TransmissionRoom
            then (if elem LFCM (getInventory pS)
                      then (Texts.gameWon, (mS, (PlayerData {inventory = getInventory pS, health = health $ fst pS, maxHealth = maxHealth $ fst pS, movesTilWaterDeath = movesTilWaterDeath $ fst pS, movesTilReactorDeath = movesTilReactorDeath $ fst pS, getName = getName $ fst pS, gameOver = 2}, snd pS)))
                      else ("You have no Long Field Communication Module in your inventory.", (mS, pS)))
            else ("You aren't in the rigth room.", (mS, pS))
    | otherwise = ("What are you trying to accomplish with that?", (mS, pS))

useTerminal :: String -> (MapState, PlayerState) -> GameState
useTerminal input (mS, pS) = if snd pS == TerminalRoom then useTerminal' input else ("There is no " ++ input ++ " to use.", (mS, pS))
    where useTerminal' input
              | elem input ["", "locker"] = ("Use what?", (mS, pS))
              | elem input ["terminal"]   = ("Accessed terminal.", (mS, pS))
              | otherwise                 = ("I don't understand.", (mS, pS))

testCode mS "Input locker code (lockernr. I):" = do
    putStr "\n>>> "
    input <- getLine
    putStrLn "Wrong code."
    return mS
testCode mS "Input locker code (lockernr. II):" = do
    putStr "\n>>> "
    input <- getLine
    putStrLn "Wrong code."
    return mS
testCode mS "Input locker code (lockernr. III):" = do
    putStr "\n>>> "
    input <- (getLine >>= (\x -> return (unwords $ words x)))
    if input == "4761"
        then do
            putStrLn Texts.rigthLockerCode
            return (addMapItem mS StockRoom LFCM)
        else do
            putStrLn "Wrong code."
            return mS
testCode mS "Input locker code (lockernr. IV):" = do
    putStr "\n>>> "
    input <- getLine
    putStrLn "Wrong code."
    return mS
testCode mS "Input security door code:" = do
    putStr "\n>>> "
    input <- (getLine >>= (\x -> return (unwords $ words x)))
    if input == "4546B"
        then do
            putStrLn Texts.tRoomDoorOpen
            return (changeDoorState (changeDoorState mS TransmissionRoom North) ElevatorRoom2 South)
        else do
            putStrLn "Wrong code."
            return mS
testCode mS _ = return mS

loadGame = do
    putStr "Save file name: "
    filePath <- getLine
    readFile filePath

saveGame game = do
    putStr "Save File: "
    filePath <- (getLine >>= (\x -> return (map toLower . unwords $ words x)))
    writeFile filePath (show game)

-- ### Help Functions ###

opDir :: Direction -> Direction -- opposite Direction
opDir dir
    | dir == North = South
    | dir == East  = West
    | dir == South = North
    | dir == West  = East

getDoorStatus :: MapState -> Location -> Direction -> DoorStatus
getDoorStatus mS loc dir = (\(Just x) -> x) $ Map.lookup dir $ getDirections mS loc

getDirections :: MapState -> Location -> Map.Map Direction DoorStatus
getDirections mS loc = directions . (\(Just x) -> x) $ Map.lookup loc mS

getActions :: MapState -> Location -> [Action]
getActions mS loc = actions . (\(Just x) -> x) $ Map.lookup loc mS

getItems :: MapState -> Location -> [Item]
getItems mS loc = isort . items . (\(Just x) -> x) $ Map.lookup loc mS

getInventory :: PlayerState -> [Item]
getInventory pS = isort . inventory $ fst pS

changeDoorState :: MapState -> Location -> Direction -> MapState
changeDoorState mS loc dir = Map.insert loc (RoomData {directions = (Map.insert dir (fst doorStatus, True) (directions getRoomData)), items = (items getRoomData), actions = (actions getRoomData)}) mS
    where getRoomData = (\(Just x) -> x) $ Map.lookup loc mS
          doorStatus = getDoorStatus mS loc dir

removeMapItem :: MapState -> Location -> Item -> MapState
removeMapItem mS loc item = Map.insert loc (RoomData {directions = getDirections mS loc, items = filter (/=item) (getItems mS loc), actions = getActions mS loc}) mS

addMapItem :: MapState -> Location -> Item -> MapState
addMapItem mS loc item = Map.insert loc (RoomData {directions = getDirections mS loc, items = nub $ item:(getItems mS loc), actions = getActions mS loc}) mS

removeMapAction :: MapState -> Location -> Action -> MapState
removeMapAction mS loc action = Map.insert loc (RoomData {directions = getDirections mS loc, items = getItems mS loc, actions = filter (/=action) (getActions mS loc)}) mS

addMapAction :: MapState -> Location -> Action -> MapState
addMapAction mS loc action = Map.insert loc (RoomData {directions = getDirections mS loc, items = getItems mS loc, actions = nub $ action:(getActions mS loc)}) mS

removePlayerItem :: PlayerState -> Item -> PlayerState
removePlayerItem pS item = (PlayerData {inventory = filter (/=item) (getInventory pS), health = health $ fst pS, maxHealth = maxHealth $ fst pS, movesTilWaterDeath = movesTilWaterDeath $ fst pS, movesTilReactorDeath = movesTilReactorDeath $ fst pS, getName = getName $ fst pS, gameOver = gameOver $ fst pS}, snd pS)

addPlayerItem :: PlayerState -> Item -> PlayerState
addPlayerItem pS item = (PlayerData {inventory = nub $ item:(getInventory pS), health = health $ fst pS, maxHealth = maxHealth $ fst pS, movesTilWaterDeath = movesTilWaterDeath $ fst pS, movesTilReactorDeath = movesTilReactorDeath $ fst pS, getName = getName $ fst pS, gameOver = gameOver $ fst pS}, snd pS)

showItems :: MapState -> Location -> String
showItems mS loc = foldl (\acc item -> acc ++ "\nThere is one " ++ (itemName item) ++ ".") "\n" (getItems mS loc)

showInventory :: PlayerState -> String
showInventory pS = foldl (\acc item -> acc ++ "\nYou carry one " ++ (itemName item) ++ ".") "\n" (getInventory pS)

locName loc = Texts.locName $ fromEnum loc
locText loc = Texts.locText $ fromEnum loc

itemName item = Texts.itemName $ fromEnum item
itemText item = Texts.itemText $ fromEnum item

isort xs = foldr (\x acc -> insert x acc) [] xs
    where insert x []     = [x]
          insert x (y:xs)
              | x <= y    = x:y:xs
              | otherwise = y:(insert x xs)

loadHandler :: IOError -> IO String
loadHandler error = do
    putStrLn "File does not exist."
    catch loadGame loadHandler

saveHandler :: (GameState, ([(Coords, Dungeon.Room)], Coords)) -> IOError -> IO ()
saveHandler game error = do
    putStrLn "Input file name."
    catch (saveGame game) (saveHandler game)
    putStr "..."

endGame a = putStrLn $ "\n\n" ++ a

-- ### Input Categories ###

saveChars input = elem input [":s", ":save"]
helpChars input = elem input [":?", ":h", ":help"]
quitChars input = elem input [":q", ":l", ":e", ":quit", ":leave", ":exit"]
moveChars input = elem input ["n", "e", "s", "w", "go north", "go east", "go south", "go west"]
elevChars input = elem input ["u", "d", "go up", "go down", "j", "jump", "jump down"]
lookChars input = elem input ["l", "look around"]
examChars input = elem input ["x", "examine me"]
useChars input
    | null input = False
    | otherwise  = elem (head $ words input) ["use"]
takeChars input
    | null input = False
    | otherwise  = elem (head $ words input) ["take"]
dropChars input
    | null input = False
    | otherwise  = elem (head $ words input) ["drop"]
pushChars input
    | null input = False
    | otherwise  = elem (head $ words input) ["push"]
inspChars input
    | null input = False
    | otherwise  = elem (head $ words input) ["inspect"]
openChars input
    | null input = False
    | otherwise  = elem (head $ words input) ["open"]
putChars input
    | null input = False
    | otherwise  = elem (head $ words input) ["put"]

-- ### Initial Values ###

initialmS = Map.fromList -- GameMap
    [(Laboratory,       RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (ElevatorRoom0, False)),   (South, (PanoramaRoomW, False)),    (West, (Empty, False))],           items = [], actions = []}),
     (ElevatorRoom0,    RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (ChiefsRoom, False)),      (South, (Floor0, True)),            (West, (Laboratory, False))],      items = [], actions = [ElevUp ElevatorRoom1]}),
     (ChiefsRoom,       RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (Empty, False)),           (South, (Empty, False)),            (West, (ElevatorRoom0, False))],   items = [ChiefsID], actions = []}),
     (PanoramaRoomW,    RoomData {directions = Map.fromList [(North, (Laboratory, False)),    (East, (Floor0, True)),           (South, (PreperationRoom, True)),   (West, (Empty, False))],           items = [], actions = []}),
     (Floor0,           RoomData {directions = Map.fromList [(North, (ElevatorRoom0, True)),  (East, (MachineRoom, False)),     (South, (TerminalRoom, False)),     (West, (PanoramaRoomW, True))],    items = [VoiceMSG], actions = [Disc]}),
     (MachineRoom,      RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (Empty, False)),           (South, (Empty, False)),            (West, (Floor0, True))],           items = [], actions = []}),
     (PreperationRoom,  RoomData {directions = Map.fromList [(North, (PanoramaRoomW, True)),  (East, (TerminalRoom, False)),    (South, (Empty, False)),            (West, (Empty, False))],           items = [UCU], actions = []}),
     (TerminalRoom,     RoomData {directions = Map.fromList [(North, (Floor0, False)),        (East, (SnakeFarm, False)),       (South, (Empty, False)),            (West, (PreperationRoom, False))], items = [], actions = []}),
     (SnakeFarm,        RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (Empty, False)),           (South, (Empty, False)),            (West, (TerminalRoom, False))],    items = [], actions = []}),
     (ElevatorRoom1,    RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (PanoramaRoomE, True)),    (South, (Floor1N, True)),           (West, (Empty, False))],           items = [], actions = [ElevUp ElevatorRoom2, ElevDown ElevatorRoom0]}),
     (PanoramaRoomE,    RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (Empty, False)),           (South, (Empty, False)),            (West, (ElevatorRoom1, True))],    items = [], actions = [JumpDown ChiefsRoom False]}),
     (Floor1N,          RoomData {directions = Map.fromList [(North, (ElevatorRoom1, True)),  (East, (Empty, False)),           (South, (Floor1S, True)),           (West, (Empty, False))],           items = [], actions = []}),
     (StockRoom,        RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (Floor1S, False)),         (South, (Empty, False)),            (West, (Empty, False))],           items = [], actions = []}),
     (Floor1S,          RoomData {directions = Map.fromList [(North, (Floor1N, True)),        (East, (Floor1E, False)),         (South, (Empty, False)),            (West, (StockRoom, False))],       items = [], actions = []}),
     (Floor1E,          RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (Cave, True)),             (South, (Empty, False)),            (West, (Floor1S, True))],          items = [], actions = [JumpDown SnakeFarm True]}),
     (ElevatorRoom2,    RoomData {directions = Map.fromList [(North, (Empty, False)),         (East, (Empty, False)),           (South, (TransmissionRoom, False)), (West, (Empty, False))],           items = [], actions = [ElevDown ElevatorRoom1]}),
     (TransmissionRoom, RoomData {directions = Map.fromList [(North, (ElevatorRoom2, False)), (East, (Empty, False)),           (South, (Empty, False)),            (West, (Empty, False))],           items = [], actions = []})]
initialpS name = (PlayerData {inventory = [ChiefsID], health = 20, maxHealth = 20, movesTilWaterDeath = -1, movesTilReactorDeath = -1, getName = name, gameOver = 0}, Floor0)






