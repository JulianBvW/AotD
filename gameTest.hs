import System.IO
import System.Directory
import qualified Data.Map as Map

data Direction = North | East | South | West deriving (Show, Eq, Ord)
data Location = L1 | L2 | L3 | L4 | L5 | L6 deriving (Show, Eq)
data Item = Stick | Torch deriving (Show, Eq, Ord)
data RoomData = RoomData {items :: [Item], directions :: Map.Map Direction Location} deriving Show
type MapState = Map.Map Location RoomData

mapState = []

main = do
    putStrLn "<< Start >>"
    gameLoop

gameLoop = do
    input <- getLine
    putStrLn input
    gameLoop

-- ### Hilfsfunktionen WIP ###

getRoom cR dir mS = (\(Just x) -> x) . Map.lookup dir . directions . snd . head $ filter (\(a, _) -> a == cR) mS






printLogoShadow = do
    putStrLn "\n\n                 \\        |                   |                                 _|    |    |          \n                _ \\    _` | \\ \\ /  -_)    \\    _|  |  |   _| -_) (_-<     _ \\   _|     _|    \\    -_) \n              _/  _\\ \\__,_|  \\_/ \\___| _| _| \\__| \\_,_| _| \\___| ___/   \\___/ _|     \\__| _| _| \\___| \n"
    putStrLn "                        (             (        (                                             \n                        )\\ )   *   )  )\\ )     )\\ )                                          \n                       (()/( ` )  /( (()/(    (()/(  (                 )      (   (    (     \n                        /(_)) ( )(_)) /(_))    /(_)) )\\  (    (   (   /((    ))\\  )(   )\\ )  \n                       (_))_|(_(_()) (_))     (_))_ ((_) )\\   )\\  )\\ (_))\\  /((_)(()\\ (()/(  \n                       | |_  |_   _| / __|     |   \\ (_)((_) ((_)((_)_)((_)(_))   ((_) )(_)) \n                       | __|_  | | _ \\__ \\ _   | |) || |(_-</ _|/ _ \\\\ V / / -_) | '_|| || | \n                       |_| (_) |_|(_)|___/(_)  |___/ |_|/__/\\__|\\___/ \\_/  \\___| |_|   \\_, | \n                                                                                       |__/  "
printLogoFull = do
    putStrLn "\n\n                          _      _             _                          __   _   _        \n                         /_\\  __| |_ _____ _ _| |_ _  _ _ _ ___ ___  ___ / _| | |_| |_  ___ \n                        / _ \\/ _` \\ V / -_) ' \\  _| || | '_/ -_|_-< / _ \\  _| |  _| ' \\/ -_)\n                       /_/ \\_\\__,_|\\_/\\___|_||_\\__|\\_,_|_| \\___/__/ \\___/_|    \\__|_||_\\___|\n"
    putStrLn "                        (             (        (                                             \n                        )\\ )   *   )  )\\ )     )\\ )                                          \n                       (()/( ` )  /( (()/(    (()/(  (                 )      (   (    (     \n                        /(_)) ( )(_)) /(_))    /(_)) )\\  (    (   (   /((    ))\\  )(   )\\ )  \n                       (_))_|(_(_()) (_))     (_))_ ((_) )\\   )\\  )\\ (_))\\  /((_)(()\\ (()/(  \n                       | |_  |_   _| / __|     |   \\ (_)((_) ((_)((_)_)((_)(_))   ((_) )(_)) \n                       | __|_  | | _ \\__ \\ _   | |) || |(_-</ _|/ _ \\\\ V / / -_) | '_|| || | \n                       |_| (_) |_|(_)|___/(_)  |___/ |_|/__/\\__|\\___/ \\_/  \\___| |_|   \\_, | \n                                                                                       |__/  "
