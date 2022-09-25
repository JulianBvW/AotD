module Terminal (terminalLoop) where
import qualified Texts as Texts
import Data.Char

terminalLoop name path = do
    putStr $ "\ESC[32m" ++ name ++ "@discovery-root\ESC[37m:\ESC[36m~/data" ++ path ++ "$\ESC[37m "
    input <- (getLine >>= (\x -> return (unwords . words $ map toLower x)))
    if elem input quitChars
        then putStrLn "Left terminal."
        else do
            let (msg, newPath) = inputHandler input path
            putStr msg
            terminalLoop name newPath
            putStr ""

inputHandler input path
    | input == ""    = ("", path)
    | input == "ls"  = (getLS path, path)
    | cdChars  input = getCD (unwords . tail $ words input) path
    | catChars input = (getCAT (unwords . tail $ words input) path, path)
    | naChars  input = ("\nCommand '" ++ input ++ "' not found. Try installing it with \n\nsudo apt-get install " ++ input ++ "\n\n", path)
    | otherwise      = ("bash: " ++ (head $ words input) ++ ": No permission.\n", path)

getLS path = getLS' $ lookup path content
    where getLS' (Just x) = x ++ "\n"
          getLS' Nothing  = "Error\n"

getCD input path
    | input == "."                = ("", path)
    | input == ".." && path == "" = ("bash: cd: ..: Access denied.\n", path)
    | input == ".." && path == "/public"     = ("", "")
    | input == ".." && path == "/public/fts" = ("", "/public")
    | input == ".." && path == "/crash-logs" = ("", "")
    | elem input (words $ getLS path) && elem input folders = ("", path ++ "/" ++ input)
    | input == "private" || input == "logs-old" = ("bash: cd: " ++ input ++ ": Access denied.\n", path)
    | otherwise = ("bash: cd: " ++ input ++ ": Directory not found.\n", path)

getCAT input path
    | elem input (words $ getLS path) && elem input files = Texts.terminalFile input
    | otherwise = "bash: cat: " ++ input ++ ": File not found or corrupted.\n"

folders = ["public", "crash-logs", "fts"]
files   = ["logo.txt", "militarylaunch.tdoc", "militarylaunch-comment17.tdoc", "interview.tdoc", "research3.tdoc", "alsiem-tribute.tdoc", "current-0002.log"]
content = [("", "\ESC[36m public    private    crash-logs \ESC[37m"),
           ("/public", "\ESC[36m fts \ESC[37m      research3.tdoc       alsiem-tribute.tdoc     interview.tdoc"),
           ("/crash-logs", "\ESC[36m logs-old \ESC[37m current-0002.log"),
           ("/public/fts", " logo.txt  militarylaunch.tdoc  militarylaunch-comment17.tdoc")]

cdChars  input = elem (head $ words input) ["cd"]
catChars input = elem (head $ words input) ["cat", "more"]
naChars  input = not $ elem (head $ words input) ["sudo", "apt", "apt-get", "mkdir", "touch", "rm", "rmdir", "mv"]
quitChars = ["leave", "exit", "quit"]

--Public
--    FTS
--        logo.txt
--        militaryLaunch.tdoc
--        militaryLaunch-commant17.tdoc
--    research3.tdoc
--    alsiem-tribute.tdoc
--    interview.tdoc
--private §-> bash: cd: private: Access denied.
--crash-logs
--    current-0002.tdoc
--    logs-old
