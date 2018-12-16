module SaveGame where
import Control.Exception
import System.IO
import System.Directory

main = catch mainD handler

mainD = do
    putStrLn "1: New, 2: Load"
    input <- getLine
    gS <- (if input == "1" then return "(True, 0)" else do
        putStrLn "What?"
        input <- getLine
        readFile input)
    let (b, i) = read gS :: (Bool, Int)
    gameLoop (b, i)

handler :: IOError -> IO ()
handler e = putStrLn "Whoops, had some trouble!"

gameLoop (b, i) = do
    putStrLn $ show (b, i)
    putStrLn "1: Add, 2: Save"
    input <- getLine
    if input == "1"
        then gameLoop (b, i + 1)
        else do
            putStrLn "Where?"
            input <- getLine
            writeFile input (show (b, i))
