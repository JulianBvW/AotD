type GameState = (String, Int)

main = gameLoop ("Woods", 0)

gameLoop (a, b) = do
    putStrLn "hello, world"
    name <- getLine
    putStr "Where do you want to go? "
    newArea <- getLine
    putStrLn $ "hi, " ++ name ++ ". You are at " ++ newArea ++ " at Move " ++ show b ++ "."
    gameLoop (newArea, b+1)
