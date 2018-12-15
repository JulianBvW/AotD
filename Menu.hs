module Menu (main) where

main = do
    putStrLn "\n"
    putStrLn "      _____   _             _                          ___    _   _       "
    putStrLn "     |  _  |_| |_ _ ___ ___| |_ _ _ ___ ___ ___    ___|  _|  | |_| |_ ___ "
    putStrLn "     |     | . | | | -_|   |  _| | |  _| -_|_ -|  | . |  _|  |  _|   | -_|"
    putStrLn "     |__|__|___|\\_/|___|_|_|_| |___|_| |___|___|  |___|_|    |_| |_|_|___|"
    putStrLn ""
    putStrLn "        _____   _____   _____      ____  _                             "
    putStrLn "       |   __| |_   _| |   __|    |    \\|_|___ ___ ___ _ _ ___ ___ _ _ "
    putStrLn "       |   __|_  | |  _|__   |_   |  |  | |_ -|  _| . | | | -_|  _| | |"
    putStrLn "       |__|  |_| |_| |_|_____|_|  |____/|_|___|___|___|\\_/|___|_| |_  |"
    putStrLn "                                                                  |___|\n\n"
    putStrLn "                              [1] New Game\n"
    putStrLn "                              [2] Load Game\n"
    putStrLn "                              [3] Settings\n\n\n\n"
    number <- getLine
    return number
