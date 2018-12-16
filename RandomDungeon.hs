module RandomDungeon (Room (CRoom, NRoom), generate, printDungeon, dungeonLoop) where
import System.Random
import Data.Char
import qualified Texts as Texts

data Dir        = N | E | S | W deriving (Show, Eq, Ord, Read)
data Room       = CRoom [Dir] Dir | NRoom [Dir] deriving (Show, Eq, Read)

type Coords     = (Integer, Integer)
type RandomInts = [Int]
type RandomInt  = Int
type Dungeon    = [(Coords, Room)]

-- ### Main FUnctions ###

dungeonLoop ((msg, player), (dungeon, goal)) = do
    --printDungeon dungeon                         -- <- Debugging
    --putStrLn $ "P: " ++ (show player)            -- <- Debugging
    --putStrLn $ "G: " ++ (show goal)              -- <- Debugging
    putStrLn $ msg
    if player == goal
        then putStrLn Texts.foundDeadBody
        else return ()
    putStr $ "You can go:" ++ (foldl (\acc x -> acc ++ " " ++ (show x)) "" (isort $ getDirections dungeon player)) ++ "\n\n> "
    input <- (getLine >>= (\x -> return (map toLower . unwords $ words x)))
    let newState = calcInput input dungeon goal player
    if snd (fst newState) == (0, -1)
        then putStrLn "Level 1 - Eastern Floor\nYou escaped the cave and are back at where you started."
        else dungeonLoop newState

calcInput input dungeon goal player
    | elem input ["n", "go north"] = if elem N (getDirections dungeon player) then (("Gone North.", (fst player, snd player + 1)), (dungeon, goal)) else (("There is a wall.", player), (dungeon, goal))
    | elem input ["e", "go east"]  = if elem E (getDirections dungeon player) then (("Gone East.",  (fst player + 1, snd player)), (dungeon, goal)) else (("There is a wall.", player), (dungeon, goal))
    | elem input ["s", "go south"] = if elem S (getDirections dungeon player) then (("Gone South.", (fst player, snd player - 1)), (dungeon, goal)) else (("There is a wall.", player), (dungeon, goal))
    | elem input ["w", "go west"]  = if elem W (getDirections dungeon player) then (("Gone West.",  (fst player - 1, snd player)), (dungeon, goal)) else (("There is a wall.", player), (dungeon, goal))
    | otherwise                    = (("Don't think of doing something different when being in this cave.", player), (dungeon, goal))

generate = do
    rg <- newStdGen
    let rs = take 200 $ randoms rg :: [Int]
    let dungeon = genDungeon startList (14:rs) -- Die 14 ist fuer die Mindestgroesse
    return (dungeon, maxCoord dungeon)

genDungeon :: [(Coords, Room)] -> RandomInts -> [(Coords, Room)] -- gen=generate
genDungeon _ [] = error "Dafuq"
genDungeon l (r:rs) = let cRooms (_, room) = case room of NRoom _ -> False
                                                          CRoom _ _ -> True
    in if null (filter cRooms l) then l else genDungeon (genLayer l r) rs

genLayer :: [(Coords, Room)] -> RandomInt -> [(Coords, Room)]
genLayer xs r = sfoldl genRooms [] xs r

genRooms acc ((x,y), CRoom direcs origin) r xs =
    ((x,y), NRoom (origin:(foldl corridorDirs [] newRooms))) : (newRooms ++ acc)
    where newRooms =
              (if elem N direcs && (lookup (x,y+1) (xs++acc)) == Nothing && y+1 < (boundy + 1)
                   then [((x,y+1), CRoom (randDirs S r) S)] else []) ++
              (if elem E direcs && (lookup (x+1,y) (xs++acc)) == Nothing && x+1 < (boundx + 1)
                   then [((x+1,y), CRoom (randDirs W r) W)] else []) ++
              (if elem S direcs && (lookup (x,y-1) (xs++acc)) == Nothing && y-1 > (-1)
                   then [((x,y-1), CRoom (randDirs N r) N)] else []) ++
              (if elem W direcs && (lookup (x-1,y) (xs++acc)) == Nothing && x-1 > (-(boundx + 1))
                   then [((x-1,y), CRoom (randDirs E r) E)] else [])
          corridorDirs acc ((x,y), CRoom _ N) = S : acc
          corridorDirs acc ((x,y), CRoom _ E) = W : acc
          corridorDirs acc ((x,y), CRoom _ S) = N : acc
          corridorDirs acc ((x,y), CRoom _ W) = E : acc
          corridorDirs acc _ = acc
genRooms acc x r xs = x : acc

maxCoord dungeon = foldl (\(oldx, oldy) ((newx, newy), _) -> if newy > oldy then (newx, newy) else (if newy == oldy then (if abs newx > abs oldx then (newx, newy) else (oldx, oldy)) else (oldx, oldy))) (0,0) dungeon

-- ### Wahrscheinlichkeiten und Grenzwerte ###

randDirs dir r = filter (/=dir) newDirs
    where newDirs
              | mod r 19 ==  0 = []
              | mod r 19 ==  1 = [N]
              | mod r 19 ==  2 = [E]
              | mod r 19 ==  3 = [S]
              | mod r 19 ==  4 = [W]
              | mod r 19 ==  5 = [N,E]
              | mod r 19 ==  6 = [N,S]
              | mod r 19 ==  7 = [N,W]
              | mod r 19 ==  8 = [E,S]
              | mod r 19 ==  9 = [E,W]
              | mod r 19 == 10 = [S,W]
              | mod r 19 == 11 = [N,E,S]
              | mod r 19 == 12 = [N,E,W]
              | mod r 19 == 13 = [N,S,W]
              | mod r 19 == 14 = [N,E,S,W]
              | otherwise      = [E,S,W]

boundx = 4--4
boundy = 6--6

-- ### Help Functions ###

getDirections dungeon coords = (\(Just (NRoom d)) -> d) $ lookup coords dungeon

isort xs = foldr (\a acc -> insert a acc) [] xs
    where insert a [] = [a]
          insert a (b:as)
              | a <= b    = a:b:as
              | otherwise = b:(insert a as)

sfoldl _ acc [] _ = acc -- sfoldl = special foldl
sfoldl f acc (x:xs) r = -- Zum Mitnehmen einer weiteren Zahl
    sfoldl f (f acc x r xs) xs (r - (div r ((if div r 9 == 0 then 2 else div r 9))))

printDungeon xs = do
    mapM_ putStr [unlines $ foldr comb ["", "", ""] (map printr [lookup (x, y) xs | x <- [-(boundy-1)..boundy-1]]) | y <- [boundy,boundy-1..0]]

comb [a,b,c] [d,e,f] = [a++d,b++e,c++f]

printr room = if room /= Nothing then [h ++ (isF N room) ++ h, (isF W room) ++ s ++ (isF E room), h ++ (isF S room) ++ h] else [qq ++ qq ++ qq, qq ++ qq ++ qq, qq ++ qq ++ qq]
    where (h, s, qq) = ("# ", "  ", "  ")

isF r room = if elem r (directions room) then "  " else "# "
    where directions (Just (NRoom dirs)) = dirs

-- ### Initial Values ###

startList = [((0,0), CRoom [N,E,W] S)]
