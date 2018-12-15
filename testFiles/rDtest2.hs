import System.Random

data Direction  = N | E | S | W deriving (Show, Eq, Ord)
data Room       = CRoom [Direction] Direction
                  | NRoom [Direction] deriving (Show, Eq)
type Coords     = (Integer, Integer)
type RandomInts = [Integer]

main = do
    rg <- newStdGen
    let rs = take 50 $ randoms rg :: [Int]
    let dungeon = genDungeon startList (14:rs) -- Die 14 ist fuer
    printDungeon dungeon                       -- die Mindestgroesse
    return ()

--genDungeon :: [(Coords, Room)] -> RandomInts -> [(Coords, Room)] -- gen=generate
genDungeon _ [] = error "Dafuq"
genDungeon l (r:rs) = let cRooms (_, room) = case room of NRoom _ -> False
                                                          CRoom _ _ -> True
    in if null (filter cRooms l) then l else genDungeon (genLayer l r) rs

--genLayer :: [(Coords, Room)] -> RandomInts -> [(Coords, Room)]
genLayer xs r = sfoldl genRooms [] xs r -- sfoldl = special foldl

genRooms acc ((x,y), CRoom direcs origin) r xs =
    ((x,y), NRoom (origin:(foldl megaF [] newRooms))) : (newRooms ++ acc)
    where newRooms =
              (if elem N direcs && (lookup (x,y+1) (xs++acc)) == Nothing && y+1 < 7
                   then [((x,y+1), CRoom (randDirs S r) S)] else []) ++
              (if elem E direcs && (lookup (x+1,y) (xs++acc)) == Nothing && x+1 < 5
                   then [((x+1,y), CRoom (randDirs W r) W)] else []) ++
              (if elem S direcs && (lookup (x,y-1) (xs++acc)) == Nothing && y-1 > (-1)
                   then [((x,y-1), CRoom (randDirs N r) N)] else []) ++
              (if elem W direcs && (lookup (x-1,y) (xs++acc)) == Nothing && x-1 > (-5)
                   then [((x-1,y), CRoom (randDirs E r) E)] else [])
          megaF acc ((x,y), CRoom _ N) = S : acc
          megaF acc ((x,y), CRoom _ E) = W : acc
          megaF acc ((x,y), CRoom _ S) = N : acc
          megaF acc ((x,y), CRoom _ W) = E : acc
          megaF acc _ = acc
genRooms acc x r xs = x : acc

sfoldl _ acc [] _ = acc
sfoldl f acc (x:xs) r =
    sfoldl f (f acc x r xs) xs (r - (div r ((if div r 9 == 0 then 2 else div r 9))))

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

startList = [((0,0), CRoom [N,E,W] S)]

printDungeon xs = do
    putStr . unlines $ foldr comb ["", "", ""] (map printr [lookup (x, 6) xs | x <- [-4..4]])
    putStr . unlines $ foldr comb ["", "", ""] (map printr [lookup (x, 5) xs | x <- [-4..4]])
    putStr . unlines $ foldr comb ["", "", ""] (map printr [lookup (x, 4) xs | x <- [-4..4]])
    putStr . unlines $ foldr comb ["", "", ""] (map printr [lookup (x, 3) xs | x <- [-4..4]])
    putStr . unlines $ foldr comb ["", "", ""] (map printr [lookup (x, 2) xs | x <- [-4..4]])
    putStr . unlines $ foldr comb ["", "", ""] (map printr [lookup (x, 1) xs | x <- [-4..4]])
    putStr . unlines $ foldr comb ["", "", ""] (map printr [lookup (x, 0) xs | x <- [-4..4]])

comb [a,b,c] [d,e,f] = [a++d,b++e,c++f]

printr room = if room /= Nothing then [h ++ (isF N room) ++ h, (isF W room) ++ s ++ (isF E room), h ++ (isF S room) ++ h] else [qq ++ qq ++ qq, qq ++ qq ++ qq, qq ++ qq ++ qq]
    where (h, s, qq) = ("# ", "  ", "  ")

isF r room = if elem r (directions room) then "  " else "# "
    where directions (Just (NRoom dirs)) = dirs

