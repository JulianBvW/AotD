import System.Random

data Direction = N | E | S | W deriving (Show, Eq, Ord)
data Room = CRoom [Direction] Direction | NRoom [Direction] | TBD deriving (Show, Eq, Ord)

randomZahl = do
    gen <- newStdGen
    return (random gen :: (Int, StdGen))

generateDungeon xs = if null (filter filterCRs xs) then xs else generateDungeon (updateState xs)
filterCRs ((_,_), x) = case x of NRoom _ -> False
                                 CRoom _ _ -> True

updateState xs = foldl f [] xs

f acc ((x,y), CRoom xs d) = filter filterTBD (((x,y), NRoom (d:(foldl megaF [] zwischenDrin))) : (zwischenDrin ++ acc))
    where zwischenDrin = (if elem N xs && (lookup (x,y+1) acc) == Nothing && y+1 < 6 then ((x,y+1), CRoom [E] S) else ((x,y), TBD)) : (if elem E xs && (lookup (x+1,y) acc) == Nothing && x+1 < 6 then ((x+1,y), CRoom [N] W) else ((x,y), TBD)) : (if elem S xs && (lookup (x,y-1) acc) == Nothing && y-1 > (0-1) then ((x,y-1), CRoom [] N) else ((x,y), TBD)) : (if elem W xs && (lookup (x-1,y) acc) == Nothing && x-1 > (0-6) then ((x-1,y), CRoom [] E) else ((x,y), TBD)) : []
f acc x = x : acc

megaF acc ((x,y), CRoom _ N) = S : acc
megaF acc ((x,y), CRoom _ E) = W : acc
megaF acc ((x,y), CRoom _ S) = N : acc
megaF acc ((x,y), CRoom _ W) = E : acc
megaF acc _ = acc

filterTBD ((_,_), TBD) = False
filterTBD ((_,_), _) = True

startList = [((0,0), CRoom [N,E,W] S)]
