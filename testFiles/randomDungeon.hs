import System.Random

data Room = NRoom Room Room Room Room | ERoom Room Room Room Room | SRoom Room Room Room Room | WRoom Room Room Room Room | Empty | Fin | NTBA | ETBA | STBA | WTBA | TBD deriving (Show, Eq, Ord)

-- BSP: [((0,0), NRoom NTBA ETBA Empty WTBA)]

addD xs = foldl addDe [] (foldl addDn [] xs)

addDn acc ((x, y), NRoom n e s w) = (if n == NTBA then ((x, y+1), NRoom NTBA ETBA Fin WTBA) else ((x, y+1), TBD)) : ((x, y), NRoom Fin e s w) : acc
addDn acc ((x, y), ERoom n e s w) = (if n == NTBA then ((x, y+1), NRoom NTBA ETBA Fin WTBA) else ((x, y+1), TBD)) : ((x, y), ERoom Fin e s w) : acc
addDn acc ((x, y), SRoom n e s w) = ((x, y), SRoom Fin e s w) : acc
addDn acc ((x, y), WRoom n e s w) = (if n == NTBA then ((x, y+1), NRoom NTBA ETBA Fin WTBA) else ((x, y+1), TBD)) : ((x, y), ERoom Fin e s w) : acc

addDe acc ((x, y), NRoom n e s w) = (if e == ETBA then ((x+1, y), ERoom NTBA ETBA STBA Fin) else ((x+1, y), TBD)) : ((x, y), NRoom n Fin s w) : acc
addDe acc ((x, y), ERoom n e s w) = (if e == ETBA then ((x+1, y), ERoom NTBA ETBA STBA Fin) else ((x+1, y), TBD)) : ((x, y), ERoom e Fin s w) : acc
addDe acc ((x, y), SRoom n e s w) = (if e == ETBA then ((x+1, y), ERoom NTBA ETBA STBA Fin) else ((x+1, y), TBD)) : ((x, y), SRoom n Fin s w) : acc
addDe acc ((x, y), WRoom n e s w) = ((x, y), WRoom n Fin s w) : acc





--[(  (x, y), CRoom [N,E,S,W]   )]
