--A Tile has four numbers on it, in order (TOP, BOT, LEFT, RIGHT)
type Tile = (Int, Int, Int, Int)

nullTile :: Tile
nullTile = (0,0,0,0)

tiles :: [Tile]
tiles = [(1,3,4,2), (3,3,2,4), (1,4,3,2), (4,1,2,2), (1,1,3,4)]

--A Grid is composed of a list of list of tiles.  The inner lists are rows.
type Grid = [[Tile]]

size :: Int
size = 3

type State = Grid

initial :: State
initial =  [[(1,4,3,2)], 
            [(4,1,2,2)], 
            [(1,3,4,2), (1,4,2,4), (1,4,4,3)]]

loser :: State
loser = []

testRow :: [Tile] -> Bool
testRow [] = True
testRow [x] = True
testRow (x:xs) = let (_,_,_, r) = x
                     (_,_,l,_ ) = (head xs) 
                 in (r == l) && testRow(xs)

testRows :: [[Tile]] -> Bool
testRows [] = True
testRows (x:xs) = testRow(x) && testRows(xs)

testCol :: [Tile] -> Bool
testCol [] = True
testCol [x] = True
testCol (x:xs) = let (_,b,_,_) = x
                     (t,_,_,_ ) = (head xs) 
                 in (t == b) && testCol(xs)

testCols :: [[Tile]] -> Bool
testCols [] = True
testCols (x:xs) = testCol(x) && testCols(xs)

--transformToCol :: [[Tile]] -> [[Tile]]
--transformToCol [] = []
--transformToCol g  = [

rowsComplete :: [Int] -> Bool
rowsComplete [] = True
rowsComplete (x:xs) = (x == size) && rowsComplete xs

goalTest :: State -> Bool
goalTest s = testRows s && testCols (transformToCol (map reverse s))  && rowsComplete (map length s)

--genNext :: [Tile] -> State -> [State]
--genNext [] s = []
--genNext (t:ts) s = [x | x <- s ] ++ (genNext ts s)

--operator :: State -> [State]
--operator s = [x | x <- genNext tiles s]
