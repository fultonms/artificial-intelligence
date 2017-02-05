--A Tile has four numbers on it, in order (TOP, BOT, LEFT, RIGHT)
type Tile = (Int, Int, Int, Int)

nullTile :: Tile
nullTile = (0,0,0,0)

tiles = [(1,3,2,4), (1,3,4,2), (3,1,2,4), (3,1,4,2), (1,2,4,3)]

--A Grid is composed of a list of list of tiles.  The inner lists are rows.
type Grid = [[Tile]]

bounds :: (Int, Int)
bounds = (3,3)

type State = Grid

initial :: State
initial =  [[(0,0,0,0), (0,0,0,0), (1,2,3,4)], 
            [(0,0,0,0), (0,0,0,0), (1,2,3,4)], 
            [(1,2,3,4), (1,2,3,4), (1,2,3,4)]]

loser :: State
loser = []

testRow :: [Tile] -> Bool
testRow [] = True
testRow [x] = True
testRow (x:xs) = let (_,_,_, r) = x
                     (_,_,l,_ ) = (head xs) 
                 in ((r == l) || ((r == 0) || (l == 0))) && testRow(xs)

testRows :: [[Tile]] -> Bool
testRows [] = True
testRows (x:xs) = testRow(x) && testRows(xs)

testCol :: [Tile] -> Bool
testCol [] = True
testCol [x] = True
testCol (x:xs) = let (_,b,_,_) = x
                     (t,_,_,_ ) = (head xs) 
                 in ((t == b) || ((t == 0) || (b == 0))) && testRow(xs)

testCols :: [[Tile]] -> Bool
testCols [] = True
testCols (x:xs) = testCol(x) && testCols(xs)

goalTest :: State -> Bool
goalTest s = testRows(s) && (testCols [[x | x <- map (!! col) s ] | col <- [0..fst bounds]])

operator :: State -> [State]
operator s = [s] 
