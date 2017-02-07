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
initial =  [[                      (1,4,3,2)],
            [                      (4,1,2,2)],
            [(1,3,4,2), (1,4,2,4), (1,4,4,3)]]

loser :: State
loser = [[]]

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
                 in (t == b) && testCol(xs) && (t /= 0 && b /= 0)

testCols :: [[Tile]] -> Bool
testCols [] = True
testCols (x:xs) = testCol(x) && testCols(xs)

getColumn :: Int -> [Tile] -> Tile
getColumn col [] = (0,0,0,0)
getColumn col (x:xs)
  | col == 0  = x
  |otherwise  = getColumn (col -1) xs


transformToCol :: [[Tile]] -> [[Tile]]
transformToCol graph = [[ x | x <- (map (getColumn col) (map reverse graph))]| col <- [0..size-1]]

rowsComplete :: [Int] -> Bool
rowsComplete [] = True
rowsComplete (x:xs) = (x == size) && rowsComplete xs

goalTest :: State -> Bool
goalTest s = testRows s && testCols (transformToCol s)  && rowsComplete (map length s)

genState :: Tile -> Int -> State -> State
genState t offset graph
  | offset == size                    = []
  | (length (graph !! offset)) < size = [([t] ++ graph!!offset)] ++ genState t (offset + 1) graph
  | otherwise                         = [(graph!!offset)] ++ genState t (offset + 1) graph

genNextStates :: [Tile] -> State -> [State]
genNextStates [] s = []
genNextStates (t:ts) s = [[x | x <- genState t 0 (reverse s)]] ++ (genNextStates ts s)

operator :: State -> [State]
operator s = [x | x <- genNextStates tiles s]

search :: State
search = treeSearch [initial] operator

treeSearch :: [State] -> (State -> [State]) -> State
treeSearch [] op = loser
treeSearch (x:xs) op
 | goalTest x  = x
 | otherwise   = treeSearch (newstates ++ xs) op
  where newstates = op x
