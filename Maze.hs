-- A Cell has an X and Y coordinate.
type Cell = (Int, Int)

--A State is a list of cells.
type State = [Cell]

--Maze is described below.
--Bounds is the total size of the maze in (X,Y)
bounds :: (Int, Int)
bounds = (5,5)

--Walls are the cells within the maze that cannot be passed.
walls :: [Cell]
walls = [(1,4), (4,2), (2,2), (3,3)]
--These commented out walls create an unsolveable Maze
--walls = [(1,4), (4,2), (2,2), (3,3), (2,4)]

--Goal is the goal cell in the maze.
goal :: Cell
goal =  (4,4)

-- The initial state is usually at the origin, and must be a non-wall and non-goal to make the maze interesting.
initial :: State
initial = [(0, 0)]

loser :: State
loser = []

goalTest :: State -> Bool
goalTest (x:xs) = x == goal

next :: State -> [Cell]
next (c:cs) = [(x,y)| (x,y) <-[(fst c + 1, snd c), (fst c - 1, snd c), (fst c, snd c + 1), (fst c, snd c - 1)],
               notElem (x, y) walls && (x >= 0 && y >= 0) && (x < fst bounds && y < snd bounds)]

operator:: State -> [State]
operator (xs) = [(n:xs) | n <- next xs, notElem n xs]

search :: State
search = treeSearch [initial] operator

treeSearch :: [State] -> (State -> [State]) -> State
treeSearch [] op = loser
treeSearch (x:xs) op
 | goalTest x  = x
 | otherwise   = treeSearch (newstates ++ xs) op
  where newstates = op x
