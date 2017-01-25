-- A Cell has an X and Y coordinate, and two Bools representing whether that Cell is a wall or a goal.
type Cell = (Int, Int, Bool, Bool)
type State = Cell

--A Maze is a list of all cells in the Maze, arranged with the first row, followed by the second, and so on.
--Mazes begin at (0,0), then extend to the right until (n,0), then go up to (0,1), following the next row to (1,n)
type Maze = [Cell]

-- The initial state is usually at the origin, and must be a non-wall and non-goal to make the maze interesting.
initial :: State
initial = (0, 0, False, False)

operator:: State -> Maze-> [State]
operator cur maze = sl

main = print initial
