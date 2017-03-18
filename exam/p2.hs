import Debug.Trace

type Vertex = Int
type Edge = (Vertex,Vertex)

vertices :: [Vertex]
vertices = [1..15]

edges :: [Edge]
edges = [(1,2),(1,3),(2,4),(2,5),(3,6),(3,7),(4,8),(4,9),(5,10),(5,11),(6,12),(6,13),(7,14),(7,15)]

start :: Vertex
start = 1

stop :: Vertex
stop = 15

type State = [Vertex]

initialState :: State
initialState = [start]

loser :: State
loser = []

goalTest :: State -> Bool
goalTest (v:vs) = v == stop

next :: State -> [Vertex]
next (v:vs) = [y | (x,y) <- edges, x==v]

operator:: State -> [State]
operator vs = [(n:vs) | n <- next vs, notElem n vs]

dfs :: [State] -> (State -> [State]) -> State
dfs [] op = loser
dfs x op | trace (show x) False = undefined
dfs (x:xs) op
   | goalTest x  = x
   | otherwise   = dfs (newstates ++ xs) op
   where newstates = op x

bfs :: [State] -> (State -> [State]) -> State
bfs [] op = loser
bfs x op | trace (show x) False = undefined
bfs (x:xs) op
   | goalTest x   = x
   | otherwise    = bfs (xs ++ newstates) op
   where newstates = op x
