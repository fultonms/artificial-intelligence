type Vertex = Int
type Edge = (Vertex,Vertex)

vertices :: [Vertex]
vertices = [1..6]

edges :: [Edge]
edges = [(1,2),(1,3),(2,3),(3,1),(2,4),(4,5),(3,6)]

start :: Vertex
start = 1

stop :: Vertex
stop = 6

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

search :: State
search = treeSearch [initialState] operator  

treeSearch :: [State] -> (State -> [State]) -> State
treeSearch [] op = loser
treeSearch (x:xs) op
 | goalTest x  = x
 | otherwise   = treeSearch (newstates ++ xs) op
  where newstates = op x
