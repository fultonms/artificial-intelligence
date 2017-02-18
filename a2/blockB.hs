-- a block is just an integer
type Block = Int

-- a pile is a list of blocks from top to bottom
type Pile = [Block]

-- a configuration is a list of piles
type Config = [Pile]

-- a board gives you the next block to discard plus a configuration
data Board = B (Int,Config)
  deriving Eq

 -- a state is just a board
type State = Board  
 
-- a history gives you all the states in the path so far from current to initial state
type History = [State]

-- initial state 
initialState :: State
--initialState = B (7,[[-1],[-1],[2,1,4,3,6,5,7,-1]]) 
--initialState = B (7,[[-1],[-1],[1,2,3,4,5,6,7,-1]]) 
--initialState = B (7,[[-1],[-1],[7,6,5,4,3,2,1,-1]]) 
--initialState = B (5,[[-1],[-1],[4,3,2,1,5,-1]])
--initialState = B (6,[[-1],[-1],[5,4,3,2,1,6,-1]]) 
initialState = B (6,[[-1],[-1],[5,4,3,2,1,6,-1]])
--initialState = B (3,[[2,-1],[1,3,-1]])

-- empty list if there is no solution
loser :: History
loser = []

-- when the next block to dicard is 0 we are done
goalTest :: State -> Bool
goalTest (B (x,c)) =     x == 0 

-- heuristic of 0 only looks at distance travelled so far
-- I want you to replace this with your heuristic
h :: State -> Int
h (B (x,c)) = x + (onTop x c)

onTop :: Int -> Config -> Int
onTop t [] = 0
onTop t (x:xs)
   | elem t x  = (listDistance t x) + (onTop t xs)
   | otherwise = 0

listDistance:: Int -> Pile -> Int
listDistance t (x:xs)
   | x == t    = 0 
   | otherwise = 1 + (listDistance t xs)

-- gives list of all states resulting from a legal move of a 
-- block from one pile to another
moveall :: Board -> [Board]
moveall (B (x,c)) = removedup [B (x, move (p1,p2) c) | p1 <- c, p2 <- c, head p1 > head p2]

-- Removes duplicate boards in a list
removedup :: [Board] -> [Board]
removedup [] = []
removedup (b:bs)
  | elem b bs     = removedup bs
  | otherwise     = b : removedup bs

-- move block from one pile to another
-- as a result those two piles are placed in beginning of the config
move :: (Pile,Pile) -> Config -> Config
move ((p1:p1s),p2) c = (p1:p2) : (p1s : cminus)  
    where cminus = remove (p1:p1s) (remove p2 c)

-- remove an element from a list
remove :: Eq a => a -> [a] -> [a]
remove x (y:ys) 
  | x == y        = ys
  | otherwise     = y : remove x ys

-- discard next block if possible
-- if it is possible, a singleton new state is given, otherwise empty   
discard :: Board -> [Board]
discard (B (x,c))
  | b == x     = [B (x-1, (bs:ps))]
  | otherwise = []
    where ((b:bs):ps) = makenfirst x c  
   
-- place pile with n on top at beginning of config
-- we know there is at most one such config
makenfirst :: Int -> Config -> Config
makenfirst n c = pileswithnontop ++ removeall pileswithnontop c  
    where pileswithnontop = [p | p <- c, head p == n] 
   
-- remove all piles from the configuration   
removeall :: [Pile] -> Config -> Config
removeall [] c = c
removeall (p:ps) c = removeall ps (remove p c)  
   
-- the operator either puts a block on top of a smaller one
-- or it discards a block if it matches the number
operator :: History -> [History] 
operator (b:bs) = [newboard:(b:bs) | newboard <- moveall b ++ discard b]

-- a node stores a state, its g value, and its h value
type Node = (History, Int, Int)  

-- prints out list so it is easier to read
instance Show Board where
  show (B x) = show x ++ "\n"
  
-- let's you do IO
--  x = do
--  print a
--  putStr "It took this many steps: "
--    print b
--       where (a,b) = astar

-- this is the funciton you call to run everything
astar :: (Node,Int)
astar = aStarSearch [([initialState],0,h initialState)] [] 0

-- this is the search function
--
-- it takes 
-- 1. a sorted list of nodes to be processed
-- 2. the list of states that have already been processed
-- 3. the number of nodes that have been processed so far
--
-- it returns a pair containing a goal node and the number of nodes processed
--
-- if the set of nodes to be processed is empty you lose
-- if the first node to be processed is a goal node then we are done
-- if the first node to be processed has already been processed it ignores it
-- otherwise it applies the operator to the first node to be processed
--   and inserts all the new nodes into the sorted list  
aStarSearch :: [Node] -> [State] -> Int -> (Node,Int)
aStarSearch [] used counter = ((loser,0,0),counter)
aStarSearch (((x:xs),gVal,hVal):ss) used counter
  | goalTest x = (((x:xs),gVal,hVal),counter)
  | elem x used = aStarSearch ss used counter
  | otherwise  = aStarSearch (insertAll newNodes ss) (x:used) (counter + 1)
    where newNodes = [((n:ns),gVal+1,h n) | (n:ns) <- (operator (x:xs))]
   
-- insert a list of nodes into a sorted list 
insertAll :: [Node] -> [Node] -> [Node]
insertAll [] ns = ns
insertAll (m:ms) ns = insertAll ms (insert m ns)

-- insert one node into a sorted list
insert :: Node -> [Node] -> [Node]
insert x [] = [x]
insert (x1,g1,h1) ((x2,g2,h2):rest)
  | g1 + h1 <= g2 + h2  = (x1,g1,h1):((x2,g2,h2):rest)
  | otherwise     = (x2,g2,h2):(insert (x1,g1,h1) rest)
