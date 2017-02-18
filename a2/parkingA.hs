-- Pos is a position on the board
-- it is a row and column
type Pos = (Int,Int)

-- car is a list of positions
-- given in order from left to right or top to bottom
-- it must take at least two positions
type Car = [Pos]

-- parking lot is a list of cars
type Parking = [Car]

-- a board is a pair consisting of your car
-- plus the rest of the parking lot
-- type Board = (Car,Parking)
data Board = B (Car,Parking)
  deriving Eq

-- a state is a board
type State = Board

-- a history gives you all the states in the path so far from current to initial state
type History = [State]

-- number of rows in parking lot from top to bottom
rows :: Int
rows = 5

-- number of columns in parking lot from left to right
columns :: Int
columns = 5

-- done when front or back of car is here
done  :: Pos
done = (5,2)

-- down gives a list of places a car could go by moving down
-- first argument is the car to be moved
-- second argument is the rest of the Parking lot
-- returns singleton list or empty list of Cars
down :: Car -> Parking -> [Car]
down  ((c1,c2) : (d1,d2) : cs) p
  | c2 /= d2 = []
  | e1 == rows = []
  | occupied (e1+1,c2) p = []
  | otherwise = [(d1,d2) : cs ++ [(e1+1,c2)]]
    where (e1,e2) = last ((d1,d2) : cs)

up :: Car -> Parking -> [Car]
up  ((c1,c2) : (d1,d2) : cs) p
  | c2 /= d2 = []
  | c1 == 1 = []
  | occupied (c1-1,c2) p = []
  | otherwise =  [(c1-1,c2) : init ((c1,c2):(d1,d2) : cs)] 

right :: Car -> Parking -> [Car]
right  ((c1,c2) : (d1,d2) : cs) p
  | c1 /= d1 = []
  | e2 == columns = []
  | occupied (c1,e2+1) p = []
  | otherwise = [(d1,d2) : cs ++ [(c1,e2+1)]]
    where (e1,e2) = last ((d1,d2) : cs)

left :: Car -> Parking -> [Car]
left  ((c1,c2) : (d1,d2) : cs) p
  | c1 /= d1 = []
  | c2 == 1 = []
  | occupied (c1,c2-1) p = []
  | otherwise =  [(c1,c2-1) : init ((c1,c2):(d1,d2) : cs)] 

occupied :: Pos -> Parking -> Bool
occupied z []  = False
occupied z (c : cs)
  | inSpot z c = True
  | otherwise = occupied z cs
  
inSpot :: Pos -> Car -> Bool
inSpot z [] = False
inSpot z (c : cs)
  | z == c = True
  | otherwise = inSpot z cs

-- initial state 
c1 :: Car
c1 = [(1,2),(2,2)]

c2 :: Car
c2 = [(4,5),(5,5)]

c3 :: Car
c3 = [(4,1),(4,2),(4,3)]

c4 :: Car
c4 = [(2,4),(2,5)]

c5 :: Car
c5 = [(1,3),(2,3)]

p :: Parking
p = [c2,c3,c4]

p2 :: Parking
p2 = [c5]

initialState :: State
initialState = B (c1,p) 

-- empty list if there is no solution
loser :: History
loser = []

-- we are done when in the final position
goalTest :: State -> Bool
goalTest (B ((c1:c1s),p))  
  | c1 == done = True
  | last (c1:c1s) == done = True
  | otherwise = False  
  
-- heuristic of 0 only looks at distance travelled so far
-- I want you to replace this with your heuristic
h :: State -> Int
h (B(c,p)) = minimum [(distance x done )| x <- c]

distance:: Pos -> Pos -> Int
distance a b = let (x1, y1) = a
                   (x2, y2) = b
               in (abs (x1-x2)) + (abs (y1-y2))
  
-- moves a specific car in the parking lot
-- takes the car and the rest of the parking lot
-- returns the set of all possible new positions of the car
moveCar :: Car -> Parking -> [Car]
moveCar c p = down c p
  ++ up c p
  ++ left c p
  ++ right c p

-- moves all cars in the parking lot   
-- takes a board 
-- returns a list of boards resulting from moving
moveCars :: Board -> [Board]
moveCars (B (c,p)) = moveCarsPrev c [] p

-- takes a list of new values of your car plus parking lot
-- returns a list of new boards resulting from that
putBackYourCar :: [Car] -> Parking -> [Board]
putBackYourCar [] p = []
putBackYourCar (c:cs) p = (B (c,p)) : (putBackYourCar cs p)

-- takes your car
-- takes a list of values of another car in the parking lot 
-- plus the rest of  the parking lot
-- returns a list of new boards resulting from that
putBackAnotherCar :: Car -> [Car] -> Parking -> [Board]
putBackAnotherCar c [] p = []
putBackAnotherCar c (c1:c1s) p = (B (c,c1:p)) : (putBackAnotherCar c c1s p)

-- moves all cars left in the parking lot
-- first argument is your car
-- second argument is previous cars in the parking lot
-- third argument is the cars left in the parking lot
-- returns a list of new boards resulting from a move 
moveCarsPrev :: Car -> Parking -> Parking -> [Board]
moveCarsPrev c p [] =  putBackYourCar (moveCar c p) p  
moveCarsPrev c p1 (c2:p2) = 
  (putBackAnotherCar c (moveCar  c2 (c:p1 ++ p2))  (p1 ++ p2)) ++ (moveCarsPrev c (c2:p1) p2)

-- the operator moves a car in some direction
operator :: History -> [History]
operator (b:bs) = [newboard:(b:bs) | newboard <- moveCars b]

-- a node stores a state, its g value, and its h value
type Node = (History, Int, Int)

-- print out board in a nice way
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

