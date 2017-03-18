type Node = Int

start :: Node
start = 16

operator :: Node -> [Node]
operator x =
  | x > 5     = [x-3, x - 5]
  | x > 3     = [x-3]
  | otherwise = []

terminal :: Node -> Boolean
terminal n = (operator n == [])

alphabeta :: [State] -> Int -> Int -> Int -> Boolean -> Int
alphabeta [x] d a b player = x
alphabeta (x:xs) d a b player
  | ((d == 0)   = x
  | b >= a      = x
  | player      = max( -9999999, alphabeta (newstates ++ x) (d-1) a b False)
  | otherwise   = min( 9999999, alphabeta (newstates ++ x) (d-1) a b True)
  where newstates = operator xs

alphabetaMin :: [State] -> Int -> Int -> Int -> Int -> Int
alphabetaMin [] d v a b = v
alphabetaMin (x:xs) d v a b
  | b <= newA   = v
  | otherwise 
