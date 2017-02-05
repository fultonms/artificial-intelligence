wordlist :: [String]
wordlist = ["tap", "private", "elevator", "first"]
--wordlist = ["bit", "rap"]

--The State is the current list of words.
type State = [String]

initial :: State
initial = []

loser :: State
loser = []

inOrder :: [String] -> Bool
inOrder [] = True
inOrder [x] = True
inOrder (x:xs) = (last x == head (head xs)) && inOrder xs

goalTest :: State -> Bool
goalTest s = (inOrder s) && ([x | x <- wordlist, notElem x s] == [])

operator :: State -> [State]
operator s = [(x:s) | x <- wordlist, (notElem x s) && (inOrder (x:s))]

search :: State
search = treeSearch [initial] operator

treeSearch :: [State] -> (State -> [State]) -> State
treeSearch [] op = loser
treeSearch (x:xs) op
 | goalTest x  = x
 | otherwise   = treeSearch (newstates ++ xs) op
  where newstates = op x
