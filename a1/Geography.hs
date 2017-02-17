wordlist :: [String]
wordlist = ["tap", "private", "elevator"]
--wordlist = ["bit", "rap"]

--The State is the current list of words.
type State = [String]

initial :: State
initial = ["first"]

loser :: State
loser = []

inOrder :: [String] -> Bool
inOrder [] = True
inOrder [x] = True
inOrder (x:xs) = (last x == head (head xs)) && inOrder xs

goalTest :: State -> Bool
goalTest s = (inOrder s) && ([x | x <- wordlist, notElem x s] == [])

operator :: State -> [State]
operator s = [ s ++ [x] | x <- wordlist, (notElem x s) && (inOrder (s ++ [x]))]

search :: State
search = treeSearch [initial] operator

treeSearch :: [State] -> (State -> [State]) -> State
treeSearch [] op = loser
treeSearch (x:xs) op
 | goalTest x  = x
 | otherwise   = treeSearch (newstates ++ xs) op
  where newstates = op x
