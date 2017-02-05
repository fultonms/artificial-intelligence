--A Domino has two strings: a top and bottoms
type Domino = (String, String)

dominoes :: [Domino]
dominoes = [("0101", "01"), ("1", "01101")]

--A state is the current list of dominos, in order
type State = [Domino]

initial :: State
initial = [head dominoes]

loser :: State
loser = []

topWord :: State -> String
topWord [] = ""
topWord (x:xs) = fst x ++ topWord(xs)

botWord :: State -> String
botWord [] = ""
botWord (x:xs) = snd x ++ botWord(xs)

goalTest :: State -> Bool
goalTest s = topWord s == botWord s

operator :: State -> [State]
operator s = [s ++ [x] | x <-dominoes]

search :: State
search = treeSearch [initial] operator

treeSearch :: [State] -> (State -> [State]) -> State
treeSearch [] op = loser
treeSearch (x:xs) op
 | goalTest x  = x
 | otherwise   = treeSearch (xs ++ newstates) op
  where newstates = op x
