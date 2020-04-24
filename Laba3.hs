first :: (a, b, c) -> a
first (requiredElement, _, _) = requiredElement
second :: (a, b, c) -> b
second (_, requiredElement, _) = requiredElement
third :: (a, b, c) -> c
third (_, _, requiredElement) = requiredElement

--splitWords word = Split.splitOn  "," word

state = [1, 2, 3]
alphabet = ['a', 'b']
s_0 = 1
s_f = [3]
transition = [(1, 'a', 2), (1, 'b', 1), (2, 'a', 2), (2, 'b', 3), (3, 'a', 1),(3, 'b', 3)]

intToList :: a -> [a]
intToList item = [item]

getTrans :: Int -> [(Int, Char, Int)]
getTrans state = filter(\x-> first x == state) transition

getDest::Int->Char->Int 
getDest state symb = third ((filter(\x-> first x == state && second x == symb) transition)!!0)

createWord:: Int->Char->Int->[Char]
createWord size symbol current | current < size = symbol : createWord size symbol (current+1)
                               | current == size = []

finder :: [Int]->Char->Int->Int->[Char]
finder reachedStates symbol current size | elem (getDest (last reachedStates) symbol) s_f = createWord (length reachedStates + 1) symbol 0
                            |  not(elem (getDest (last reachedStates) symbol) reachedStates) = finder (reachedStates ++ intToList(getDest (last reachedStates) symbol)) symbol current size
                            |  (elem (getDest (last reachedStates) symbol) reachedStates) && current+1 == size  = ""
                            |  (elem (getDest (last reachedStates) symbol) reachedStates)  = finder [1] (alphabet !! (current+1)) (current+1) size

findWord :: [Char]
findWord  =  finder [1] (alphabet !! 0) 0 (length alphabet)

main = do
 print findWord