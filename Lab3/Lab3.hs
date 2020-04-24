first (requiredElement, _, _) = requiredElement
second (_, requiredElement, _) = requiredElement
third (_, _, requiredElement) = requiredElement

--splitWords word = Split.splitOn  "," word

state = [1, 2, 3, 4, 5]
alphabet = ['a', 'b', 'c']
s_0 = 1
s_f = [4, 5]
transition = [(1, 'a', 2), (1, 'b', 1), (2, 'a', 3), (2, 'b', 4), (2, 'c', 5),(5,'a',5),(5,'b',3),(3,'a',5),(3,'b',4),(4,'b',4),(4,'a',1)]
quantity = 3

intToList item = [item]

getTrans state = filter(\x-> first x == state) transition

getDest::Int->Char->Int
getDest state symb = third ((filter(\x-> first x == state && second x == symb) transition)!!0)

createWord:: Int->Char->Int->[Char]
createWord size symbol current | current < size = symbol : createWord size symbol (current+1)
                               | current == size = []

finder :: [Int]->Char->Int->Int->[Char]
finder reachedStates symbol current size | elem (getDest (last reachedStates) symbol) s_f = createWord (length reachedStates) symbol 0
                            |  not(elem (getDest (last reachedStates) symbol) reachedStates) && current+1==size = finder [1] (alphabet !! (current+1)) (current+1) size
                            |  not(elem (getDest (last reachedStates) symbol) reachedStates) = finder (reachedStates ++ intToList(getDest (last reachedStates) symbol)) symbol current size
                            |  (elem (getDest (last reachedStates) symbol) reachedStates) && current+1 == size  = ""
                            |  (elem (getDest (last reachedStates) symbol) reachedStates)  = finder [1] (alphabet !! (current+1)) (current+1) size


findWord  =  finder [1] (alphabet !! 0) 0 (length alphabet)

main = do
 print findWord