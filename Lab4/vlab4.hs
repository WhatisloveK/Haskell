import Data.List 

first :: (a, b) -> a
first (requiredElement, _) = requiredElement

second :: (a, b) -> b
second (_, requiredElement) = requiredElement

nonTerminals :: [Char]
nonTerminals = ['A'..'Z']

grammar :: [(Char, [Char])]
grammar = [ ('S',"Xs"),
 ('X',"aXab"),('X',"ab"),
 ('Y',"aYd"), ('Y',"b")] 

getNonTerminalsFromGrammar :: [Char]
getNonTerminalsFromGrammar = [ first x | x <- grammar]

getUniqueNonTerminalsFromGrammar :: [Char]
getUniqueNonTerminalsFromGrammar = intersect nonTerminals getNonTerminalsFromGrammar

getStates :: [Char] -> [Char]
getStates current = filter(\x-> elem x nonTerminals) current

getPossibleStates :: Char -> [Char]
getPossibleStates current = concat [ getStates(second x) | x <- grammar, first x == current ]

getNewPossibleStates :: [Char] -> [Char] -> [Char]
getNewPossibleStates existingStates currentStates = [ x | x <- currentStates, not(elem x existingStates) ]


getAllReachableStates :: [Char] -> Int -> [Char]
getAllReachableStates reachedStates current 
  | (length (getNewPossibleStates reachedStates(getPossibleStates(reachedStates !! current))) /= 0) =  getAllReachableStates (reachedStates ++ (getNewPossibleStates reachedStates(getPossibleStates(reachedStates !! current)))) (current+1)
  | (current+1 < length reachedStates) = getAllReachableStates reachedStates (current+1)
  | otherwise = reachedStates

main :: IO ()
main = do
 let reachedStates = getAllReachableStates "S" 0
 
 print (filter(\x-> elem (first x) reachedStates) grammar)