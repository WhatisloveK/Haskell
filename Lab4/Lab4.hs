import Data.List 

first :: (a, b) -> a
first (requiredElement, _) = requiredElement

second :: (a, b) -> b
second (_, requiredElement) = requiredElement

nonTerminals :: [Char]
nonTerminals = ['A'..'Z']

grammar :: [(Char, [Char])]
-- grammar = [ ('S',"X"),('S',"Y"),
--  ('X',"aXab"),('X',"ab"),
--  ('Y',"aYd"), ('Y',"b")] 

-- grammar = [ ('S',"TK"), 
--  ('K',"+TK"), ('K',"e"),
--  ('T',"ML"),
--  ('L',"*MT"), ('L', "e"),
--  ('M',"(S)"), ('M',"c")]

-- grammar = [ ('S',"aBD"),('S',"D"),('S',"AC"),('S',"b"),
--  ('A',"SCB"), ('A',"SABC"), ('A',"CbD"), ('A',"e"), 
--  ('B',"CA"),('B',"d"),
--  ('C',"ADc"), ('C',"a"), ('C',"e"),
--  ('D',"EaC"), ('D',"SC"),
--  ('E',"BCS"), ('E',"a")] 
grammar = [ ('S',"AbS"), ('S',"AC"), 
 ('A',"BD"),
 ('C',"Sa"), ('C',"e"),
 ('B',"BC"), ('B',"e"), 
 ('D',"aB"), ('D',"BA")]

getNonTerminalsFromGrammar :: [Char]
getNonTerminalsFromGrammar = [ first x | x <- grammar]

getUniqueNonTerminalsFromGrammar :: [Char]
getUniqueNonTerminalsFromGrammar = intersect nonTerminals getNonTerminalsFromGrammar

-------------------------------------------Epsilon Non Terminals-----------------------------------------------
getInitENonTerminal :: [Char]
getInitENonTerminal = [ first x | x <- grammar, second x == "e" ]

getCurrentENonTerminals :: [Char] -> [Char]
getCurrentENonTerminals listOfENonTerminals = [ first x | x <- grammar, intersect (second x) listOfENonTerminals == second x, not(elem (first x) listOfENonTerminals) ]

getEpsilonNonTerminals :: [Char] -> [Char]
getEpsilonNonTerminals listOfEpsilonNonTerminals 
    | length (getCurrentENonTerminals listOfEpsilonNonTerminals) == 0 = listOfEpsilonNonTerminals
    | otherwise = getEpsilonNonTerminals (listOfEpsilonNonTerminals ++ (getCurrentENonTerminals listOfEpsilonNonTerminals))

-----------------------------------------------------------------------------------------------------------------


getPossibleRules :: [Char] -> [(Char, [Char])]
getPossibleRules currentNonTerminals = [ x | x <- grammar, elem (first x) currentNonTerminals, elem (last (second x)) nonTerminals ]

getAdjustedRules :: [Char] -> [Char] -> Int -> [Char]
getAdjustedRules word eNonTerminals current 
    | (current == ((length word) - 1)) && (elem (word !! current) eNonTerminals) = (word !! current) : getAdjustedRules word eNonTerminals (current - 1)
    | (current == ((length word) - 1)) = (word !! current) : []
    | ( current >= 0) && (elem (word !! current) eNonTerminals) = word !! current : getAdjustedRules word eNonTerminals (current - 1)
    | ( current >= 0) && (elem (word !! current) nonTerminals) = word !! current : []
    | otherwise = []

getRulesToBeChecked :: [(Char, [Char])] -> [Char] -> [Char]
getRulesToBeChecked rules eNonTerminals  = nub (concat [ getAdjustedRules (second x) eNonTerminals (length (second x) - 1)  | x <- rules ] )

getNewRulesToBeChecked :: [Char] -> [Char] -> [Char]
getNewRulesToBeChecked rules existingRules = [ x | x <- rules, not (elem x existingRules) ]

getReachableStates :: [Char] -> [Char] -> [Char]
getReachableStates currentNonTerminals eNonTerminals 
    | length (getNewRulesToBeChecked (getRulesToBeChecked (getPossibleRules currentNonTerminals) eNonTerminals) currentNonTerminals) == 0 = currentNonTerminals
    | otherwise = getReachableStates (currentNonTerminals ++ (getNewRulesToBeChecked (getRulesToBeChecked (getPossibleRules currentNonTerminals) currentNonTerminals) currentNonTerminals)) eNonTerminals

hasRecursionForCurrentNonTerminal :: Char -> [Char] -> Bool
hasRecursionForCurrentNonTerminal current eNonTerminals = elem current (getReachableStates (getRulesToBeChecked (getPossibleRules (current:[])) eNonTerminals) eNonTerminals)

hasRightRecursiveNonTerminals :: [Char] -> Int -> [Char] -> [Char]    
hasRightRecursiveNonTerminals nonTerminalsToBeChecked current eNonTerminals
    | hasRecursionForCurrentNonTerminal (nonTerminalsToBeChecked !! current) eNonTerminals = 
        if (current+1< length nonTerminalsToBeChecked) then (nonTerminalsToBeChecked !! current) : (hasRightRecursiveNonTerminals nonTerminalsToBeChecked (current + 1) eNonTerminals)
        else (nonTerminalsToBeChecked !! current) : []
    | (current + 1 < length nonTerminalsToBeChecked) = hasRightRecursiveNonTerminals nonTerminalsToBeChecked (current + 1) eNonTerminals
    | otherwise = ""


main :: IO ()
main = do
 let eNonTerminals = getEpsilonNonTerminals getInitENonTerminal
 print (hasRightRecursiveNonTerminals getUniqueNonTerminalsFromGrammar 0 eNonTerminals)