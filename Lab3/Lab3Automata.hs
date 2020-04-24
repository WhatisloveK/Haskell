import Data.List.Split as Split

first (requiredElement, _, _) = requiredElement
second (_, requiredElement, _) = requiredElement
third (_, _, requiredElement) = requiredElement

splitWords word = Split.splitOn  "," word

state = [1, 2, 3, 4, 5]
alphabet = ['a', 'b', 'c']
s_0 = 1
s_f = [3, 4, 5]
transition = [(1, 'a', 2), (1, 'b', 1), (2, 'a', 3), (2, 'b', 4), (2, 'c', 5)]
quantity = 3

getTransitions state = filter(\x-> first x == state) transition

getDest state symb = third ((filter(\x-> first x == state && second x == symb) transition)!!0)
checkWhatStateAndWord' word state k | k>=length word = (state, word)
                                    | otherwise = checkWhatStateAndWord' word (getDest state (word!!k)) (k+1) 
checkWhatStateAndWord word = checkWhatStateAndWord' word s_0 0

goTransitions words pr_word trans k | k>=length trans = words
                              | otherwise = goTransitions (doAutomat' (writeWords words (pr_word++[(second (trans!!k))])) (checkWhatStateAndWord (pr_word++[(second (trans!!k))])) (length (pr_word++[(second (trans!!k))]))) pr_word trans (k+1)

doAutomat' words state_word count | count>=quantity =  words
                                 | otherwise = goTransitions words (snd state_word) (getTransitions (fst state_word)) 0

writeWords words new_word = words++","++new_word

doAutomat = doAutomat' "" (s_0, "") 0

isRightWord word = fst(checkWhatStateAndWord word) elem s_f

findNeededLengthWords words = filter(\x->length x == quantity) (splitWords words)

findNeededWords words = filter(\x->isRightWord x) (findNeededLengthWords words)

doWordsExist words = if length (findNeededWords words)>0 then True else False

main = do
    let t = doAutomat
    let res = doWordsExist t
    print res