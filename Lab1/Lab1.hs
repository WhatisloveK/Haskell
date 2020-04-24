readInts :: String -> [Int]
readInts input = read input :: [Int]

readInt :: String -> Int
readInt input = read input :: Int

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort[y | y<-xs, y<x] 
              ++ [x]
              ++ qsort[y | y<-xs, y>=x]

deleteFunction :: Int -> [Int] -> [Int]
deleteFunction min input = 
 if head input == min then tail input
 else head input : deleteFunction min (tail input)


removeList :: [Int] -> [Int] -> [Int]
removeList list forDelition
  | (length forDelition == 0) = list
  | (head(list) `elem` forDelition) = removeList (tail(list)) (deleteFunction (head list)  forDelition )
  | (head(list) `notElem` forDelition) = head(list) :  removeList (tail list) forDelition
  | otherwise = []
  
main = do
  putStrLn "N = 5"
  let n = 5
  putStrLn "Enter list"
  list <- getLine
  let input = readInts list
  let sorted = qsort input

  print  $ removeList input (take n sorted)