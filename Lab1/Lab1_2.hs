toInt :: Float -> Int
toInt x = round x

toFloat :: Int -> Float
toFloat number = fromIntegral  number

getPerfectSqureIndexList :: Int->[Int]->[Int]
getPerfectSqureIndexList lengthOfList list 
  | ((lengthOfList - length list) /= lengthOfList) && (is_square(lengthOfList- length list)) = head(list) : getPerfectSqureIndexList lengthOfList (tail list)
  | ((lengthOfList - length list) /= lengthOfList) && (is_square(lengthOfList- length list)==False) = getPerfectSqureIndexList lengthOfList (tail list)
  |otherwise = []
  
is_square :: Int -> Bool
is_square n = sq * sq == n
    where sq = floor $ sqrt $ (fromIntegral n::Double)

main :: IO ()
main = do
 let list = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
 
 let t = getPerfectSqureIndexList (length list) list
 --let t = findIndex isPerfectSquare() list
 print t
 --putStrLn "Works"