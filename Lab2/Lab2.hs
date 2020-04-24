main = do
  let ar=[5,4,3,4,3]
  let indexes = [i|i<-[0..length ar -1], i < length ar, (i==0||ar!!(i-1) < ar!!(i)), (i==(length ar-1)||ar!!(i+1)< ar!!i)]

  let releasecandidat = zip ar [0..] 
  print ([x| x <- releasecandidat, snd (x) `elem` indexes])