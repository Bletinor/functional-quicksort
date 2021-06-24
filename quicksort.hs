quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort lowerTail ++ [x] ++ quickSort upperTail
 where
  lowerTail = [y | y <- xs, y < x]
  upperTail = [y | y <- xs, y >= x]

main :: IO()
main = do
    print $ quickSort [10,8,5,6,3,2,7,1,9,4]
