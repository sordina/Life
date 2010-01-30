module RandomList where

import Random

randomListValue :: RandomGen g => g -> [a] -> (g,a)
randomListValue gen list = (g,val)
  where
    val = list !! i
    (i,g) = randomR (0,l) gen 
    l = length list

randomList :: (RandomGen g, Integral i) => g -> [a] -> i -> [a]
randomList gen list len = map snd $ take (fromIntegral len) l
  where
    l = iterate f i
    i = randomListValue gen list
    f (g, val) = randomListValue g list
