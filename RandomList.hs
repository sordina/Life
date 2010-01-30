module RandomList (randomListValue, randomList) where

import Random
import ListUtils

randomListValue :: RandomGen g => g -> [a] -> (g,a)
randomListValue gen list = (g,val)
  where
    val = list !! i
    (i,g) = randomR (0,l) gen 
    l = length list

randomList :: RandomGen g => g -> [a] -> [a]
randomList gen list = map snd l
  where
    l = iterate f i
    i = randomListValue gen list
    f (g, val) = randomListValue g list

randomListN :: (RandomGen g, Integral i) => g -> [a] -> i -> [a]
randomListN gen list len = take (fromIntegral len) (randomList gen list)

randomRows :: (RandomGen g, Integral i) => g -> [a] -> i -> [[a]]
randomRows gen list width = splitLen (fromIntegral width) l
  where
    l = randomList gen list

randomRowsN :: (RandomGen g, Integral i) => g -> [a] -> i -> i -> [[a]]
randomRowsN gen list width len = take (fromIntegral len) (randomRows gen list (fromIntegral width))

{-- Can't test StdGen elements for some ungodly reason.

prop_elem :: StdGen -> [Integer] -> Bool
prop_elem gen list = result `elem` list
  where (g, result) = randomListValue gen list

-}
