module RandomList (randomListValue, randomList, randomListN, randomRows, randomRowsN) where

import Random
import ListUtils
import Test.QuickCheck (Property, (==>))

randomListValue :: RandomGen g => g -> [a] -> (g,a)
randomListValue gen list = (g,val)
  where
    val = list !! i
    (i,g) = randomR (0,l) gen 
    l = length list - 1

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

-- Properties tests

-- Tests that a random selection from a list is an element of that list.
prop_elem :: [Integer] -> Property
prop_elem list = not (null list) ==> result `elem` list
  where
    gen = mkStdGen 8008135
    (g, result) = randomListValue gen list
