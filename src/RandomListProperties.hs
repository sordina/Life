module RandomListProperties (prop_elem) where

import Random
import RandomList
import Test.QuickCheck (Property, (==>))

-- Tests that a random selection from a list is an element of that list.
prop_elem :: [Integer] -> Property
prop_elem list = not (null list) ==> result `elem` list
  where
    gen = mkStdGen 8008135
    (g, result) = randomListValue gen list
