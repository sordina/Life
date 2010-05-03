module ListUtils where

splitLen :: Integer -> [a] -> [[a]]
splitLen width starting = s starting []
  where
    w = fromIntegral width
    s :: [a] -> [[a]] -> [[a]]
    s [] done = done
    s todo done = start : s rest done
      where
        (start, rest) = splitAt w todo

nestedAt :: Integer -> Integer -> [[a]] -> a
nestedAt outside inside = (!! i) . (!! o)
  where
    i = fromInteger inside
    o = fromInteger outside

zipWithRows2 :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipWithRows2 = zipWith . zipWith
