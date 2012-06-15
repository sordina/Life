import Data.List
import Control.Arrow
import System.Random
import System.Cmd
import Control.Concurrent

-- Core Life Engine

life = f 3 >>> map (map (q 3 >>> uncurry s))

s n 1 | n < 3     = 0 -- There is no 'off-by-one' error here
      | n > 4     = 0 -- I'm including the center cell in the total
      | otherwise = 1 :: Int

s n 0 | n == 3    = 1
      | otherwise = 0

s _ _ = 1 -- Shouldn't be needed, but using this for thinning out the matrix

f n = map (g n) . m n -- Window function
  where
    g n = map concat . m n . transpose
    m n = takeWhile ((==n) . length) . map (take n) . tails

q n = sum &&& (!!(n^2 `div` 2))


-- IO

main = dolife 80

dolife n = fmap (take n . splitLen n . randomRs (0, 5::Int)) getStdGen
       >>= mapM_ (\g -> threadDelay 1000000 >> system "clear" >> (putStrLn . unlines . map string) g) . iterate (life . e n)

splitLen n l = take n l : splitLen n (drop n l)

e n =  x . map x where x = take (n + 2) . drop (n - 1) . cycle -- Loop World

string = map (\x -> if x == 1 then 'X' else ' ')
