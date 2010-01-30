{--
    http://en.wikipedia.org/wiki/Conway's_Game_of_Life

    Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
    Any live cell with more than three live neighbours dies, as if by overcrowding.
    Any live cell with two or three live neighbours lives on to the next generation.
    Any dead cell with exactly three live neighbours becomes a live cell.
--}

module LifeMatrix (randomGame, LifeSnapshot, Health (Alive, Dead), toListWithPos) where

-- Libraries
import Control.Monad

-- Imports
import ArrayMatrix
import RandomList
import Random

type LifeSnapshot = ArrayMatrix Health

data Health = Alive | Dead deriving (Eq, Show)

randomGame :: (Integral i) => i -> IO [LifeSnapshot]
randomGame size = liftM (iterate nextSnapshot) (randomSnapshot size)

randomSnapshot :: Integral i => i -> IO LifeSnapshot
randomSnapshot size = do
  gen <- newStdGen
  return $ fromRows $ randomRowsN gen [Alive, Dead] size size

nextSnapshot :: LifeSnapshot -> LifeSnapshot
nextSnapshot = neighbourMap nextCell

nextCell :: Health -> [Health] -> Health
nextCell cell neighbours = nextState cell (countHealth neighbours)

countHealth :: [Health] -> Integer
countHealth = foldr f 0
  where
    f Alive n = n + 1
    f Dead n = n

nextState :: Health -> Integer -> Health
nextState Alive neighbours
  | neighbours < 2 = Dead
  | neighbours > 3 = Dead
  | otherwise      = Alive

nextState Dead neighbours
  | neighbours == 3 = Alive
  | otherwise       = Dead

instance Show LifeSnapshot
  where
    show l = unlines (map (show . map f) (toRows l))
      where
        f Alive = "x"
        f Dead  = " "

-- This probably doesn't work as I have no idea what I'm doing... But it typechecks ^___^
instance Read LifeSnapshot
  where
    readsPrec _ str = [(result, str)]
      where
        result = fromRows rows
        rows = map (map f) l
        l = lines str
        f 'x' = Alive
        f ' ' = Dead
