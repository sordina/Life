module LifeMatrix (randomGame) where

-- Libraries
import Control.Monad

-- Imports
import ArrayMatrix
import Random

type LifeSnapshot = ArrayMatrix Health

data Health = Alive | Dead deriving (Eq, Show)

instance Random Health
  where

randomGame :: IO [LifeSnapshot]
randomGame = liftM (iterate nextSnapshot) randomSnapshot

randomSnapshot :: IO LifeSnapshot
randomSnapshot = liftM fromRows $ mkRows 100
  where
    mkValues n = sequence $ replicateM n getStdRandom (randomR (Alive,Dead))
    mkRows n = replicateM n (mkValues n)

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
