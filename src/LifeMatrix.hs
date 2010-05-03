module LifeMatrix (randomGame, LifeSnapshot, Health (Alive, Dead), toListWithPos) where

-- Libraries
import Control.Monad

-- Imports
import ArrayMatrix2
import ListUtils
import RandomList
import Random
import Colors

type LifeSnapshot = ArrayMatrix Health

data Health = Alive Color | Dead deriving (Eq, Show)

randomGame :: (Integral i) => i -> IO [LifeSnapshot]
randomGame = liftM (iterate nextSnapshot) . randomSnapshot

randomSnapshot :: Integral i => i -> IO LifeSnapshot
randomSnapshot size = do
  gen <- newStdGen
  colors <- return $ randomList gen non_black
  deads  <- return $ randomList gen [True, False]
  lifes  <- return $ take s2 (zipWith zipper deads colors)
  return $ fromRows $ splitLen s lifes

  where
    s  :: Integer = fromIntegral size
    s2 :: Int = fromIntegral size ^ 2
    zipper False _     = Dead
    zipper True  color = Alive color

nextSnapshot :: LifeSnapshot -> LifeSnapshot
nextSnapshot = neighbourMap nextState

countHealth :: [Health] -> Integer
countHealth = foldr f 0
  where
    f (Alive _) n = n + 1
    f Dead n = n

colors []                 = []
colors (Alive color : xs) = color : colors xs
colors (Dead        : xs) =         colors xs

nextState :: Health -> [Health] -> Health
nextState (Alive color) neighbours
  | num < 2 || num > 3 = Dead
  | otherwise          = Alive $ foldl1 (|-|) (color : colors neighbours)
  where num = countHealth neighbours

nextState Dead neighbours
  | countHealth neighbours == 3 = Alive white
  | otherwise                   = Dead

instance Show LifeSnapshot
  where
    show l = unlines (map (map f) (toRows l))
      where
        f (Alive _) = 'x'
        f Dead  = ' '

-- This probably doesn't work as I have no idea what I'm doing... But it typechecks ^___^
instance Read LifeSnapshot
  where
    readsPrec _ str = [(result, str)]
      where
        result = fromRows rows
        rows = map (map f) l
        l = lines str
        f 'x' = Alive white
        f _   = Dead
