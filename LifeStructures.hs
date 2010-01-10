module LifeStructures where

import ListUtils (splitLen, nestedAt)
import Data.Maybe (catMaybes)

-- Data structures

data LifeSnapshot a = LifeSnapshot {
    startCell :: LifeCell a,
    nested :: [[LifeCell a]],
    nestedRaw :: [[a]]
  }

data LifeCell a = LifeCell {
    state :: a,
    neighbours :: [Maybe (LifeCell a)]
  }

-- Instances

instance Functor LifeSnapshot
  where fmap f = fromNestedRaw . map (map f) . toNestedRaw

-- Special instance for neighbour useage

fmap' :: (LifeCell a -> a) -> LifeSnapshot a -> LifeSnapshot a
fmap' f = fromNestedRaw . map (map f) . toNested

-- Constructors

mkCell :: [[a]] -> (Integer, Integer) -> (Integer, Integer) -> LifeCell a
mkCell matrix (x, y) (mx, my) = LifeCell {
    state = nestedAt x y matrix,
    neighbours = do
      y' <- [x - 1 .. x + 1]
      x' <- [y - 1 .. y + 1]
      return $ mkMaybe $ mkCell matrix (x', y') (mx, my)
  }
  where
    n = const Nothing
    mkMaybe :: LifeCell a -> Maybe (LifeCell a)
    mkMaybe
      | x <= 0 = n
      | y <= 0 = n
      | x >= mx = n
      | y >= my = n
      | otherwise = Just

fromNestedRaw :: [[a]] -> LifeSnapshot a
fromNestedRaw cells = fromNestedInner (fromIntegral $ length $ head cells) cells

fromNestedInner :: Integer -> [[a]] -> LifeSnapshot a
fromNestedInner width cells = LifeSnapshot {
    startCell = sCell,
    nestedRaw = cells,
    nested = (map rightList . downList) sCell
  }
  where
    mx = width
    my = fromIntegral $ length cells
    sCell = mkCell cells (0, 0) (mx, my)

fromFlat :: Integer -> [a] -> LifeSnapshot a
fromFlat width = fromNestedInner width . (splitLen width)

-- Destructors

toNested :: LifeSnapshot a -> [[LifeCell a]]
toNested = nested

toNestedRaw :: LifeSnapshot a -> [[a]]
toNestedRaw = nestedRaw

toFlat :: LifeSnapshot a -> [LifeCell a]
toFlat = concat . toNested

toFlatRaw :: LifeSnapshot a -> [a]
toFlatRaw = concat . toNestedRaw

-- Direction functions

direction :: Integer -> LifeCell a -> Maybe (LifeCell a)
direction num = (!! n) . neighbours
  where n = fromIntegral num

up = direction 1
down = direction 6
left = direction 3
right = direction 5

angle :: (LifeCell a -> Maybe (LifeCell a)) -> LifeCell a -> [LifeCell a]
angle f cell = cell : mkRest next
  where
    next = f cell
    mkRest Nothing = []
    mkRest (Just nextCell) = angle f nextCell

downList = angle down
rightList = angle right

-- Existance functions

catNeighbours :: LifeCell a -> [LifeCell a]
catNeighbours = catMaybes . neighbours

catNeighboursRaw :: LifeCell a -> [a]
catNeighboursRaw = map state . catMaybes . neighbours
