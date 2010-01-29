module LifeStructures where

import ListUtils (splitLen, nestedAt)
import Data.Maybe (catMaybes)
import Test.QuickCheck.Gen

-- Data structures

data LifeSnapshot a = LifeSnapshot {
    startCell :: LifeCell a,
    nested :: [[LifeCell a]]
  } deriving (Show, Eq)

data LifeCell a = LifeCell {
    state :: a,
    neighbours :: [Maybe (LifeCell a)]
  } deriving (Show, Eq)

-- Instances

instance Functor LifeSnapshot
  where fmap f = fromRows . map (map f) . toRowsRaw

-- Special instance for neighbour useage

fmap' :: (LifeCell a -> a) -> LifeSnapshot a -> LifeSnapshot a
fmap' f = fromRows . map (map f) . toRows

-- Constructors

mkCell :: [[a]] -> (Integer, Integer) -> (Integer, Integer) -> LifeCell a
mkCell matrix (x, y) (mx, my) = LifeCell {
    state = nestedAt x y matrix,
    neighbours = do
      y' <- [y - 1 .. y + 1]
      x' <- [x - 1 .. x + 1]
      return $ mkMaybe x' y'
  }
  where
    mkMaybe x' y'
      | x' <= 0            = Nothing
      | y' <= 0            = Nothing
      | x' >= mx           = Nothing
      | y' >= my           = Nothing
      | x' == x && y' == y = Nothing -- The self cell isn't a neighbour
      | otherwise          = Just $ mkCell matrix (x', y') (mx, my)

fromRows :: [[a]] -> LifeSnapshot a
fromRows rows = fromRowsInner (fromIntegral $ length $ head rows) rows

fromRowsInner :: Integer -> [[a]] -> LifeSnapshot a
fromRowsInner width cells = LifeSnapshot {
    startCell = sCell,
    nested = (map rightList . downList) sCell
  }
  where
    mx = width
    my = fromIntegral $ length cells
    sCell = mkCell cells (0, 0) (mx, my)

fromFlat :: Integer -> [a] -> LifeSnapshot a
fromFlat width = fromRowsInner width . splitLen width

-- Destructors

toRows :: LifeSnapshot a -> [[LifeCell a]]
toRows = nested

toRowsRaw :: LifeSnapshot a -> [[a]]
toRowsRaw = map (map state) . toRows

toFlat :: LifeSnapshot a -> [LifeCell a]
toFlat = concat . toRows

toFlatRaw :: LifeSnapshot a -> [a]
toFlatRaw = concat . toRowsRaw

toFlatWithPositions :: LifeSnapshot a -> [(Integer, Integer, a)]
toFlatWithPositions list = l'''
  where
    l = toRowsRaw list
    l' = map (zip n) l
    l'' = zip n l'
    l''' = concatMap (uncurry f) l''
    f y = map (\(x,i) -> (x, y, i))
    n = [0..]

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

-- Properties tests

testList = [
    1,2,1,2,
    1,1,2,2,
    2,1,2,1,
    2,2,1,1
  ]

testLife = fromFlat 4 testList

testLife' = (map state . toFlat) testLife

prop_cells = testList == testLife'
