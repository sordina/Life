{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Matrix (
    Matrix,
    fromRows,
    toList,
    toListWithPos,
    toRows,
    rows,
    columns,
    at,
    neighbours,
    neighbourMap
  ) where

import Data.Maybe (catMaybes)

class Matrix m a
  where
    -- required
    fromRows       :: [[a]] -> m a
    rows           :: m a   -> Integer
    columns        :: m a   -> Integer
    at             :: m a   -> Integer -> Integer -> a

    -- defaults
    toList         :: m a   -> [a]
    toListWithPos  :: m a   -> [(Integer, Integer, a)]
    toRows         :: m a   -> [[a]]
    row            :: m a   -> Integer -> [a]
    column         :: m a   -> Integer -> [a]
    vicinityRows   :: m a   -> Integer -> Integer -> [[Maybe a]]
    vicinityMatrix :: Matrix m (Maybe a) => m a -> Integer -> Integer -> m (Maybe a)
    neighbours     :: Matrix m (Maybe a) => m a -> Integer -> Integer -> [a]
    neighbourMap   :: Matrix m (Maybe a) => (a -> [a] -> a) -> m a -> m a

    toList m = do
      y <- [0 .. rows m - 1]
      x <- [0 .. columns m - 1]
      return $ at m x y

    toListWithPos m = do
      y <- [0 .. rows m - 1]
      x <- [0 .. columns m - 1]
      return (x, y, at m x y)

    toRows m = do
      y <- [0 .. rows m - 1]
      return $ do
        x <- [0 .. columns m - 1]
        return $ at m x y

    row    m n = [at m x n | x <- [0 .. columns m - 1]]
    column m n = [at m n y | y <- [0 .. rows    m - 1]]

    vicinityRows m x y = do
      y' <- [y - 1 .. y + 1]
      return $ do
        x' <- [x - 1 .. x + 1]
        return $ cell x' y' where
          cell x y
            | x <  0         = Nothing
            | y <  0         = Nothing
            | x >= columns m = Nothing
            | y >= rows m    = Nothing
            | otherwise      = Just $ at m x y

    vicinityMatrix m x = fromRows . vicinityRows m x

    neighbours m x y = catMaybes $ outside $ toList $ vicinityMatrix m x y
      where
        outside l = take 4 l ++ drop 5 l

    neighbourMap f m = fromRows $ do
      y <- [0 .. columns m - 1]
      return $ do
        x <- [0 .. rows m - 1]
        return $ f (at m x y) (neighbours m x y)
