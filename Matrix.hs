module Matrix (Matrix, fromRows, toList, rows, columns, at, neighbours) where

import Data.Maybe (catMaybes)

class Matrix m a
  where
    fromRows       :: [[a]] -> m a
    toList         :: m a   -> [a]
    rows           :: m a   -> Integer
    columns        :: m a   -> Integer
    row            :: m a   -> Integer -> [a]
    column         :: m a   -> Integer -> [a]
    at             :: m a   -> Integer -> Integer -> a
    vicinityRows   :: m a   -> Integer -> Integer -> [[Maybe a]]
    vicinityMatrix :: Matrix m (Maybe a) => m a -> Integer -> Integer -> m (Maybe a)
    neighbours     :: Matrix m (Maybe a) => m a -> Integer -> Integer -> [a]

    toList m = do
      y <- [0 .. rows m - 1]
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
            | otherwise       = Just $ at m x y

    vicinityMatrix m x y = fromRows $ vicinityRows m x y

    neighbours m x y = catMaybes $ outside $ toList $ vicinityMatrix m x y

outside l = take 4 l ++ drop 5 l
