{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module ArrayMatrix (ArrayMatrix, fromRows, toRows, toListWithPos, neighbours, neighbourMap)
  where

import Matrix
import Array
import Control.Monad (guard)

data ArrayMatrix a = ArrayMatrix {
    rows'    :: Integer,
    columns' :: Integer,
    cells    :: Array Integer (Array Integer a)
  }

instance Matrix ArrayMatrix a_
  where
    at (ArrayMatrix _ _ cells) x y = cells ! y ! x
    rows     = rows'
    columns  = columns'

    fromRows m@(y@(_:_):_) = ArrayMatrix rows columns cells
      where
        rows    = fromIntegral $ length m
        columns = fromIntegral $ length y
        cells   = cellsf m
        cellsf  = listArray (0, rows - 1) . map (listArray (0, columns - 1))

    fromRows _ = error "Invalid rowlist passed in."

    -- Include some optimizations for builtins

    -- We should be able to bypass 3 levels of abstraction
    -- with an efficient implementation of neighbours here.
    neighbours (ArrayMatrix rows columns cells) x y = do
      y' <- [max 0 (y-1) .. min (columns-1) (y+1)]
      x' <- [max 0 (x-1) .. min (rows-1)    (x+1)]
      guard $ (x' /= x) || (y' /= y)
      return $ cells ! y' ! x'
