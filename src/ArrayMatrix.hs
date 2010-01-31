module ArrayMatrix (ArrayMatrix, fromRows, toRows, toListWithPos, neighbourMap)
  where

import Matrix
import Array

data ArrayMatrix a = ArrayMatrix {
    rows'    :: Integer,
    columns' :: Integer,
    cells    :: Array Integer (Array Integer a)
  }

instance Matrix ArrayMatrix a_
  where
    at m x y = (cells m ! y) ! x
    rows     = rows'
    columns  = columns'

    fromRows m@(y@(_:_):_) = ArrayMatrix rows columns cells
      where
        rows    = fromIntegral $ length m
        columns = fromIntegral $ length y
        cells   = cellsf m
        cellsf  = listArray (0, rows - 1) . map (listArray (0, columns - 1))

    fromRows m = error "Invalid rowlist passed in."
