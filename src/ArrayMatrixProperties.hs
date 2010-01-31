module ArrayMatrixProperties where

import ArrayMatrix

prop_neighbours :: Bool
prop_neighbours = neighbours matrix 2 1 == "bcdhjnop"

matrix :: ArrayMatrix Char
matrix = fromRows [
    "abcdef",
    "ghijkl",
    "mnopqr",
    "stuvwx"
  ]

instance Show (ArrayMatrix Char)
  where
    show = unlines . map show . toRows
