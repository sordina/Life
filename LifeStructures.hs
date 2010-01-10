module LifeStructures (LifeSnapshot, LifeCell)
where

newtype LifeSnapshot = LifeSnapshot {
    startCell :: LifeCell
  }

newtype LifeCell = LifeCell {
    neighbours :: [LifeCell]
  }
