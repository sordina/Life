module LifeBool where

import LifeStructures

-- This module deals with creating a binary life model

type HealthSnapshot = LifeSnapshot Health

type Health = Bool

mkSnapshot :: [[Health]] -> HealthSnapshot
mkSnapshot = fromNestedRaw

nextSnapshot :: HealthSnapshot -> HealthSnapshot
nextSnapshot = fmap' nextCell

nextCell :: LifeCell Health -> Health
nextCell cell = nextState (state cell) (countHealth $ catNeighboursRaw cell)

countHealth :: [Health] -> Integer
countHealth = foldr f 0
  where
    f True n = n + 1
    f False n = n

nextState :: Health -> Integer -> Health
nextState True neighbours
  | neighbours < 2 = False
  | neighbours > 3 = False
  | otherwise      = True

nextState False neighbours
  | neighbours == 3 = True
  | otherwise       = False

-- Module properties

-- A somewhat pointless property as it just sets, then gets.
prop_mkSnapshot :: [[Health]] -> Bool
prop_mkSnapshot health = destructed == health
  where
    constructed = mkSnapshot health
    destructed = toNestedRaw constructed
