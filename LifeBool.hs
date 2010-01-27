module LifeBool where

import LifeStructures
import Test.QuickCheck ((==>), Property)

-- This module deals with creating a binary life model

type HealthSnapshot = LifeSnapshot Health

type Health = Bool

mkSnapshot :: [[Health]] -> HealthSnapshot
mkSnapshot = fromRows

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

--instance Arbitrary

-- Checks that a snapshot's successor is the same size
prop_samesize :: [Bool] -> Property
prop_samesize bools = length bools >= 10 ==> len1 == len2
  where
    snapshot = fromFlat 5 bools
    len = length . toRows
    len1 = len snapshot
    len2 = len $ nextSnapshot snapshot

-- A somewhat pointless property as it just sets, then gets.
prop_mkSnapshot :: [[Health]] -> Bool
prop_mkSnapshot health = destructed == health
  where
    constructed = mkSnapshot health
    destructed = toRowsRaw constructed
