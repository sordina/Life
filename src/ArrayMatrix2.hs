{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module ArrayMatrix2 (ArrayMatrix, fromRows, toRows, toListWithPos, neighbours, neighbourMap)
where

import Matrix
import Data.Array
import Control.Monad (guard)

import qualified Control.Comonad as C
import qualified Control.Comonad.Store.Pointer as P

type ArrayMatrix = Array (Integer, Integer)

instance Matrix ArrayMatrix a
  where
    at arr x y = arr !(x,y)
    rows = (1+) . snd . snd . bounds
    columns = (1+) . fst . snd . bounds

    fromRows lst = array ((0,0), (cols-1, rows-1)) $ mkNumAssoc lst
      where rows = fromIntegral $ length lst
            cols = fromIntegral . length $ head lst

            mkNumAssoc lst = [((c, r), e) | (r , row) <- zip [0..] lst,
                                            (c,    e) <- zip [0..] row]

    neighbourMap f arr = P.array (P.Pointer (0,0) arr C.=>> upd)
      where
        maxx = fst . snd $ bounds arr
        maxy = snd . snd $ bounds arr

        upd ptr@(P.Pointer (x,y) arr) = f (P.extract ptr) neighbours
          where
            neighbours = map (arr!) [(x', y') | y' <- [max 0 (y-1) .. min maxy (y+1)],
                                                x' <- [max 0 (x-1) .. min maxx (x+1)],
                                                x' /= x || y' /= y ]
