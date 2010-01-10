module LifeRendering where

import Graphics.UI.GLUT
import Control.Monad (mapM_)

import LifeBool
import Colors
import LifeStructures (toFlatWithPositions)

type Point = (Integer, Integer)

renderSnapshot :: HealthSnapshot -> IO ()
renderSnapshot snapshot = do
  color white
  renderPrimitive Quads $ mapM_ renderPoint pointsHealth
  where
    pointsHealth = toFlatWithPositions snapshot

renderPoint :: (Integer, Integer, Health) -> IO ()
renderPoint (x, y, True) = point x y
renderPoint _ = return ()

point :: Integer -> Integer -> IO ()
point x y = do
  v x  y  0
  v x  y' 0
  v x' y' 0
  v x' y  0
  where
    x' = x + 1
    y' = y + 1

v x y z = vertex $ Vertex3 (f x) (f y) (f z)
  where
    f = fromIntegral :: Integer -> GLdouble
