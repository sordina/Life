module LifeRendering where

import Graphics.UI.GLUT
import Control.Monad (mapM_)

import Colors
import LifeMatrix

type Point = (Integer, Integer)

renderSnapshot :: LifeSnapshot -> IO ()
renderSnapshot snapshot = renderPrimitive Quads $ mapM_ renderPoint pointsHealth
  where
    pointsHealth = toListWithPos snapshot

renderPoint :: (Integer, Integer, Health) -> IO ()
renderPoint (x, y, Alive color) = point x y color
renderPoint _ = return () -- Do nothing for dead cells

point :: Integer -> Integer -> Color3 GLfloat -> IO ()
point x y inColor = do
  color inColor
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
