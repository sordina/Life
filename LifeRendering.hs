module LifeRendering where

import Graphics.UI.GLUT
import Control.Monad (mapM_)

import LifeBool
import Colors
import ListUtils
import LifeStructures (toNestedRaw)

type Point = (Integer, Integer)

renderSnapshot :: HealthSnapshot -> IO ()
renderSnapshot snapshot = renderInner points
  where
    nestedRaw = toNestedRaw snapshot
    width = (length $ head nestedRaw) - 1
    height = (length nestedRaw) - 1
    points = [(fromIntegral x, fromIntegral y) | x <- [0..width], y <- [0..height]]

    renderInner :: [Point] -> IO ()
    renderInner points = do
      clear [ColorBuffer]
      color white

      renderPrimitive Quads $ do
        mapM_ renderPoint points

      flush

renderPoint :: Point -> IO ()
renderPoint (x, y) = do
  point x y

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
