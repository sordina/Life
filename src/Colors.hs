module Colors (
    Color,
    non_black,
		red,
		green,
		blue,
		yellow,
		violet,
		teal,
		white,
    black,

    (|-|)
	) where

import Graphics.Rendering.OpenGL hiding (Color)

red    = c 1 0 0
green  = c 0 1 0
blue   = c 0 0 1
yellow = c 1 1 0
violet = c 0 1 1
teal   = c 1 0 1
white  = c 1 1 1
black  = c 0 0 0

(|-|) :: Color -> Color -> Color
Color3 a b c |-| Color3 d e f = Color3 (a +- d) (b +- e) (c +- f)
  where
    a +- b = fromIntegral $ (a' + b') `mod` 2
        where
          a' = oz a :: Integer
          b' = oz b :: Integer
          oz n
            | n > 0.5   = 1
            | otherwise = 0

type Color = Color3 GLfloat

non_black = [
    red,
    green,
    blue,
    yellow,
    violet,
    teal,
    white
  ]

c :: Int -> Int -> Int -> Color
c r g b = Color3 (f r) (f g) (f b)
	where f = fromIntegral
