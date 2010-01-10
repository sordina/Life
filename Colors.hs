module Colors (
		red,
		green,
		blue,
		yellow,
		violet,
		teal,
		white,
    black
	) where

import Graphics.Rendering.OpenGL

red    = c 1 0 0
green  = c 0 1 0
blue   = c 0 0 1
yellow = c 1 1 0
violet = c 0 1 1
teal   = c 1 0 1
white  = c 1 1 1
black  = c 0 0 0

c :: Int -> Int -> Int -> Color3 GLfloat
c r g b = Color3 (f r) (f g) (f b)
	where f = fromIntegral
